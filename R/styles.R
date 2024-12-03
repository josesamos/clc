# Función común para leer el estilo desde GeoPackage o PostGIS
read_style_from_source <- function(source, layer_name = NULL) {
  if (inherits(source, "PostgreSQLConnection")) {
    # Conexión a PostGIS
    query <- "SELECT * FROM layer_styles"
    style <- DBI::dbGetQuery(source, query)
  } else if (is.character(source) && file.exists(source)) {
    # GeoPackage
    style <- sf::st_read(source, layer = "layer_styles", quiet = TRUE)
  } else {
    stop("Invalid source: must be either a PostGIS connection or a GeoPackage file path.")
  }

  # Seleccionar el estilo basado en el nombre de la capa o el primero por defecto
  if (!is.null(layer_name)) {
    style <- style[style$f_table_name == layer_name, ]
    if (nrow(style) == 0) {
      stop("No style found for the specified layer name: ", layer_name)
    }
  } else {
    style <- style[1, ]  # Seleccionar el primer estilo si no se especifica un nombre de capa
  }

  return(style)
}


# Función común para filtrar las categorías según los valores del raster
filter_categories <- function(style, r_clc) {
  # Leer el archivo XML del estilo QML
  st_xml <- xml2::read_xml(style$styleQML[1])

  # Extraer las categorías
  categories <- xml2::xml_find_all(st_xml, "//category")
  id <- as.integer(xml2::xml_attr(categories, "value"))
  des <- xml2::xml_attr(categories, "label")

  # Extraer colores de los símbolos
  s <- xml2::xml_find_all(st_xml, ".//symbols/symbol")
  name <- xml2::xml_attr(s, "name")
  color <- xml2::xml_find_first(s, ".//prop[@k='color']") |> xml2::xml_attr("v")

  # Convertir color RGB a hexadecimal
  rgb2hex <- function(color) {
    rgb <- strsplit(color, ",")
    rgb <- rgb[[1]]
    rgb <- as.numeric(rgb)
    rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255)
  }
  color2 <- sapply(color, rgb2hex)
  names(color2) <- name
  color2 <- color2[order(as.numeric(names(color2)))]

  # Filtrar categorías según los valores presentes en el ráster
  values <- sort(terra::unique(r_clc)[, 1])
  if (!is.null(values)) {
    des <- des[id %in% values]
    color2 <- color2[id %in% values]
    id <- id[id %in% values]
  }

  # Crear el data frame de categorías
  categories <- data.frame(
    ID = id,
    Descripcion = des,
    Color = color2
  )

  return(categories)
}










#' Assign a Specified or Default Style to Layers in a GeoPackage Based on Layer Name
#'
#' This function copies the `layer_styles` from a source GeoPackage to a destination GeoPackage.
#' By default, it assigns the first style from the source to all layers in the destination,
#' but a specific style can be selected based on the layer name from the source GeoPackage.
#' If no style is specified, the first style in the `layer_styles` table is applied.
#'
#' @param from A string representing the path to the source GeoPackage file.
#' @param to A string representing the path to the destination GeoPackage file.
#' @param layers_to_copy An optional character vector specifying the names of layers
#'   in the destination GeoPackage to which the styles should be applied.
#'   If `NULL` (default), applies the style to all layers.
#' @param layer_name An optional string representing the name of the layer from the source
#'   `layer_styles` table whose style should be applied. If `NULL` (default), applies the
#'   first style.
#'
#' @return The updated `layer_styles` table, returned invisibly.
#'
#' @examples
#' \dontrun{
#' # Paths to GeoPackage files
#' source_gpkg <- "source.gpkg"
#' dest_gpkg <- "destination.gpkg"
#'
#' # Assign styles to all layers using the first style
#' assign_styles_to_layers(from = source_gpkg, to = dest_gpkg)
#'
#' # Assign styles to specific layers using the first style
#' assign_styles_to_layers(from = source_gpkg, to = dest_gpkg,
#'                         layers_to_copy = c("layer1", "layer2"))
#'
#' # Assign a specific style based on layer name to all layers
#' assign_styles_to_layers(from = source_gpkg, to = dest_gpkg,
#'                         layer_name = "layerX")
#' }
#'
#' @export
assign_styles_to_layers <- function(from, to, layers_to_copy = NULL, layer_name = NULL) {
  # Usar la función común para obtener el estilo
  style <- read_style_from_source(from, layer_name)

  # Leer las capas del GeoPackage destino
  all_layers <- sf::st_layers(to)$name

  # Determinar las capas a las que aplicar los estilos
  if (is.null(layers_to_copy)) {
    layers_to_copy <- all_layers
  } else {
    # Verificar que las capas especificadas existan en el destino
    missing_layers <- setdiff(layers_to_copy, all_layers)
    if (length(missing_layers) > 0) {
      stop("The following layers do not exist in the destination GeoPackage: ",
           paste(missing_layers, collapse = ", "))
    }
  }

  # Preparar la tabla de estilos
  n <- length(layers_to_copy)
  my_style <- do.call(rbind, replicate(n, style, simplify = FALSE))

  # Actualizar los nombres de tabla y SLD para cada capa
  for (i in seq_along(layers_to_copy)) {
    my_style$f_table_name[i] <- layers_to_copy[i]
    my_style$styleSLD[i] <- gsub(
      style$f_table_name,
      layers_to_copy[i],
      my_style$styleSLD[i],
      fixed = TRUE
    )
  }

  # Escribir la tabla de estilos en el destino
  sf::st_write(
    obj = my_style,
    dsn = to,
    layer = "layer_styles",
    append = FALSE,
    quiet = TRUE
  )

  invisible(my_style)  # Retornar la tabla de estilos actualizada
}


#' Assign Styles from a GeoPackage to a PostGIS Database (using an active connection)
#'
#' This function copies the `layer_styles` from a source GeoPackage to a destination
#' PostGIS database, assigning the first style from the source to all layers in the
#' destination. A specific style can be selected based on the layer name from the source
#' GeoPackage. If no style is specified, the first style in the `layer_styles` table is applied.
#'
#' @param from A string representing the path to the source GeoPackage file.
#' @param conn A database connection object to the destination PostGIS database (an active `DBI` connection).
#' @param layers_to_copy An optional character vector specifying the names of layers
#'   in the destination PostGIS database to which the styles should be applied.
#'   If `NULL` (default), applies the style to all layers.
#' @param layer_name An optional string representing the name of the layer from the source
#'   `layer_styles` whose style should be applied. If `NULL` (default), applies the
#'   first style.
#'
#' @return The updated `layer_styles` table in PostGIS, returned invisibly.
#'
#' @examples
#' \dontrun{
#' # Paths to GeoPackage file
#' source_gpkg <- "source.gpkg"
#'
#' # Connect to PostGIS database using an active connection
#' conn <- DBI::dbConnect(RPostgres::Postgres(),
#'                        dbname = "mydb", user = "myuser", host = "localhost", password = "mypassword")
#'
#' # Assign styles to all layers using the first style
#' assign_styles_to_layers_pg(from = source_gpkg, conn = conn)
#'
#' # Assign styles to specific layers using the first style
#' assign_styles_to_layers_pg(from = source_gpkg, conn = conn,
#'                          layers_to_copy = c("layer1", "layer2"))
#'
#' # Assign a specific style based on layer name to all layers
#' assign_styles_to_layers_pg(from = source_gpkg, conn = conn,
#'                          layer_name = "layerX")
#'
#' # Don't forget to disconnect the database connection after usage
#' DBI::dbDisconnect(conn)
#' }
#'
#' @export
assign_styles_to_layers_pg <- function(from, conn, layers_to_copy = NULL, layer_name = NULL) {
  # Usar la función común para obtener el estilo
  style <- read_style_from_source(from, layer_name)

  # Leer las capas de la base de datos PostGIS
  query <- "SELECT table_name FROM information_schema.tables WHERE table_schema='public';"
  layers_in_pg <- DBI::dbGetQuery(conn, query)$table_name

  # Determinar las capas a las que aplicar los estilos
  if (is.null(layers_to_copy)) {
    layers_to_copy <- layers_in_pg
  } else {
    # Verificar que las capas especificadas existan en el destino
    missing_layers <- setdiff(layers_to_copy, layers_in_pg)
    if (length(missing_layers) > 0) {
      stop("The following layers do not exist in the PostGIS database: ",
           paste(missing_layers, collapse = ", "))
    }
  }

  # Preparar la tabla de estilos
  n <- length(layers_to_copy)
  my_style <- do.call(rbind, replicate(n, style, simplify = FALSE))

  # Actualizar los nombres de tabla y SLD para cada capa
  for (i in seq_along(layers_to_copy)) {
    my_style$f_table_name[i] <- layers_to_copy[i]
    my_style$styleSLD[i] <- gsub(
      style$f_table_name,
      layers_to_copy[i],
      my_style$styleSLD[i],
      fixed = TRUE
    )
  }

  # Escribir la tabla de estilos en la base de datos PostGIS
  DBI::dbWriteTable(conn, "layer_styles", my_style, append = TRUE, row.names = FALSE)

  invisible(my_style)  # Retornar la tabla de estilos actualizada
}






