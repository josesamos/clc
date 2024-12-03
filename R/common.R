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


