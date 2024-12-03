
# Function to extract categories and colors from a style
extract_categories_and_colors <- function(style) {
  st_xml <- xml2::read_xml(style$styleQML[1])

  categories <- xml2::xml_find_all(st_xml, "//category")
  id <- as.integer(xml2::xml_attr(categories, "value"))
  des <- xml2::xml_attr(categories, "label")

  s <- xml2::xml_find_all(st_xml, ".//symbols/symbol")
  name <- xml2::xml_attr(s, "name")
  color <- xml2::xml_find_first(s, ".//prop[@k='color']") |> xml2::xml_attr("v")

  rgb2hex <- function(color) {
    rgb <- strsplit(color, ",")
    rgb <- rgb[[1]]
    rgb <- as.numeric(rgb)
    rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255)
  }
  color2 <- sapply(color, rgb2hex)
  names(color2) <- name
  color2 <- color2[order(as.numeric(names(color2)))]

  return(data.frame(id = id, description = des, color = color2))
}



#' Extract Categories from a Style Layer in a GeoPackage Based on Layer Name
#'
#' This function extracts the categories (labels and associated colors) from the
#' `layer_styles` layer of a GeoPackage. The style information is used to create
#' a mapping between category IDs, descriptions, and colors for a given raster layer.
#'
#' The function will use the style for the specified layer name, or if no name is
#' provided, it defaults to using the first style in the `layer_styles` table.
#'
#' @param from A string representing the path to the source GeoPackage file.
#' @param r_clc A `terra` raster object containing the land cover data.
#' @param layer_name An optional string representing the name of the layer from the source
#'   `layer_styles` whose style should be applied. If `NULL` (default), applies the
#'   first style.
#'
#' @return A data frame (`categories`) containing the category IDs, descriptions,
#' and associated colors for the values present in the raster.
#' The data frame has three columns: `id`, `description`, and `color` (HEX color codes).
#'
#' @family transformation functions
#'
#' @examples
#' \dontrun{
#' # Path to GeoPackage file
#' source_gpkg <- "source.gpkg"
#'
#' # Load the raster layer (e.g., CORINE Land Cover)
#' library(terra)
#' r_clc <- rast("clc_raster.tif")
#'
#' # Extract categories from the GeoPackage style layer and the raster
#' categories <- extract_categories_from_style(from = source_gpkg, r_clc = r_clc)
#'
#' # Extract categories for a specific layer
#' categories_layerX <- extract_categories_from_style(from = source_gpkg, r_clc = r_clc, layer_name = "layerX")
#' }
#'
#' @export
extract_categories_from_style <- function(from, r_clc, layer_name = NULL) {
  style <- sf::st_read(from, layer = "layer_styles", quiet = TRUE)

  if (!is.null(layer_name)) {
    style <- style[style$f_table_name == layer_name, ]
    if (nrow(style) == 0) {
      stop("No style found for the specified layer name: ", layer_name)
    }
  } else {
    style <- style[1, ]
  }

  cat <- extract_categories_and_colors(style)

  values <- sort(terra::unique(r_clc)[, 1])

  data.frame(
    id = cat$id[cat$id %in% values],
    description = cat$description[cat$id %in% values],
    color = cat$color[cat$id %in% values]
  )

  return(categories)
}


#' Extract Categories from a PostGIS Database Style Layer
#'
#' This function extracts the categories (labels and associated colors) from the `layer_styles`
#' table of a PostGIS database. The style information is used to create a mapping between
#' category IDs, descriptions, and colors for a given raster layer.
#'
#' The function will use the style for the specified layer name, or if no name is
#' provided, it defaults to using the first style in the `layer_styles` table.
#'
#' @param conn A database connection object to the destination PostGIS database (an active `DBI` connection).
#' @param r_clc A `terra` raster object containing the land cover data.
#' @param layer_name An optional string representing the name of the layer from the source
#'   `layer_styles` whose style should be applied. If `NULL` (default), applies the
#'   first style.
#'
#' @return A data frame (`categories`) containing the category IDs, descriptions,
#' and associated colors for the values present in the raster.
#' The data frame has three columns: `ID`, `description` (Description), and `Color` (HEX color codes).
#'
#' @family transformation functions
#'
#' @examples
#' \dontrun{
#' # Connect to PostGIS database
#' conn <- DBI::dbConnect(RPostgres::Postgres(),
#'                        dbname = "mydb", user = "myuser", host = "localhost", password = "mypassword")
#'
#' # Load the raster layer (e.g., CORINE Land Cover)
#' library(terra)
#' r_clc <- rast("clc_raster.tif")
#'
#' # Extract categories for a specific layer from PostGIS
#' categories_pg <- extract_categories_from_style_pg(conn = conn, r_clc = r_clc)
#' DBI::dbDisconnect(conn)
#' }
#'
#' @export
extract_categories_from_style_pg <- function(conn, r_clc, layer_name = NULL) {
  # Leer la tabla de estilos desde la base de datos PostGIS
  query <- "SELECT * FROM layer_styles"
  style <- DBI::dbGetQuery(conn, query)

  # Seleccionar el estilo basado en el nombre de la capa o el primero por defecto
  if (!is.null(layer_name)) {
    style <- style[style$f_table_name == layer_name, ]
    if (nrow(style) == 0) {
      stop("No style found for the specified layer name: ", layer_name)
    }
  } else {
    style <- style[1, ]  # Seleccionar el primer estilo si no se especifica un nombre de capa
  }

  # Usar la función común para extraer categorías y colores
  cat <- extract_categories_and_colors(style)

  # Filtrar categorías según los valores presentes en el ráster
  values <- sort(terra::unique(r_clc)[, 1])
  if (!is.null(values)) {
    cat$description <- cat$description[cat$id %in% values]
    cat$color <- cat$color[cat$id %in% values]
    cat$id <- cat$id[cat$id %in% values]
  }

  # Crear el data frame de categorías
  categories <- data.frame(
    ID = cat$id,
    description = cat$description,
    Color = cat$color
  )

  return(categories)
}
