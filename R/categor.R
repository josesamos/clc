#' Extract categories and colors from a style
#'
#' Parses a QGIS QML style file to extract categories and their associated colors.
#'
#' @param style A data frame containing a QGIS QML style in the column `styleQML`.
#' The first entry is used for extraction.
#' @return A data frame with the following columns:
#' - `id`: Integer category IDs.
#' - `description`: Labels associated with each category.
#' - `color`: Hexadecimal representation of the colors for each category.
#' @keywords internal
#' @noRd
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

  return(data.frame(
    id = id,
    description = des,
    color = color2
  ))
}


#' Extract Categories from a Style Layer Based on Layer Name
#'
#' This function extracts the categories (labels and associated colors) from the
#' `layer_styles` layer of a GeoPackage or PostGIS database. The style information
#' is used to create a mapping between category IDs, descriptions, and colors.
#'
#' The function will use the style for the specified layer name, or if no name is
#' provided, it defaults to using the first style in the `layer_styles` table.
#'
#' The result can be filtered by considering only the specified values, provided
#' a set other than NULL is assigned to the `values` variable.
#'
#' @param from A data source for the input styles. This can be:
#'   - A string representing the path to a GeoPackage file.
#'   - A `DBI` database connection object to a PostGIS database, created using [RPostgres::dbConnect()].
#' @param layer_name An optional string representing the name of the layer from the source
#'   `layer_styles` whose style should be applied. If `NULL` (default), applies the
#'   first style.
#' @param values The set of values used to filter the results; filtering is applied only if this set
#'   is not NULL.
#'
#' @return A data frame containing the category IDs, descriptions, and associated
#' colors for the values present in the raster.
#' The data frame has three columns: `id`, `description`, and `color` (HEX color codes).
#'
#' @examples
#'
#' gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")
#'
#' categories <- extract_categories_from_style(from = gpkg_path)
#'
#' categories <- extract_categories_from_style(from = gpkg_path, layer_name = "clc")
#'
#' categories <- extract_categories_from_style(from = gpkg_path,
#'   values = c(111, 211, 311))
#'
#' categories <- extract_categories_from_style(from = gpkg_path, layer_name = "clc",
#'   values = c(111, 211, 311))
#'
#' \dontrun{
#' conn <- RPostgres::dbConnect(
#'   RPostgres::Postgres(),
#'   dbname = 'exampledb',
#'   host = 'localhost',
#'   port = '5432',
#'   user = 'postgres',
#'   password = 'postgres'
#' )
#'
#' categories <- extract_categories_from_style(from = conn)
#'
#' categories <- extract_categories_from_style(from = conn, layer_name = "clc")
#'
#' categories <- extract_categories_from_style(from = conn,
#'   values = c(111, 211, 311))
#'
#' categories <- extract_categories_from_style(from = conn, layer_name = "clc",
#'   values = c(111, 211, 311))
#' }
#'
#' @export
extract_categories_from_style <- function(from, layer_name = NULL, values = NULL) {
  style <- read_style_from_source(from, layer_name)

  cat <- extract_categories_and_colors(style)

  if (!is.null(values)) {
    cat <- cat[cat$id %in% values, ]
  }
  return(cat)
}
