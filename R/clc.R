.onLoad <- function(libname, pkgname) {
  utils::data(
    "clc_code",
    package = pkgname,
    envir = parent.env(environment())
  )
}

#' `clc` S3 class
#'
#' Create an object of class `clc`.
#'
#' This function creates an object of class `clc` from a vector layer in either
#' a GeoPackage or a PostGIS database.
#'
#' The layer must have a style defined in the source.
#'
#' @param source The source of the vector layer. Can be a file path to a GeoPackage or a PostGIS connection.
#' @param layer_name The name of the layer in the source to be used.
#' @return An object of class `clc`.
#' @examples
#' # ex1
#' source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
#' clo <- clc(source = source_gpkg, layer_name = "clc")
#'
#' \dontrun{
#' # ex2
#' conn <- RPostgres::dbConnect(
#'   RPostgres::Postgres(),
#'   dbname = 'exampledb',
#'   host = 'localhost',
#'   port = '5432',
#'   user = 'postgres',
#'   password = 'postgres'
#' )
#' clo <- clc(source = conn, layer_name = "clc")
#' }
#' @export
clc <- function(source, layer_name) {
  layer <- suppressWarnings(sf::st_read(source, layer = layer_name, quiet = TRUE))
  style <- read_style_from_source(source, layer_name)

  obj <- list(
    name = layer_name,
    layer = layer,
    style = style
  )

  class(obj) <- "clc"
  obj
}


#' Clip the Layer with a Polygon
#'
#' This function clips the object layer using a polygon layer. It handles CRS
#' transformations automatically if necessary, ensuring the output is in the same
#' CRS as the input polygon.
#'
#' @param clo A `clc` object.
#' @param polygon An `sf` object representing the polygon layer used for clipping.
#'
#' @return A `clc` object.
#'
#' @examples
#'
#' source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
#' clo <- clc(source = source_gpkg, layer_name = "clc")
#'
#' polygon <- sf::st_read(source_gpkg, layer = 'lanjaron', quiet = TRUE)
#'
#' clo2 <- clo |>
#'         cut_to_extent(polygon)
#'
#' @export
cut_to_extent <- function(clo, polygon)
  UseMethod("cut_to_extent")


#' @rdname cut_to_extent
#' @export
cut_to_extent.clc <- function(clo, polygon) {

  layer <- clip_multipoligon(clo$layer, polygon)

  obj <- list(
    name = clo$name,
    layer = layer,
    style = clo$style
  )

  class(obj) <- "clc"
  obj
}


#' Convert a `clc` Object to Raster Format
#'
#' Returns an object of class `clc_raster` that contains a `terra` raster object
#' representing the converted vector layer into raster format.
#'
#' @param clo A `clc` object.
#' @param field The field in the vector layer used to assign values in the raster.
#' @param base_raster (Optional) A raster object to use as the base for rasterization.
#' @param resolution (Optional) Numeric resolution to define the raster grid if `base_raster` is not provided.
#' @return An object of class `clc_raster`.
#' @details The function requires either `base_raster` or `resolution` to be provided.
#' If both are missing, an error is raised.
#' @examples
#'
#' source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
#' clo <- clc(source = source_gpkg, layer_name = "clc")
#'
#' raster_path <- system.file("extdata", "mdt.tif", package = "clc")
#' base_raster <- terra::rast(raster_path)
#'
#' # ex1
#' r <- clo |>
#'      as_raster( field = "CODE_18", base_raster = base_raster)
#'
#' # ex2
#' r <- clo |>
#'      as_raster( field = "CODE_18", resolution = 50)
#'
#' @export
as_raster <- function(clo, field, base_raster, resolution)
  UseMethod("as_raster")

#' @rdname as_raster
#' @export
as_raster.clc <- function(clo,
                          field,
                          base_raster = NULL,
                          resolution = NULL) {

  values <- sort(unique(clo$layer[[field]]))
  category <- clc_category(clo$style, values)

  clc_raster(clo$layer, field, category, base_raster, resolution)
}


#' Save a CLC layer and its style to a GeoPackage or PostGIS database
#'
#' This function saves a CLC layer and its style to a GeoPackage file or a PostGIS database.
#' The destination is determined by the `to` argument.
#'
#' @param clo A `clc` object.
#' @param to A data destination for the output. This can be:
#'   - A string representing the path to a GeoPackage file.
#'   - A `DBI` database connection object to a PostGIS database, created using [RPostgres::dbConnect()].
#' @param database A string, database name, only in case the destination is in PostGIS.
#' @param schema A string, schema name, only in case the destination is in PostGIS.
#'   Defaults to `'public'`.
#' @param layer_name A character string specifying the name of the layer in the output.
#'   If `NULL`, the name of the input `layer` is used.
#'
#' @return clo A `clc` object.
#'
#' @details The function overwrites the table if it already exists.
#'
#' @examples
#'
#' esa <- system.file("extdata", "esa", package = "clc")
#' clo <- clc(dir = esa)
#'
#' r <- clo |>
#'      save_to()
#'
#' @export
save_to <- function(clo, to, database, schema, layer_name)
  UseMethod("save_to")


#' @rdname save_to
#' @export
save_to.clc <- function(clo,
                     to,
                     database = NULL,
                     schema = 'public',
                     layer_name = NULL) {
  if (is.null(layer_name)) {
    layer_name <- clo$name
  }

  suppressMessages(sf::st_write(
    obj = clo$layer,
    dsn = to,
    layer = layer_name,
    delete_layer = TRUE
  ))

  assign_styles_to_layers(clo$style, to, database, schema, layers = layer_name)
  clo
}

# Plot vectorial


# library(sf)
# library(ggplot2)
# library(xml2)
#
# # Ruta al GeoPackage
# gpkg_path <- "ruta_a_tu_geopackage.gpkg"
#
# # Leer la capa
# layer <- st_read(gpkg_path, layer = "nombre_de_la_capa")
#
# # Leer el estilo desde la tabla 'layer_styles'
# styles <- st_read(gpkg_path, layer = "layer_styles")
#
# # Extraer el XML del estilo
# style_xml <- read_xml(styles$styleQML[1])
#
# # Extraer categorías (asumiendo un campo <category>)
# categories <- xml_find_all(style_xml, "//category")
# values <- xml_attr(categories, "value")
# labels <- xml_attr(categories, "label")
# colors <- xml_attr(categories, "symbol")
#
# # Convertir colores a un formato interpretable
# rgb2hex <- function(rgb) {
#   rgb_vals <- as.numeric(unlist(strsplit(rgb, ",")))
#   rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], maxColorValue = 255)
# }
# colors_hex <- sapply(colors, rgb2hex)
#
# # Asociar colores a la capa según un atributo
# layer$color <- colors_hex[match(layer$tu_campo, values)]
#
# # Visualizar con ggplot2
# ggplot(data = layer) +
#   geom_sf(aes(fill = color), color = NA) +
#   scale_fill_identity() +
#   theme_minimal()


