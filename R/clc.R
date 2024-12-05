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
#' clo <- clc(source_gpkg, "clc")
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
#' clo <- clc(conn, "clc")
#' }
#' @export
clc <- function(source, layer_name) {
  layer <- suppressWarnings(sf::st_read(source, layer = layer_name, quiet = TRUE))
  style <- suppressWarnings(sf::st_read(source, layer = "layer_styles", quiet = TRUE))

  obj <- list(
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
#' esa <- system.file("extdata", "esa", package = "clc")
#' clo <- clc(dir = esa)
#'
#' r <- clo |>
#'      cut_to_extent()
#'
#' @export
cut_to_extent <- function(clo, polygon)
  UseMethod("cut_to_extent")


#' @rdname cut_to_extent
#' @export
cut_to_extent.clc <- function(clo, polygon) {

  layer <- safe_clip_multipolygon(clo$layer, polygon)

  obj <- list(
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
#' clo <- clc(source_gpkg, "clc")
#'
#' r <- clo |>
#'      as_raster()
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


