#' Clip a Vector Layer with a Polygon
#'
#' This function clips a vector layer (e.g., points, lines, polygons) using a polygon layer.
#' It handles CRS (Coordinate Reference System) transformations automatically if necessary,
#' ensuring the output is in the same CRS as the input polygon.
#'
#' @param vector An `sf` object representing the vector layer to be clipped.
#' @param polygon An `sf` object representing the polygon layer used for clipping.
#'
#' @return An `sf` object containing the features of the input `vector` that intersect with the `polygon`.
#' The output will be in the CRS of the `polygon`, and it will retain all attributes of the input `vector`.
#'
#' @examples
#'
#' # Example data
#' vector <- sf::st_as_sf(data.frame(
#'   id = 1:3,
#'   geometry = sf::st_sfc(
#'     st_point(c(0.5, 0.5)),
#'     st_point(c(1.5, 1.5)),
#'     st_point(c(2.5, 2.5))
#'   )
#' ), crs = 4326)
#'
#' polygon <- sf::st_as_sf(data.frame(
#'   id = 1,
#'   geometry = sf::st_sfc(st_polygon(list(rbind(
#'     c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)
#'   ))))
#' ), crs = 4326)
#'
#' # Clip the vector using the polygon
#' clipped_vector <- clip_vector(vector, polygon)
#'
#' @export
clip_vector <- function(vector, polygon) {
  if (!sf::st_crs(vector) == sf::st_crs(polygon)) {
    polygon <- sf::st_transform(polygon, sf::st_crs(vector))
  }
  res <- sf::st_intersection(vector, polygon)
  res <- res[names(vector)]
  if (!sf::st_crs(vector) == sf::st_crs(polygon)) {
    res <- sf::st_transform(res, sf::st_crs(polygon))
  }
  res
}


#' Safely Clip a Multipolygon Vector Layer
#'
#' This function clips a `MULTIPOLYGON` vector layer using a polygon layer, handling specific
#' issues that might arise with geometries encoded incorrectly or containing unknown WKB types.
#' It serves as a fallback when the `clip_vector()` function fails due to errors like
#' `ParseException: Unknown WKB type 12`, which is associated with *MULTIPOLYGON* types.
#'
#' The function ensures that the input layer is correctly encoded as `MULTIPOLYGON` and
#' uses GDAL utilities for re-encoding if necessary. The output is projected to the CRS
#' of the clipping polygon.
#'
#' This solution is inspired by a discussion on handling WKB type errors in R:
#' <https://gis.stackexchange.com/questions/389814/r-st-centroid-geos-error-unknown-wkb-type-12>.
#'
#' @param vector A `sf` multipolygon vector layer to be clipped.
#' @param polygon A `sf` polygon layer used as the clipping geometry.
#'
#' @return A `sf` vector layer with the clipped geometries.
#'
#' @family transformation functions
#'
#' @examples
#'
#' # Example data
#' vector <- sf::st_as_sf(data.frame(
#'   id = 1:2,
#'   geometry = sf::st_sfc(
#'     st_polygon(list(rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)))),
#'     st_polygon(list(rbind(c(1, 1), c(3, 1), c(3, 3), c(1, 3), c(1, 1))))
#'   )
#' ), crs = 4326)
#'
#' polygon <- sf::st_as_sf(data.frame(
#'   id = 1,
#'   geometry = sf::st_sfc(st_polygon(list(rbind(
#'     c(1, 1), c(2, 1), c(2, 2), c(1, 2), c(1, 1)
#'   ))))
#' ), crs = 4326)
#'
#' # Clip the vector using the polygon
#' result <- safe_clip_multipolygon(vector, polygon)
#' print(result)
#'
#' @export
safe_clip_multipolygon <- function(vector, polygon) {
  tryCatch({
    v <- sf::st_cast(vector, "MULTIPOLYGON")
    clip_vector(v, polygon)
  }, error = function(e) {
    f <- tempfile(fileext = ".gpkg")
    sf::st_write(vector, f, quiet = TRUE)
    g <- tempfile(fileext = ".gpkg")
    gdalUtilities::ogr2ogr(f, g, f = "GPKG", nlt = "MULTIPOLYGON")
    v <- sf::st_read(g, quiet = TRUE)
    clip_vector(v, polygon)
  })
}

