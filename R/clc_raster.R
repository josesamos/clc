#' `clc_raster` S3 Class
#'
#' Create an object of class `clc_raster`.
#'
#' This function creates an object of class `clc_raster` from a `terra` raster object
#' and a `clc_category` object.
#'
#' @param vector_layer A vector layer in `sf` format to be rasterized.
#' @param field A string, the field in the vector layer used to assign values in the raster.
#' @param category A `clc_category` object.
#' @param base_raster (Optional) A raster object to use as the base for rasterization.
#' @param resolution (Optional) Numeric resolution to define the raster grid if `base_raster` is not provided.
#' @return An object of class `clc_raster`.
#' @details The function requires either `base_raster` or `resolution` to be provided.
#' If both are missing, an error is raised.
#'
#' @keywords internal
#' @noRd
clc_raster <- function(vector_layer,
                       field,
                       category,
                       base_raster = NULL,
                       resolution = NULL) {

  vector_layer[[field]] <- as.integer(vector_layer[[field]])
  raster <- vector_to_raster_layers(vector_layer, field, base_raster, resolution)

  obj <- list(
    raster = raster,
    category = category
    )

  class(obj) <- "clc_raster"
  obj
}



#' @rdname get_levels
#' @export
get_levels.clc_raster <- function(clo) {
  clo$category |>
    get_levels()
}


#' @rdname get_colors
#' @export
get_colors.clc_raster <- function(clo) {
  clo$category |>
    get_colors()
}


#' @rdname plot_clc
#' @export
plot_clc.clc_raster <- function(clo, ...) {
  r_clc <- clo$raster
  levels(r_clc) <- clo$category |> get_levels()
  terra::plot(r_clc, col = clo$category |> get_colors(), ...)
}


#' Retrieve a Raster Representation of CLC
#'
#' Retrieve a raster representation (`terra::SpatRaster`) from a CLC object.
#'
#' @param clo A `clc_raster` object.
#'
#' @return A `terra::SpatRaster` object.
#'
#' @family CLC class functions
#'
#' @examples
#' source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
#' clo <- clc(source = source_gpkg, layer_name = "clc")
#'
#' r <- clo |>
#'      as_raster(resolution = 50)
#'
#' clc_r <- r |>
#'          get_raster()
#'
#' @export
get_raster <- function(clo)
  UseMethod("get_raster")


#' @rdname get_raster
#' @export
get_raster.clc_raster <- function(clo) {
  clo$raster
}



#' Convert a Vector Layer to Raster Format
#'
#' Converts a vector layer (in sf format) into a raster layer using a specified field
#' and either a base raster or a resolution.
#'
#' If a base raster is used, the vector layer is cropped to the bounding box of the
#' base raster, and the resulting raster has the same CRS as the base raster.
#'
#' @param vector_layer A vector layer in `sf` format to be rasterized.
#' @param field The field in the vector layer used to assign values in the raster.
#' @param base_raster (Optional) A raster object to use as the base for rasterization.
#' @param resolution (Optional) Numeric resolution to define the raster grid if `base_raster` is not provided.
#' @return A `terra` raster object representing the converted vector layer into raster format.
#' @details The function requires either `base_raster` or `resolution` to be provided.
#' If both are missing, an error is raised.
#' @keywords internal
#' @noRd
vector_to_raster_layers <- function(vector_layer,
                                    field,
                                    base_raster = NULL,
                                    resolution = NULL) {
  if (!is.null(base_raster)) {
    r_base <- base_raster
    bbox_raster <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(r_base)))
    vector_layer <- clip_multipoligon(vector_layer, bbox_raster)
    if (nrow(vector_layer) == 0) {
      stop("The vector layer does not overlap with the base raster.")
    }
  } else if (!is.null(resolution)) {
    vector_extent <- sf::st_bbox(vector_layer)
    r_base <- terra::rast(
      ext = vector_extent,
      res = resolution,
      crs = sf::st_crs(vector_layer)$wkt
    )
  } else {
    stop("Either 'base_raster' or 'resolution' must be provided.")
  }

  raster_result <- terra::rasterize(vector_layer, r_base, field = field)

  return(raster_result)
}


