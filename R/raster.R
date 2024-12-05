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
#' @return A raster object created from the vector layer.
#' @details The function requires either `base_raster` or `resolution` to be provided.
#' If both are missing, an error is raised.
#' @examples
#' gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")
#' vector_layer <- sf::st_read(gpkg_path, layer = 'clc', quiet = TRUE)
#'
#' raster_path <- system.file("extdata", "mdt.tif", package = "clc")
#' base_raster <- terra::rast(raster_path)
#'
#' # Ex1
#' raster_result <- vector_to_raster_layers(
#'   vector_layer = vector_layer,
#'   field = "CODE_18",
#'   base_raster = base_raster
#' )
#'
#' # Ex2
#' raster_result <- vector_to_raster_layers(
#'   vector_layer = vector_layer,
#'   field = "CODE_18",
#'   resolution = 50
#' )
#'
vector_to_raster_layers <- function(vector_layer,
                                    field,
                                    base_raster = NULL,
                                    resolution = NULL) {
  if (!is.null(base_raster)) {
    r_base <- base_raster
    bbox_raster <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(r_base)))
    vector_layer <- safe_clip_multipolygon(vector_layer, bbox_raster)
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


#' Convert a Stored Vector Layer to Raster Format
#'
#' This function converts a vector layer, stored in a GeoPackage or in PostGIS,
#' into a raster format using either an existing raster as a base or, if no base
#' raster is provided, generating a new raster based on the extent and resolution
#' of the vector layer.
#'
#' If a base raster is used, the vector layer is cropped to the bounding box of the
#' base raster, and the resulting raster has the same CRS as the base raster.
#'
#' @param from A data source for the input styles. This can be:
#'   - A string representing the path to a GeoPackage file.
#'   - A `DBI` database connection object to a PostGIS database, created using [RPostgres::dbConnect()].
#' @param layer_name A string representing the name of the vector layer in the GeoPackage to be converted to raster.
#' @param field A string representing the name of the layer field from which the values for the raster layer will be taken.
#' @param raster_path A string, the file path of the raster object to use as the base for the output raster. If `NULL`,
#'   the function will generate a new raster using the dimensions of the vector layer and the provided resolution.
#' @param resolution A numeric value specifying the resolution of the raster (in meters). This is used to generate
#'   a new raster if no `base_raster` is provided. The function will automatically calculate the raster's extent from
#'   the vector layer.
#'
#' @return A `terra` raster object representing the converted vector layer into raster format.
#' @details The function requires either `base_raster` or `resolution` to be provided.
#' If both are missing, an error is raised.
#'
#' @examples
#' gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")
#' raster_path <- system.file("extdata", "mdt.tif", package = "clc")
#'
#' # Ex1
#' raster_result <- vector_to_raster(
#'   from = gpkg_path,
#'   layer_name = "clc",
#'   field = "CODE_18",
#'   raster_path = raster_path
#' )
#'
#' # Ex2
#' raster_result_res <- vector_to_raster(
#'   from = gpkg_path,
#'   layer_name = "clc",
#'   field = "CODE_18",
#'   resolution = 50
#' )
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
#' # Ex3
#' raster_result <- vector_to_raster(
#'   from = conn,
#'   layer_name = "clc",
#'   field = "CODE_18",
#'   raster_path = raster_path
#' )
#'
#' # Ex4
#' raster_result_res <- vector_to_raster(
#'   from = conn,
#'   layer_name = "clc",
#'   field = "CODE_18",
#'   resolution = 100
#' )
#' }
#'
#' @export
vector_to_raster <- function(from,
                             layer_name,
                             field,
                             raster_path = NULL,
                             resolution = NULL) {
  vector_layer <- sf::st_read(from, layer = layer_name, quiet = TRUE)

  if (!is.null(raster_path)) {
    base_raster <- terra::rast(raster_path)
  } else {
    base_raster <- NULL
  }

  raster_result <- vector_to_raster_layers(vector_layer, field, base_raster, resolution)

  return(raster_result)
}
