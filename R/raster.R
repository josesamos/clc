#' Rasterize a vector layer
#'
#' Converts a vector layer (in sf format) into a raster layer using a specified field
#' and either a base raster or a resolution.
#'
#' @param vector_layer A vector layer in `sf` format to be rasterized.
#' @param field The field in the vector layer used to assign values in the raster.
#' @param base_raster (Optional) A raster object to use as the base for rasterization.
#' @param resolution (Optional) Numeric resolution to define the raster grid if `base_raster` is not provided.
#' @return A raster object created from the vector layer.
#' @details The function requires either `base_raster` or `resolution` to be provided.
#' If both are missing, an error is raised.
#' @keywords internal
#' @noRd
process_vector_to_raster <- function(vector_layer,
                                     field,
                                     base_raster = NULL,
                                     resolution = NULL) {
  if (!is.null(base_raster)) {
    raster_base <- base_raster
  } else if (!is.null(resolution)) {
    vector_extent <- sf::st_bbox(vector_layer)
    raster_base <- terra::rast(
      ext = vector_extent,
      res = resolution,
      crs = sf::st_crs(vector_layer)$wkt
    )
  } else {
    stop("Either 'base_raster' or 'resolution' must be provided.")
  }

  raster_result <- terra::rasterize(vector_layer, raster_base, field = field)

  return(raster_result)
}


#' Convert a CORINE Land Cover Vector Layer to Raster Format
#'
#' This function converts a vector layer representing **CORINE Land Cover (CLC)**
#' data, stored in a GeoPackage or in PostGIS, into a raster format using either an
#' existing raster as a base or, if no base raster is provided, generating a new
#' raster based on the extent and resolution of the vector layer.
#'
#' @param from A data source for the input styles. This can be:
#'   - A string representing the path to a GeoPackage file.
#'   - A `DBI` database connection object to a PostGIS database, created using [RPostgres::dbConnect()].
#' @param layer_name A string representing the name of the vector layer in the GeoPackage to be converted to raster.
#' @param field A string representing the name of the layer field from which the values for the raster layer will be taken.
#' @param base_raster A `terra` raster object to use as the base for the output raster. If `NULL`, the function will
#'   generate a new raster using the dimensions of the vector layer and the provided resolution.
#' @param resolution A numeric value specifying the resolution of the raster (in meters). This is used to generate
#'   a new raster if no `base_raster` is provided. The function will automatically calculate the raster's extent from
#'   the vector layer.
#'
#' @return A `terra` raster object representing the converted vector layer into raster format.
#'
#' @examples
#' \dontrun{
#' source_gpkg <- "source.gpkg"
#'
#' # Convert vector layer to raster using a base raster
#' base_raster <- terra::rast("base_raster.tif")
#' raster_result <- vector_to_raster(
#'   from = source_gpkg,
#'   layer_name = "CLC_layer",
#'   field = "CLC",
#'   base_raster = base_raster
#' )
#'
#' # Convert vector layer to raster with a specified resolution
#' raster_result_res <- vector_to_raster(
#'   from = source_gpkg,
#'   layer_name = "CLC_layer",
#'   field = "CLC",
#'   resolution = 100
#' )
#'
#' conn <- RPostgres::dbConnect(
#'   RPostgres::Postgres(),
#'   dbname = 'exampledb',
#'   host = 'localhost',
#'   port = '5432',
#'   user = 'postgres',
#'   password = 'postgres'
#' )
#'
#' # Convert vector layer to raster using a base raster
#' base_raster <- terra::rast("base_raster.tif")
#' raster_result <- vector_to_raster(
#'   from = conn,
#'   layer_name = "CLC_layer",
#'   field = "CLC",
#'   base_raster = base_raster
#' )
#'
#' # Convert vector layer to raster with a specified resolution
#' raster_result_res <- vector_to_raster(
#'   from = conn,
#'   layer_name = "CLC_layer",
#'   field = "CLC",
#'   resolution = 100
#' )
#' }
#'
#' @export
vector_to_raster <- function(from,
                             layer_name,
                             field,
                             base_raster = NULL,
                             resolution = NULL) {
  vector_layer <- sf::st_read(from, layer = layer_name, quiet = TRUE)

  raster_result <- process_vector_to_raster(vector_layer, field, base_raster, resolution)

  return(raster_result)
}
