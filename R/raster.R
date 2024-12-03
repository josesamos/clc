# Función local para procesar la rasterización a partir de una capa vectorial en formato sf
process_vector_to_raster <- function(vector_layer, base_raster = NULL, resolution = NULL) {
  # Verificar si se proporcionó un ráster base o una resolución
  if (!is.null(base_raster)) {
    # Usar el ráster base proporcionado
    raster_base <- base_raster
  } else if (!is.null(resolution)) {
    # Generar un ráster base a partir de la extensión de la capa vectorial
    vector_extent <- sf::st_bbox(vector_layer)
    raster_base <- terra::rast(ext = vector_extent, res = resolution, crs = sf::st_crs(vector_layer)$wkt)
  } else {
    stop("Either 'base_raster' or 'resolution' must be provided.")
  }

  # Rasterizar la capa vectorial
  raster_result <- terra::rasterize(vector_layer, raster_base, field = "ID")

  return(raster_result)
}


#' Convert a CORINE Land Cover Vector Layer to Raster Format
#'
#' This function converts a vector layer representing **CORINE Land Cover (CLC)**
#' data, stored in a **GeoPackage**, into a **raster** format using either an
#' existing raster as a base or, if no base raster is provided, generating a new
#' raster based on the extent and resolution of the vector layer.
#'
#' @param from A string representing the path to the source **GeoPackage** file containing the vector layer.
#' @param layer_name A string representing the name of the vector layer in the **GeoPackage** to be converted to raster.
#' @param base_raster A **terra** raster object to use as the base for the output raster. If `NULL`, the function will
#'   generate a new raster using the dimensions of the vector layer and the provided resolution.
#' @param resolution A numeric value specifying the resolution of the raster (in meters). This is used to generate
#'   a new raster if no `base_raster` is provided. The function will automatically calculate the raster's extent from
#'   the vector layer.
#'
#' @return A **terra** raster object representing the converted vector layer into raster format.
#'
#' @examples
#' \dontrun{
#' # Path to the source GeoPackage
#' source_gpkg <- "source.gpkg"
#'
#' # Convert vector layer to raster using a base raster
#' base_raster <- terra::rast("base_raster.tif")
#' raster_result <- vector_to_raster(from = source_gpkg, layer_name = "CLC_layer", base_raster = base_raster)
#'
#' # Convert vector layer to raster with a specified resolution
#' raster_result_res <- vector_to_raster(from = source_gpkg, layer_name = "CLC_layer", resolution = 100)
#' }
#'
#' @export
vector_to_raster <- function(from, layer_name, base_raster = NULL, resolution = NULL) {
  # Leer la capa vectorial desde el GeoPackage
  if (!is.character(from) || !file.exists(from)) {
    stop("'from' must be a valid GeoPackage file path.")
  }
  vector_layer <- sf::st_read(from, layer = layer_name, quiet = TRUE)

  # Usar la función local para procesar la rasterización
  raster_result <- process_vector_to_raster(vector_layer, base_raster, resolution)

  return(raster_result)
}


#' Convert a PostGIS Vector Layer to Raster Format
#'
#' This function converts a vector layer stored in a PostGIS database into a raster format.
#' It allows the use of an existing raster as a base for the output or, if no base raster is provided,
#' generates a new raster using the specified resolution and the extent of the vector layer.
#'
#' @param conn A database connection object to the source PostGIS database (an active `DBI` connection).
#' @param layer_name A string representing the name of the vector layer in the PostGIS database to be converted to raster.
#' @param schema An optional string specifying the schema in the PostGIS database where the vector layer is located.
#'   Default is `"public"`.
#' @param base_raster An optional `terra` raster object to use as the base for the output raster.
#'   If `NULL`, the function generates a new raster based on the extent of the vector layer and the provided resolution.
#' @param resolution An optional numeric value specifying the resolution of the raster (in meters).
#'   This parameter is used to create a new raster if `base_raster` is not provided.
#'
#' @return A `terra` raster object representing the converted vector layer in raster format.
#'
#' @details The function queries the specified layer from the PostGIS database using the provided `conn`, `layer_name`,
#'   and `schema` parameters. If a `base_raster` is provided, the output raster will have the same resolution, extent,
#'   and coordinate reference system as the base raster. If `base_raster` is not provided, the function generates a new
#'   raster using the specified `resolution` and the extent of the vector layer.
#'
#' @examples
#' \dontrun{
#' # Connect to a PostGIS database
#' conn <- DBI::dbConnect(
#'   RPostgres::Postgres(),
#'   dbname = "mydb",
#'   user = "myuser",
#'   password = "mypassword",
#'   host = "localhost"
#' )
#'
#' # Convert a PostGIS vector layer to raster with a specified resolution
#' raster_result <- vector_to_raster_pg(
#'   conn = conn,
#'   layer_name = "land_cover",
#'   schema = "public",
#'   resolution = 100
#' )
#'
#' # Convert a PostGIS vector layer using an existing raster as base
#' base_raster <- terra::rast("base_raster.tif")
#' raster_result_base <- vector_to_raster_pg(
#'   conn = conn,
#'   layer_name = "land_cover",
#'   schema = "public",
#'   base_raster = base_raster
#' )
#'
#' # Disconnect from the database
#' DBI::dbDisconnect(conn)
#' }
#'
#' @export
vector_to_raster_pg <- function(conn, layer_name, schema = "public", base_raster = NULL, resolution = NULL) {
  # Verificar que la conexión sea válida
  if (!inherits(conn, "PostgreSQLConnection")) {
    stop("'conn' must be an active PostGIS database connection.")
  }

  # Construir la consulta SQL para recuperar la capa vectorial
  query <- paste0("SELECT * FROM ", schema, ".", layer_name)
  vector_layer <- DBI::dbGetQuery(conn, query)
  vector_layer <- sf::st_as_sf(vector_layer)

  # Usar la función local para procesar la rasterización
  raster_result <- process_vector_to_raster(vector_layer, base_raster, resolution)

  return(raster_result)
}
