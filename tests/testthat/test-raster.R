test_that("vector_to_raster_layers works correctly with base raster", {
  gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")
  raster_path <- system.file("extdata", "mdt.tif", package = "clc")

  vector_layer <- sf::st_read(gpkg_path, layer = "clc", quiet = TRUE)
  base_raster <- terra::rast(raster_path)
  result <- vector_to_raster_layers(vector_layer, field = "CODE_18", base_raster)

  expect_s4_class(result, "SpatRaster")

  # Verify that the CRS matches the base raster
  base_raster <- terra::rast(raster_path)
  expect_equal(terra::crs(result), terra::crs(base_raster))

  # Verify that the extent matches the base raster
  expect_equal(terra::ext(result) == terra::ext(base_raster), TRUE)
})

test_that("vector_to_raster_layers works correctly with resolution", {
  gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")

  resolution <- 50

  vector_layer <- sf::st_read(gpkg_path, layer = "clc", quiet = TRUE)
  result <- vector_to_raster_layers(vector_layer, field = "CODE_18", resolution = resolution)

  expect_s4_class(result, "SpatRaster")

  # Verify that the resolution is correct
  expect_equal(terra::res(result), c(resolution, resolution))
})

test_that("vector_to_raster_layers handles missing arguments correctly", {
  gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")

  vector_layer <- sf::st_read(gpkg_path, layer = "clc", quiet = TRUE)

  # Expect an error when neither raster_path nor resolution is provided
  expect_error(
    vector_to_raster_layers(vector_layer, field = "CODE_18"),
    "Either 'base_raster' or 'resolution' must be provided."
  )
})

test_that("vector_to_raster_layers stops if the vector does not overlap with the base raster", {
  gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")
  vector_layer <- sf::st_read(gpkg_path, layer = "clc", quiet = TRUE)

  # Create a raster base outside the vector extent
  base_raster <- terra::rast(
    ext = terra::ext(-1000, -900, -1000, -900),
    crs = sf::st_crs(vector_layer)$wkt,
    res = 100
  )

  # Expect an error due to no overlap
  expect_error(
    vector_to_raster_layers(vector_layer, field = "CODE_18", base_raster = base_raster),
    "The vector layer does not overlap with the base raster."
  )
})

