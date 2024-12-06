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



test_that("find_clc_column identifies the correct column in the clc layer", {
  gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")

  clc_layer <- sf::st_read(gpkg_path, layer = "clc", quiet = TRUE)

  # Test for the correct column
  column_name <- find_clc_column(clc_layer)
  expect_equal(column_name, "CODE_18")
})


test_that("find_clc_column identifies the correct numeric column in the clc layer", {
  gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")

  clc_layer <- sf::st_read(gpkg_path, layer = "clc", quiet = TRUE)
  clc_layer[["CODE_18"]] <- as.integer(clc_layer[["CODE_18"]])

  # Test for the correct column
  column_name <- find_clc_column(clc_layer)
  expect_equal(column_name, "CODE_18")
})


test_that("find_clc_column throws an error when no column matches in lanjaron layer", {
  gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")

  lanjaron_layer <- sf::st_read(gpkg_path, layer = "lanjaron", quiet = TRUE)

  # Test for error when no column matches
  expect_error(find_clc_column(lanjaron_layer),
                         "No column found whose values are a CLC code.")
})

test_that("find_clc_column throws an error when multiple columns match", {
  gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")

  clc_layer <- sf::st_read(gpkg_path, layer = "clc", quiet = TRUE)
  clc_layer$duplicate_column <- clc_layer$CODE_18

  # Test for error when multiple columns match
  expect_error(find_clc_column(clc_layer),
                         "Multiple columns found whose values are CLC codes. Please specify explicitly.")
})

test_that("find_clc_column works with an empty sf object", {
  empty_layer <- sf::st_sf(sf::st_sfc(), data.frame())

  # Test for error when the sf object is empty
  expect_error(find_clc_column(empty_layer),
                         "No column found whose values are a CLC code.")
})


test_that("plot function runs without errors", {

  source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
  clo <- clc(source = source_gpkg, layer_name = "clc")

  r <- clo |>
       as_raster(resolution = 50)

  temp_file <- tempfile(fileext = ".png")
  png(filename = temp_file, width = 800, height = 600)

  expect_silent(plot_clc(r))

  dev.off()
})

