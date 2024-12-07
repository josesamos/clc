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


testthat::test_that("clc_raster creates an object correctly", {
  source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
  clo <- clc(source = source_gpkg, layer_name = "clc")

  raster_path <- system.file("extdata", "mdt.tif", package = "clc")
  base_raster <- terra::rast(raster_path)

  r <- clo |>
    as_raster(base_raster = base_raster)

  # Check class
  testthat::expect_s3_class(r, "clc_raster")

  # Check structure
  testthat::expect_true(inherits(r$raster, "SpatRaster"))
  testthat::expect_s3_class(r$category, "clc_category")
})

testthat::test_that("get_levels.clc_raster returns correct levels", {
  source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
  clo <- clc(source = source_gpkg, layer_name = "clc")

  r <- clo |>
    as_raster(resolution = 50)

  levels <- get_levels(r)

  # Check structure
  testthat::expect_equal(colnames(levels), c("id", "description", "color"))
  testthat::expect_equal(nrow(levels), 20)
})

testthat::test_that("get_colors.clc_raster returns correct colors", {
  source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
  clo <- clc(source = source_gpkg, layer_name = "clc")

  r <- clo |>
    as_raster(resolution = 50)

  colors <- get_colors(clo)

  # Check structure
  testthat::expect_type(colors, "character")
  testthat::expect_length(colors, 20)
})

testthat::test_that("get_raster.clc_raster returns the raster", {
  source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
  clo <- clc(source = source_gpkg, layer_name = "clc")

  r <- clo |>
    as_raster(resolution = 50)

  raster <- get_raster(r)

  # Check that the raster is returned correctly
  testthat::expect_true(inherits(raster, "SpatRaster"))
  testthat::expect_equal(terra::nlyr(raster), 1) # Ensure single-layer raster
  testthat::expect_equal(terra::res(raster), c(50, 50))
})
