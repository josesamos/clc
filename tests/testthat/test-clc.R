

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


testthat::test_that("clc object is created correctly from GeoPackage", {
  source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")

  clo <- clc(source = source_gpkg, layer_name = "clc")

  testthat::expect_s3_class(clo, "clc")

  # Test components
  testthat::expect_equal(clo$name, "clc")
  testthat::expect_true(!is.null(clo$field))
  testthat::expect_s3_class(clo$category, "clc_category")
  testthat::expect_s3_class(clo$layer, "sf")
  testthat::expect_true(nrow(clo$layer) > 0)
})

testthat::test_that("cut_to_extent correctly clips clc layer", {
  source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")

  polygon <- sf::st_read(source_gpkg, layer = "lanjaron", quiet = TRUE)

  clo <- clc(source = source_gpkg, layer_name = "clc")

  clipped_clo <- clo |> cut_to_extent(polygon)

  # Test class and structure
  testthat::expect_s3_class(clipped_clo, "clc")
  testthat::expect_true(nrow(clipped_clo$layer) > 0)

  # Test CRS consistency
  testthat::expect_equal(sf::st_crs(clipped_clo$layer), sf::st_crs(polygon))
})

testthat::test_that("as_raster converts clc object to raster format", {
  source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")

  clo <- clc(source = source_gpkg, layer_name = "clc")

  raster_path <- system.file("extdata", "mdt.tif", package = "clc")
  base_raster <- terra::rast(raster_path)

  # Convert to raster
  r_clc <- clo |> as_raster(base_raster = base_raster)

  # Test class
  testthat::expect_s3_class(r_clc, "clc_raster")
  testthat::expect_true(inherits(r_clc$raster, "SpatRaster"))
  testthat::expect_true(terra::nlyr(r_clc$raster) == 1)
})

testthat::test_that("save_to saves clc object to GeoPackage", {
  source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")

  clo <- clc(source = source_gpkg, layer_name = "clc")

  temp_gpkg <- tempfile(fileext = ".gpkg")
  clo <- clo |> save_to(temp_gpkg)

  # Test saved GeoPackage
  layers <- sf::st_layers(temp_gpkg)$name
  testthat::expect_true("clc" %in% layers)

  # Verify saved layer
  saved_layer <- sf::st_read(temp_gpkg, layer = "clc", quiet = TRUE)
  testthat::expect_s3_class(saved_layer, "sf")
  testthat::expect_true(nrow(saved_layer) > 0)
})

testthat::test_that("copy_to copies clc style to a new destination", {
  source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")

  clo <- clc(source = source_gpkg, layer_name = "clc")

  temp_gpkg <- tempfile(fileext = ".gpkg")
  clo |> save_to(temp_gpkg)

  clo |> copy_to(temp_gpkg, layers = "clc")

  # Verify styles in the new GeoPackage
  style_layer <- sf::st_read(temp_gpkg, layer = "layer_styles", quiet = TRUE)
  testthat::expect_true(nrow(style_layer) > 0)
  testthat::expect_true(any(style_layer$f_table_name == "clc"))
})

testthat::test_that("plot_clc generates a valid plot", {
  source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")

  clo <- clc(source = source_gpkg, layer_name = "clc")

  p <- clo |> plot_clc()

  # Check plot object
  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("prepare_plot generates a ggplot2 object", {
  source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")

  clo <- clc(source = source_gpkg, layer_name = "clc")

  p <- clo |> prepare_plot()

  # Check plot object
  testthat::expect_s3_class(p, "ggplot")
})

test_that("find_clc_column throws an error for non-sf objects", {
  non_sf_input <- data.frame(column1 = c("100", "200", "300"), column2 = c(1, 2, 3))

  expect_error(
    find_clc_column(non_sf_input),
    "'vector_layer' must be an 'sf' object."
  )
})


