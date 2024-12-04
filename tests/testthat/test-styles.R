
test_that("read_style_from_source works correctly", {
  gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")

  expect_true(file.exists(gpkg_path))

  style <- read_style_from_source(gpkg_path)
  expect_s3_class(style, "data.frame")
  expect_true("f_table_name" %in% colnames(style))

  layer_name <- "clc"
  style_specific <- read_style_from_source(gpkg_path, layer_name = layer_name)
  expect_s3_class(style_specific, "data.frame")
  expect_true(all(style_specific$f_table_name == layer_name))

  nonexistent_layer <- "no_such_layer"
  expect_error(
    read_style_from_source(gpkg_path, layer_name = nonexistent_layer),
    regexp = "No style found for the specified layer name"
  )
})


test_that("assign_styles_to_layers works correctly", {
  original_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")

  expect_true(file.exists(original_gpkg))

  temp_gpkg_no_styles <- tempfile(fileext = ".gpkg")
  sf::st_layers(original_gpkg)$name |>
    setdiff("layer_styles") |>
    purrr::walk(~{
      layer_data <- sf::st_read(original_gpkg, layer = .x, quiet = TRUE)
      sf::st_write(layer_data, temp_gpkg_no_styles, layer = .x, quiet = TRUE)
    })

  temp_gpkg_with_styles <- tempfile(fileext = ".gpkg")
  sf::st_layers(original_gpkg)$name |>
    purrr::walk(~{
      layer_data <- sf::st_read(original_gpkg, layer = .x, quiet = TRUE)
      sf::st_write(layer_data, temp_gpkg_with_styles, layer = .x, quiet = TRUE)
    })

  # Case 1: Assign styles when there are no styles in the destination
  assign_styles_to_layers(
    from = original_gpkg,
    to = temp_gpkg_no_styles,
    layers_to_copy = c("clc"),
    layer_name = "clc"
  )

  styles_no_styles <- sf::st_read(temp_gpkg_no_styles, layer = "layer_styles", quiet = TRUE)
  expect_s3_class(styles_no_styles, "data.frame")
  expect_true("clc" %in% styles_no_styles$f_table_name)

  # Case 2: Update styles when they already exist in the destination
  assign_styles_to_layers(
    from = original_gpkg,
    to = temp_gpkg_with_styles,
    layers_to_copy = c("lanjaron"),
    layer_name = "clc"
  )

  # Verify that the styles for the selected layers have been updated
  styles_with_styles <- sf::st_read(temp_gpkg_with_styles, layer = "layer_styles", quiet = TRUE)
  updated_styles <- styles_with_styles[styles_with_styles$f_table_name %in% c("lanjaron"), ]
  expect_equal(nrow(updated_styles), 1)

  # Verify that unmodified layer styles remain intact
  unmodified_styles <- styles_with_styles[!(styles_with_styles$f_table_name %in% c("lanjaron")), ]
  expect_equal(nrow(unmodified_styles), 1)

  # Case 3: Assign styles to all layers when layers_to_copy is not specified
  assign_styles_to_layers(
    from = original_gpkg,
    to = temp_gpkg_no_styles
  )

  # Verify that all layers have styles assigned
  all_layers_styles <- sf::st_read(temp_gpkg_no_styles, layer = "layer_styles", quiet = TRUE)
  expect_equal(sort(unique(all_layers_styles$f_table_name)), sort(setdiff(sf::st_layers(temp_gpkg_no_styles)$name, "layer_styles")))
})
