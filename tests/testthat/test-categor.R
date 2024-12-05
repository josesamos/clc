test_that("extract_categories_and_colors works correctly", {
  gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")

  styles <- sf::st_read(gpkg_path, layer = "layer_styles", quiet = TRUE)

  expect_gt(nrow(styles), 0)

  categories <- extract_categories_and_colors(styles[1, ])

  expect_s3_class(categories, "data.frame")

  expect_true(all(c("id", "description", "color") %in% colnames(categories)))

  # Verify colors are in hexadecimal format
  expect_true(all(grepl("^#[A-Fa-f0-9]{6}$", categories$color)))
})

test_that("extract_categories_from_style works correctly with specific values", {
  gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")

  categories <- extract_categories_from_style(from = gpkg_path, layer_name = "clc")

  expect_s3_class(categories, "data.frame")

  expect_true(all(c("id", "description", "color") %in% colnames(categories)))

  # Filter by specific values
  unique_ids <- unique(categories$id)
  filtered_categories <- extract_categories_from_style(
    from = gpkg_path,
    layer_name = "clc",
    values = unique_ids[1:2]
  )

  # Verify that only the corresponding categories are returned
  expect_equal(nrow(filtered_categories), 2)
  expect_equal(sort(filtered_categories$id), sort(unique_ids[1:2]))
})

test_that("extract_categories_from_style handles non-existent layers correctly", {
  gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")

  # Attempt to extract categories from a non-existent layer
  expect_error(
    extract_categories_from_style(from = gpkg_path, layer_name = "no_such_layer"),
    "No style found for the specified layer name"
  )
})

test_that("extract_categories_from_style works without specific values", {
  gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")

  categories <- extract_categories_from_style(from = gpkg_path, layer_name = "clc")

  # Verify the result is a complete data.frame
  expect_s3_class(categories, "data.frame")
  expect_gt(nrow(categories), 0)
})
