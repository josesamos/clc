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

test_that("clc_category works correctly with specific values", {
  gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")

  style <- read_style_from_source(gpkg_path, layer_name = "clc")

  categories <- clc_category(style, values = NULL)

  expect_s3_class(categories, "clc_category")

  expect_true(all(c("id", "description", "color") %in% names(categories)))

  # Filter by specific values
  unique_ids <- unique(categories$id)
  filtered_categories <- clc_category(style, values = unique_ids[1:2])

  # Verify that only the corresponding categories are returned
  expect_equal(length(filtered_categories$id), 2)
  expect_equal(sort(filtered_categories$id), sort(unique_ids[1:2]))
})

