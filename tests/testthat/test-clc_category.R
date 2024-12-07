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


testthat::test_that("clc_category creates an object correctly", {
  source <- system.file("extdata", "clc.gpkg", package = "clc")
  layer_name <- "clc"
  layer <- suppressWarnings(sf::st_read(source, layer = layer_name, quiet = TRUE))
  field <- "CODE_18"
  style <- read_style_from_source(source, layer_name)

  # Values to filter
  values <- c(111, 112)

  clo <- clc_category(style, values)

  testthat::expect_s3_class(clo, "clc_category")

  # Check content
  testthat::expect_equal(clo$id, c(111, 112))
  testthat::expect_equal(clo$description, c("Tejido urbano continuo", "Tejido urbano discontinuo"))
  testthat::expect_equal(clo$color, c("#E6004D", "#FF0000"))
})

testthat::test_that("get_levels.clc_category returns correct levels", {
  source <- system.file("extdata", "clc.gpkg", package = "clc")
  layer_name <- "clc"
  layer <- suppressWarnings(sf::st_read(source, layer = layer_name, quiet = TRUE))
  field <- "CODE_18"
  style <- read_style_from_source(source, layer_name)
  clo <- clc_category(style)

  # Retrieve levels
  levels <- get_levels(clo)

  # Check structure
  testthat::expect_equal(colnames(levels), c("id", "description", "color"))
  testthat::expect_equal(nrow(levels), 44)
})

testthat::test_that("get_colors.clc_category returns correct colors", {
  source <- system.file("extdata", "clc.gpkg", package = "clc")
  layer_name <- "clc"
  layer <- suppressWarnings(sf::st_read(source, layer = layer_name, quiet = TRUE))
  field <- "CODE_18"
  style <- read_style_from_source(source, layer_name)
  clo <- clc_category(style)

  # Retrieve colors
  colors <- get_colors(clo)

  # Check structure
  testthat::expect_type(colors, "character")
  testthat::expect_length(colors, 44)
})

