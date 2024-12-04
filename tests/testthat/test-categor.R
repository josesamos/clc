test_that("extract_categories_and_colors works correctly", {
  # Path to the GeoPackage included in the package
  gpkg_path <- system.file("extdata", "clc.gpkg", package = "nombre_del_paquete")

  # Read the styles layer
  styles <- sf::st_read(gpkg_path, layer = "layer_styles", quiet = TRUE)

  # Ensure the styles layer contains data
  expect_gt(nrow(styles), 0)

  # Extract categories and colors
  categories <- extract_categories_and_colors(styles[1, ])

  # Verify the result is a data.frame
  expect_s3_class(categories, "data.frame")

  # Verify the columns are present
  expect_true(all(c("id", "description", "color") %in% colnames(categories)))

  # Verify colors are in hexadecimal format
  expect_true(all(grepl("^#[A-Fa-f0-9]{6}$", categories$color)))
})

test_that("extract_categories_from_style works correctly with specific values", {
  # Path to the GeoPackage included in the package
  gpkg_path <- system.file("extdata", "clc.gpkg", package = "nombre_del_paquete")

  # Extract categories from the GeoPackage
  categories <- extract_categories_from_style(from = gpkg_path, layer_name = "clc")

  # Verify the result is a data.frame
  expect_s3_class(categories, "data.frame")

  # Verify the columns are present
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
  # Path to the GeoPackage included in the package
  gpkg_path <- system.file("extdata", "clc.gpkg", package = "nombre_del_paquete")

  # Attempt to extract categories from a non-existent layer
  expect_error(
    extract_categories_from_style(from = gpkg_path, layer_name = "no_such_layer"),
    "No style found for the specified layer name"
  )
})

test_that("extract_categories_from_style works without specific values", {
  # Path to the GeoPackage included in the package
  gpkg_path <- system.file("extdata", "clc.gpkg", package = "nombre_del_paquete")

  # Extract categories without specifying values
  categories <- extract_categories_from_style(from = gpkg_path, layer_name = "clc")

  # Verify the result is a complete data.frame
  expect_s3_class(categories, "data.frame")
  expect_gt(nrow(categories), 0)
})
