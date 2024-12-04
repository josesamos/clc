# Function to read the style from GeoPackage or PostGIS
read_style_from_source <- function(source, layer_name = NULL) {
  style <- suppressWarnings(sf::st_read(source, layer = "layer_styles", quiet = TRUE))

  if (!is.null(layer_name)) {
    style <- style[style$f_table_name == layer_name, ]
    if (nrow(style) == 0) {
      stop("No style found for the specified layer name: ", layer_name)
    }
  } else {
    style <- style[1, ]
  }

  # transform PostGIS style into GeoPackage style
  if ("id" %in% names(style)) {
    names_style_geo <- c("f_table_catalog", "f_table_schema", "f_table_name", "f_geometry_column",
                         "styleName", "styleQML", "styleSLD", "useAsDefault", "description",
                         "owner", "ui", "update_time")
    style <- style[, tolower(names_style_geo)]
    names(style) <- names_style_geo
    style$f_table_catalog <- ''
    style$f_table_schema <- ''
  }
  return(style)
}

# Function to get the layers to copy
get_layers_to_copy <- function(layers_to_copy, all_layers) {
  all_layers <- setdiff(all_layers, c("raster_columns", "layer_styles"))
  if (is.null(layers_to_copy)) {
    layers_to_copy <- all_layers
  } else {
    missing_layers <- setdiff(layers_to_copy, all_layers)
    if (length(missing_layers) > 0) {
      stop("The following layers do not exist in the destination: ",
           paste(missing_layers, collapse = ", "))
    }
  }
  layers_to_copy
}

# Function to get existing styles
get_existing_styles <- function(to, layers_in_to, style) {
  if ("layer_styles" %in% layers_in_to) {
    existing_styles <- suppressWarnings(sf::st_read(to, layer = "layer_styles", quiet = TRUE))
  } else {
    existing_styles <- style[0, ]  # Create an empty table with the same structure
  }
}

# Function that generates the table with the styles of the indicated layers
generate_new_styles <- function(layers_to_copy, style, database = NULL, schema = NULL) {
  n <- length(layers_to_copy)
  new_styles <- do.call(rbind, replicate(n, style, simplify = FALSE))

  for (i in seq_along(layers_to_copy)) {
    new_styles$f_table_name[i] <- layers_to_copy[i]
    if (!is.null(schema)) {
      new_styles$f_table_schema[i] <- schema
    }
    if (!is.null(database)) {
      new_styles$f_table_catalog[i] <- database
    }
    new_styles$styleSLD[i] <- gsub(
      style$f_table_name,
      layers_to_copy[i],
      new_styles$styleSLD[i],
      fixed = TRUE
    )
  }
  if (!is.null(database)) {
    new_styles$useasdefault <- TRUE
    new_styles$id <- 1
  }
  new_styles
}

# Function to combine existing styles and new styles for layers to copy
combine_styles <- function(existing_styles, new_styles, layers_to_copy, to) {
  if ('id' %in% names(new_styles)) {
    names(new_styles) <- tolower(names(new_styles))
    new_styles <- new_styles[, c('id', setdiff(names(new_styles), 'id'))]
  }
  combined_styles <- rbind(
    existing_styles[!(existing_styles$f_table_name %in% layers_to_copy), ],
    new_styles
  )
  if ('id' %in% names(combined_styles)) {
    combined_styles$id <- 1:nrow(combined_styles)
  }
  sf::st_write(
    obj = combined_styles,
    dsn = to,
    layer = "layer_styles",
    append = FALSE,
    quiet = TRUE
  )
  combined_styles
}



# Function to get all layers in PostGIS
get_all_layers_pg <- function(conn, schema) {
  query <- "
  SELECT
    table_schema AS schema_name,
    table_name,
    column_name AS geometry_column,
    udt_name AS geometry_type
  FROM information_schema.columns
  WHERE udt_name IN ('geometry', 'geography');
"
  all_layers <- RPostgres::dbGetQuery(conn, query)
  all_layers <- all_layers[all_layers$schema_name == schema, "table_name"]
}


# Function to check if 'layer_styles' table exists in PostGIS
exist_layer_styles_pg <- function(conn, schema = "public") {
  table <- "layer_styles"
  query_check <- sprintf("
    SELECT 1
    FROM information_schema.tables
    WHERE table_name = '%s' AND table_schema = '%s';",
                         table, schema
  )
  table_exists <- RPostgres::dbGetQuery(conn, query_check)

  if (nrow(table_exists) > 0) {
    table
  } else {
    NULL
  }
}



#' Assign a Specified or Default Style to Layers in a GeoPackage Based on Layer Name
#'
#' This function copies the `layer_styles` from a source GeoPackage to a destination GeoPackage.
#' By default, it assigns the first style from the source to all layers in the destination,
#' but a specific style can be selected based on the layer name from the source GeoPackage.
#' If no style is specified, the first style in the `layer_styles` table is applied.
#'
#' @param from A data source for the input styles. This can be:
#'   - A string representing the path to a GeoPackage file.
#'   - A `DBI` database connection object to a PostGIS database, created using [RPostgres::dbConnect()].
#' @param to A data destination for the output styles. This can be:
#'   - A string representing the path to a GeoPackage file.
#'   - A `DBI` database connection object to a PostGIS database, created using [RPostgres::dbConnect()].
#' @param database A string, database name, only in case the destination is in PostGIS.
#' @param schema A string, schema name, only in case the destination is in PostGIS.
#' @param layers_to_copy An optional character vector specifying the names of layers
#'   in the destination GeoPackage to which the styles should be applied.
#'   If `NULL` (default), applies the style to all layers.
#' @param layer_name An optional string representing the name of the layer from the source
#'   `layer_styles` table whose style should be applied. If `NULL` (default), applies the
#'   first style.
#'
#' @return The updated `layer_styles` table, returned invisibly.
#'
#' @examples
#' \dontrun{
#' source_gpkg <- "source.gpkg"
#' dest_gpkg <- "destination.gpkg"
#'
#' # Assign styles to all layers using the first style
#' assign_styles_to_layers(from = source_gpkg, to = dest_gpkg)
#'
#' # Assign styles to specific layers using the first style
#' assign_styles_to_layers(from = source_gpkg, to = dest_gpkg,
#'                         layers_to_copy = c("layer1", "layer2"))
#'
#' # Assign a specific style based on layer name to all layers
#' assign_styles_to_layers(from = source_gpkg, to = dest_gpkg,
#'                         layer_name = "layerX")
#'
#' conn <- RPostgres::dbConnect(
#'   RPostgres::Postgres(),
#'   dbname = 'exampledb',
#'   host = 'localhost',
#'   port = '5432',
#'   user = 'postgres',
#'   password = 'postgres'
#' )
#' dest_gpkg <- "destination.gpkg"
#'
#' # Assign styles to all layers using the first style
#' assign_styles_to_layers(from = conn, to = dest_gpkg)
#'
#' # Assign styles to specific layers using the first style
#' assign_styles_to_layers(from = conn, to = dest_gpkg,
#'                         layers_to_copy = c("layer1", "layer2"))
#'
#' # Assign a specific style based on layer name to all layers
#' assign_styles_to_layers(from = conn, to = dest_gpkg,
#'                         layer_name = "layerX")
#'
#' RPostgres::dbDisconnect(conn)
#'
#' source_gpkg <- "source.gpkg"
#' conn <- RPostgres::dbConnect(
#'   RPostgres::Postgres(),
#'   dbname = 'exampledb',
#'   host = 'localhost',
#'   port = '5432',
#'   user = 'postgres',
#'   password = 'postgres'
#' )
#'
#' # Assign styles to all layers using the first style
#' assign_styles_to_layers(from = source_gpkg, to = conn, database = 'exampledb')
#'
#' # Assign styles to specific layers using the first style
#' assign_styles_to_layers(from = source_gpkg, to = conn, database = 'exampledb',
#'                         layers_to_copy = c("layer1", "layer2"))
#'
#' # Assign a specific style based on layer name to all layers
#' assign_styles_to_layers(from = source_gpkg, to = conn, database = 'exampledb',
#'                         layer_name = "layerX")
#'
#' RPostgres::dbDisconnect(conn)
#'
#'
#' source_conn <- RPostgres::dbConnect(
#'   RPostgres::Postgres(),
#'   dbname = 'exampledb1',
#'   host = 'localhost',
#'   port = '5432',
#'   user = 'postgres',
#'   password = 'postgres'
#' )
#' conn <- RPostgres::dbConnect(
#'   RPostgres::Postgres(),
#'   dbname = 'exampledb2',
#'   host = 'localhost',
#'   port = '5432',
#'   user = 'postgres',
#'   password = 'postgres'
#' )
#'
#' # Assign styles to all layers using the first style
#' assign_styles_to_layers(from = source_conn, to = conn, database = 'exampledb2')
#'
#' # Assign styles to specific layers using the first style
#' assign_styles_to_layers(from = source_conn, to = conn, database = 'exampledb2',
#'                         layers_to_copy = c("layer1", "layer2"))
#'
#' # Assign a specific style based on layer name to all layers
#' assign_styles_to_layers(from = source_conn, to = conn, database = 'exampledb2',
#'                         layer_name = "layerX")
#'
#' RPostgres::dbDisconnect(source_conn)
#' RPostgres::dbDisconnect(conn)
#' }
#'
#' @export
assign_styles_to_layers <- function(from, to, database = NULL, schema='public', layers_to_copy = NULL, layer_name = NULL) {
  style <- read_style_from_source(from, layer_name)

  if (is.null(database)) {
    schema <- NULL
    all_layers <- sf::st_layers(to)$name
    layers_in_to <- all_layers
  } else {
    all_layers <- get_all_layers_pg(conn, schema)
    layers_in_to <- exist_layer_styles_pg(conn, schema)
  }

  layers_to_copy <- get_layers_to_copy(layers_to_copy, all_layers)

  existing_styles <- get_existing_styles(to, layers_in_to, style)

  new_styles <- generate_new_styles(layers_to_copy, style, database, schema)

  combined_styles <- combine_styles(existing_styles, new_styles, layers_to_copy, to)

  invisible(combined_styles)
}

