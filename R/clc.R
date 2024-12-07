.onLoad <- function(libname, pkgname) {
  utils::data(
    "clc_codes",
    package = pkgname,
    envir = parent.env(environment())
  )
}

#' `clc` S3 Class
#'
#' Create an object of class `clc`.
#'
#' This function creates an object of class `clc` from a vector layer in either
#' a GeoPackage or a PostGIS database.
#'
#' The layer must have a style defined in the source.
#'
#' @param source The source of the vector layer. This can be:
#'   - A string representing the path to a GeoPackage file.
#'   - A `DBI` database connection object to a PostGIS database, created using [RPostgres::dbConnect()].
#' @param layer_name The name of the layer in the source to be used.
#' @param field (Optional) A string, the layer field that contains CLC codes. If NULL,
#'   the function will attempt to locate the column containing the CLC codes.
#'
#' @return An object of class `clc`.
#'
#' @family CLC class functions
#'
#' @examples
#' # ex1
#' source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
#' clo <- clc(source = source_gpkg, layer_name = "clc")
#'
#' \dontrun{
#' # ex2
#' conn <- RPostgres::dbConnect(
#'   RPostgres::Postgres(),
#'   dbname = 'exampledb',
#'   host = 'localhost',
#'   port = '5432',
#'   user = 'user',
#'   password = 'password'
#' )
#' clo <- clc(source = conn, layer_name = "clc")
#' }
#' @export
clc <- function(source, layer_name, field = NULL) {
  layer <- suppressWarnings(sf::st_read(source, layer = layer_name, quiet = TRUE))
  style <- read_style_from_source(source, layer_name)

  if (is.null(field)) {
    field <- find_clc_column(layer)
  }

  clc_new(layer, style, layer_name, field)
}


#' New `clc` object
#'
#' @param layer A vector layer in `sf` format.
#' @param style A data frame containing a QGIS QML style in the column `styleQML`.
#' @param layer_name The name of the layer in the source to be used.
#' @param field A string, the layer field that contains CLC codes.
#'
#' @return An object of class `clc`.
#'
#' @keywords internal
#' @noRd
clc_new <- function(layer, style, layer_name, field) {
  values <- sort(unique(layer[[field]]))
  category <- clc_category(style, values)

  obj <- list(
    name = layer_name,
    field = field,
    values = values,
    category = category,
    layer = layer,
    style = style
  )

  class(obj) <- "clc"
  obj
}

#' Clip the Layer with a Polygon
#'
#' This function clips the object layer using a polygon layer. It handles CRS
#' transformations automatically if necessary, ensuring the output is in the same
#' CRS as the input polygon.
#'
#' @param clo A `clc` object.
#' @param polygon An `sf` object representing the polygon layer used for clipping.
#'
#' @return A `clc` object.
#'
#' @family CLC class functions
#'
#' @examples
#' source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
#' clo <- clc(source = source_gpkg, layer_name = "clc")
#'
#' polygon <- sf::st_read(source_gpkg, layer = 'lanjaron', quiet = TRUE)
#'
#' clo2 <- clo |>
#'         cut_to_extent(polygon)
#'
#' @export
cut_to_extent <- function(clo, polygon)
  UseMethod("cut_to_extent")


#' @rdname cut_to_extent
#' @export
cut_to_extent.clc <- function(clo, polygon) {

  layer <- clip_multipoligon(clo$layer, polygon)

  clc_new(layer, clo$style, clo$layer_name, clo$field)
}


#' Convert a `clc` Object to Raster Format
#'
#' Returns an object of class `clc_raster` that contains a `terra::SpatRaster` raster
#' object representing the converted vector layer into raster format.
#'
#' @param clo A `clc` object.
#' @param base_raster (Optional) A raster object to use as the base for rasterization.
#' @param resolution (Optional) Numeric resolution to define the raster grid.
#'
#' @return An object of class `clc_raster`.
#'
#' @family CLC class functions
#'
#' @details The function requires either `base_raster` or `resolution` to be provided.
#' If both are missing, an error is raised.
#'
#' @examples
#' source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
#' clo <- clc(source = source_gpkg, layer_name = "clc")
#'
#' raster_path <- system.file("extdata", "mdt.tif", package = "clc")
#' base_raster <- terra::rast(raster_path)
#'
#' # ex1
#' r <- clo |>
#'      as_raster(base_raster = base_raster)
#'
#' # ex2
#' r <- clo |>
#'      as_raster(resolution = 50)
#'
#' @export
as_raster <- function(clo, base_raster, resolution)
  UseMethod("as_raster")

#' @rdname as_raster
#' @export
as_raster.clc <- function(clo,
                          base_raster = NULL,
                          resolution = NULL) {

  clc_raster(clo$layer, clo$field, clo$category, base_raster, resolution)
}


#' Save a Layer and its Style to a GeoPackage or PostGIS Database
#'
#' This function saves a layer and its style to a GeoPackage file or a PostGIS database.
#' The destination is determined by the `to` argument.
#'
#' @param clo A `clc` object.
#' @param to A data destination for the output. This can be:
#'   - A string representing the path to a GeoPackage file.
#'   - A `DBI` database connection object to a PostGIS database, created using [RPostgres::dbConnect()].
#' @param database A string, database name, only in case the destination is in PostGIS.
#' @param schema A string, schema name, only in case the destination is in PostGIS.
#'   Defaults to `'public'`.
#' @param layer_name A character string specifying the name of the layer in the output.
#'   If `NULL`, the name of the input `layer` is used.
#'
#' @return clo A `clc` object.
#'
#' @family CLC class functions
#'
#' @details The function overwrites the table if it already exists.
#'
#' @examples
#' source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
#' clo <- clc(source = source_gpkg, layer_name = "clc")
#'
#' # ex1
#' out_gpkg <- tempfile(fileext = ".gpkg")
#' clo <- clo |>
#'   save_to(out_gpkg)
#'
#' \dontrun{
#' # ex2
#' conn <- RPostgres::dbConnect(
#'   RPostgres::Postgres(),
#'   dbname = 'exampledb',
#'   host = 'localhost',
#'   port = '5432',
#'   user = 'user',
#'   password = 'password'
#' )
#' clo <- clo |>
#'   save_to(conn, 'exampledb')
#' }
#' @export
save_to <- function(clo, to, database, schema, layer_name)
  UseMethod("save_to")


#' @rdname save_to
#' @export
save_to.clc <- function(clo,
                     to,
                     database = NULL,
                     schema = 'public',
                     layer_name = NULL) {
  if (is.null(layer_name)) {
    layer_name <- clo$name
  }

  suppressMessages(sf::st_write(
    obj = clo$layer,
    dsn = to,
    layer = layer_name,
    delete_layer = TRUE
  ))

  assign_styles_to_layers(clo$style, to, database, schema, layers = layer_name)
  clo
}


#' Copy a Style to a GeoPackage or PostGIS Database
#'
#' This function copies a style to the specified layers in a GeoPackage file or
#' a PostGIS database. The destination is determined by the `to` argument.
#'
#' @param clo A `clc` object.
#' @param to A data destination for the output. This can be:
#'   - A string representing the path to a GeoPackage file.
#'   - A `DBI` database connection object to a PostGIS database, created using [RPostgres::dbConnect()].
#' @param database A string, database name, only in case the destination is in PostGIS.
#' @param schema A string, schema name, only in case the destination is in PostGIS.
#'   Defaults to `'public'`.
#' @param layers An optional character vector specifying the names of layers in the
#'   destination to which the styles should be applied. If `NULL` (default), applies
#'   the style to all layers.
#'
#' @return clo A `clc` object.
#'
#' @family CLC class functions
#'
#' @examples
#' source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
#' clo <- clc(source = source_gpkg, layer_name = "clc")
#'
#' out_gpkg <- tempfile(fileext = ".gpkg")
#' clo <- clo |>
#'   save_to(out_gpkg)
#'
#' # ex1
#' clo <- clo |>
#'   copy_to(out_gpkg, layers = 'clc')
#'
#' \dontrun{
#' conn <- RPostgres::dbConnect(
#'   RPostgres::Postgres(),
#'   dbname = 'exampledb',
#'   host = 'localhost',
#'   port = '5432',
#'   user = 'user',
#'   password = 'password'
#' )
#' clo <- clo |>
#'   save_to(conn, 'exampledb')
#'
#' # ex2
#' clo <- clo |>
#'   copy_to(conn, 'exampledb', layers = 'clc')
#' }
#' @export
copy_to <- function(clo, to, database, schema, layers)
  UseMethod("copy_to")


#' @rdname copy_to
#' @export
copy_to.clc <- function(clo,
                        to,
                        database = NULL,
                        schema = 'public',
                        layers = NULL) {
  assign_styles_to_layers(clo$style, to, database, schema, layers)
  clo
}


#' Plot CLC Layer
#'
#' Plot CLC data stored in objects of supported classes. The function adapts the plot
#' based on the class of the input data (vectorial or raster format).
#'
#' For the raster version, the `terra::plot` function is used with the `col` parameter
#' configured, while all other parameters supported by the function can also be defined (using `...`).
#'
#' For the vector version, `ggplot2::ggplot` is used, and by using the `prepare_plot` function
#' instead of this one (`plot_clc`), further customization can be applied as needed.
#'
#' @param clo An object containing CLC data. This must be an instance of a supported class, such as:
#'   - A vectorial CLC data object (e.g., `clc` object).
#'   - A raster CLC data object (e.g., `clc_raster`).
#' @param ... Additional arguments passed to the `terra::plot` function.
#'
#' @return An object containing CLC data.
#'
#' @family CLC class functions
#' @seealso \code{\link{prepare_plot}}
#'
#' @examples
#' source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
#' clo <- clc(source = source_gpkg, layer_name = "clc")
#'
#' temp_file <- tempfile(fileext = ".png")
#' png(filename = temp_file, width = 800, height = 600)
#'
#' clo |>
#'   plot_clc()
#'
#' dev.off()
#'
#' @export
plot_clc <- function(clo, ...)
  UseMethod("plot_clc")


#' @rdname plot_clc
#' @export
plot_clc.clc <- function(clo, ...) {

  levels <- clo |> get_levels()

  clo |> prepare_plot() +
    ggplot2::scale_fill_manual(
      values = stats::setNames(levels$color, levels$id),
      labels = stats::setNames(levels$description, levels$id),
      name = ""
    ) +
    ggplot2::theme_minimal()
  clo
}


#' Prepare a Plot for CLC Vectorial Data
#'
#' Generates a `ggplot2` object to visualize CLC Vectorial data. The function
#' processes the data stored in a `clc` object, ensuring that the codes field is
#' mapped correctly to the categories and their associated styles.
#'
#' @param clo A `clc` object.
#'
#' @return A `ggplot2` object ready for rendering.
#'
#' @family CLC class functions
#' @seealso \code{\link{plot_clc}}
#'
#' @examples
#' source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
#' clo <- clc(source = source_gpkg, layer_name = "clc")
#'
#' p <- clo |>
#'   prepare_plot()
#'
#' levels <- clo |>
#'   get_levels()
#'
#' p <- p +
#'   ggplot2::scale_fill_manual(
#'     values = stats::setNames(levels$color, levels$id),
#'     labels = stats::setNames(levels$description, levels$id),
#'     name = ""
#'   ) +
#'   ggplot2::theme_minimal()
#'
#' temp_file <- tempfile(fileext = ".png")
#' png(filename = temp_file, width = 800, height = 600)
#'
#' p
#'
#' dev.off()
#'
#' @export
prepare_plot <- function(clo)
  UseMethod("prepare_plot")


#' @rdname prepare_plot
#' @export
prepare_plot.clc <- function(clo) {

  field <- clo$field
  layer <- clo$layer
  levels <- clo |> get_levels()

  layer[[field]] <- factor(layer[[field]], levels = levels$id)

  p <- ggplot2::ggplot(data = layer) +
    ggplot2::geom_sf(ggplot2::aes(fill = !!rlang::sym(field)))
  p
}


#' @rdname get_levels
#' @export
get_levels.clc <- function(clo) {
  clo$category |>
    get_levels()
}


#' @rdname get_colors
#' @export
get_colors.clc <- function(clo) {
  clo$category |>
    get_colors()
}



#' Find Column Matching CLC Codes
#'
#' Identifies the name of the column in an `sf` object whose unique values
#' are a subset of the specified CLC codes. Throws an error if no such column
#' exists or if more than one column satisfies the condition.
#'
#' @param vector_layer An `sf` object representing the vector layer.
#'
#' @return The name of the column as a character string.
#'
#' @keywords internal
#' @noRd
find_clc_column <- function(vector_layer) {
  if (!inherits(vector_layer, "sf")) {
    stop("'vector_layer' must be an 'sf' object.")
  }

  # Check each column
  matching_columns <- sapply(vector_layer, function(column) {
    if (is.character(column)) {
      all(unique(column) %in% clc_codes)
    } else {
      FALSE
    }
  })

  matched_names <- names(matching_columns)[matching_columns]

  if (length(matched_names) == 0) {
    # Check each column
    matching_columns <- sapply(vector_layer, function(column) {
      if (is.numeric(column)) {
        all(unique(suppressWarnings(as.integer(column))) %in% as.integer(clc_codes))
      } else {
        FALSE
      }
    })

    matched_names <- names(matching_columns)[matching_columns]
  }

  if (length(matched_names) == 0) {
    stop("No column found whose values are a CLC code.")
  }
  if (length(matched_names) > 1) {
    stop("Multiple columns found whose values are CLC codes. Please specify explicitly.")
  }

  matched_names
}
