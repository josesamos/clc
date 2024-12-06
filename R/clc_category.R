#' `clc_category` S3 Class
#'
#' Create an object of class `clc_category`.
#'
#' This function creates an object of class `clc_category` from a CLC style definition
#' and a set of values to filter the original style.
#'
#' @param style A data frame containing a QGIS QML style in the column `styleQML`.
#' @param values The set of values used to filter the results; filtering is applied only if this set
#'   is not NULL.
#' @return An object of class `clc_category`.
#' @examples
#' source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
#' obj <- clc(source_gpkg, "clc")
#'
#' @export
clc_category <- function(style, values = NULL) {

  cat <- extract_categories_and_colors(style)

  if (!is.null(values)) {
    cat <- cat[cat$id %in% values, ]
  }

  obj <- list(
    id = cat$id,
    description = cat$description,
    color = cat$color
  )

  class(obj) <- "clc_category"
  obj
}


#' Retrieve levels from a `clc_category` object
#'
#' This generic function extracts the levels values associated with a
#' `clc_category` object. It returns a data frame contains the fields `id`,
#' `description`, and `color` from the `clc_category` object.
#'
#' @param cloc A `clc_category` object.
#'
#' @return A data frame with columns:
#'   - `id`: The identifier of the category.
#'   - `description`: A textual description of the category.
#'   - `color`: The color associated with the category.
#'
#' @examples
#' #
#'
#' @export
get_levels <- function(cloc)
  UseMethod("get_levels")


#' @rdname get_levels
#' @export
get_levels.clc_category <- function(cloc) {
  data.frame(
    id = cloc$id,
    description = cloc$description,
    color = cloc$color,
    stringsAsFactors = FALSE
  )
}


#' Retrieve colors from a `clc_category` object
#'
#' This generic function extracts the color values associated with a
#' `clc_category` object. It returns a character vector containing the
#' `color` field from the object.
#'
#' @param cloc A `clc_category` object.
#'
#' @return A character vector of colors.
#'
#' @examples
#' #
#'
#' @export
get_colors <- function(cloc)
  UseMethod("get_colors")


#' @rdname get_colors
#' @export
get_colors.clc_category <- function(cloc) {
  cloc$color
}


#' Extract categories and colors from a style
#'
#' Parses a QGIS QML style file to extract categories and their associated colors.
#'
#' @param style A data frame containing a QGIS QML style in the column `styleQML`.
#' The first entry is used for extraction.
#' @return A data frame with the following columns:
#' - `id`: Integer category IDs.
#' - `description`: Labels associated with each category.
#' - `color`: Hexadecimal representation of the colors for each category.
#' @keywords internal
#' @noRd
extract_categories_and_colors <- function(style) {
  st_xml <- xml2::read_xml(style$styleQML[1])

  categories <- xml2::xml_find_all(st_xml, "//category")
  id <- as.integer(xml2::xml_attr(categories, "value"))
  des <- xml2::xml_attr(categories, "label")

  s <- xml2::xml_find_all(st_xml, ".//symbols/symbol")
  name <- xml2::xml_attr(s, "name")
  color <- xml2::xml_find_first(s, ".//prop[@k='color']") |> xml2::xml_attr("v")

  rgb2hex <- function(color) {
    rgb <- strsplit(color, ",")
    rgb <- rgb[[1]]
    rgb <- as.numeric(rgb)
    rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255)
  }
  color2 <- sapply(color, rgb2hex)
  names(color2) <- name
  color2 <- color2[order(as.numeric(names(color2)))]

  return(data.frame(
    id = id,
    description = des,
    color = color2
  ))
}

