#' Get layer categories from a styles layer from one source `GeoPackage`
#'
#' It obtains them from the first defined style.
#'
#' @param from A GeoPackage file name.
#' @param r_clc A `terra` raster.
#'
#' @return `categories` A data frame of categories.
#'
#' @family transformation functions
#'
#' @examples
#' #
#'
#' @export
get_layer_categories <- function(from, r_clc) {
  layer <- "layer_styles"
  style <- sf::st_read(from, layer = layer, quiet = TRUE)
  style <- style[1, ]
  st_xml <- xml2::read_xml(style$styleQML[1])

  categories <- xml2::xml_find_all(st_xml, "//category")
  id <- as.integer(xml2::xml_attr(categories, "value"))
  des <- xml2::xml_attr(categories, "label")
  symbol <- xml2::xml_attr(categories, "symbol")

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

  values <- sort(terra::unique(r_clc)[, 1])

  if (!is.null(values)) {
    des <- des[id %in% values]
    color2 <- color2[id %in% values]
    id <- id[id %in% values]
  }
  categories <- data.frame(
    ID = id,
    Descripcion = des,
    Color = color2
  )
}

# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}
