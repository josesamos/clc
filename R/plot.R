# Plot vectorial


# library(sf)
# library(ggplot2)
# library(xml2)
#
# # Ruta al GeoPackage
# gpkg_path <- "ruta_a_tu_geopackage.gpkg"
#
# # Leer la capa
# layer <- st_read(gpkg_path, layer = "nombre_de_la_capa")
#
# # Leer el estilo desde la tabla 'layer_styles'
# styles <- st_read(gpkg_path, layer = "layer_styles")
#
# # Extraer el XML del estilo
# style_xml <- read_xml(styles$styleQML[1])
#
# # Extraer categorías (asumiendo un campo <category>)
# categories <- xml_find_all(style_xml, "//category")
# values <- xml_attr(categories, "value")
# labels <- xml_attr(categories, "label")
# colors <- xml_attr(categories, "symbol")
#
# # Convertir colores a un formato interpretable
# rgb2hex <- function(rgb) {
#   rgb_vals <- as.numeric(unlist(strsplit(rgb, ",")))
#   rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], maxColorValue = 255)
# }
# colors_hex <- sapply(colors, rgb2hex)
#
# # Asociar colores a la capa según un atributo
# layer$color <- colors_hex[match(layer$tu_campo, values)]
#
# # Visualizar con ggplot2
# ggplot(data = layer) +
#   geom_sf(aes(fill = color), color = NA) +
#   scale_fill_identity() +
#   theme_minimal()



# Plot ráster


# file <- "datos/Lanjaron/p03out/clc-lanjaron-jsamos.gpkg"
# clc <- sf::st_read(file, layer = "clc-lanjaron-bbox-jsamos")
#
# r_base <- terra::rast(terra::ext(clc), resolution = 25, crs = sf::st_crs(clc)$wkt)
#
# # o bien
#
# file <- 'datos/Lanjaron/p03out/mdt-lanjaron-bbox-jsamos.tif'
# mdt <- terra::rast(file)
#
# r_base <- terra::rast(mdt)
#
#
# clc$CODE_18 <- as.integer(clc$CODE_18)
# r_clc <- terra::rasterize(terra::vect(clc), r_base, field = "CODE_18")
#
# terra::plot(r_clc)
#
# from <- "datos/Lanjaron/p03out/clc-lanjaron-jsamos.gpkg"
# categorias <- sigugr::get_layer_categories(from, r_clc)
#
# from <- "datos/Lanjaron/p03out/clc-lanjaron-jsamos.gpkg"
# categorias <- sigugr::get_layer_categories(from, r_clc)

