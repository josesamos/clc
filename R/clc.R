.onLoad <- function(libname, pkgname) {
  utils::data(
    "sat_rest",
    "sat_band",
    "sat_rest_msk",
    package = pkgname,
    envir = parent.env(environment())
  )
}

#' `clc` S3 class
#'
#' Creates a `clc` object from a set of raster files.
#'
#' Given a folder name or a vector of folder names, containing satellite band
#' raster files, creates an object containing all rasters grouped according to
#' their spatial resolution.
#'
#' If there are several rasters of the same area (tiles), it previously merges
#' them to form a single raster of the total area.
#'
#' A working folder where the virtual rasters are created can be indicated as a
#' parameter. Additionally, we indicate whether we wish to process only the
#' spectral band files (B1 to B12) or all available files.
#'
#' @param dir A string or string vector, folder names.
#' @param out_dir A string, output folder.
#' @param only_spectral_bands A boolean, include only spectral bands.
#'
#' @return A `clc` object.
#'
#' @family satellite definition
#' @seealso \code{\link{sat_untarzip}}
#'
#' @examples
#'
#' esa <- system.file("extdata", "esa", package = "clc")
#'
#' sr <- clc(dir = esa)
#'
#' sr <- clc(dir = esa,
#'              out_dir = tempdir(),
#'              only_spectral_bands = FALSE)
#'
#' @export
clc <- function(dir, out_dir = NULL, only_spectral_bands = TRUE) {
  files <- NULL
  for (d in dir) {
    lf <-
      list.files(
        path = d,
        pattern = "*.TIF|*.jp2",
        recursive = TRUE,
        full.names = TRUE,
        ignore.case = TRUE
      )
    files <- c(files, lf)
  }
  if (is.null(out_dir)) {
    dir <- tempdir()
    sub_dir <- snakecase::to_snake_case(paste0(Sys.time()))
    dir.create(file.path(dir, sub_dir))
    out_dir <- paste0(dir, '/', sub_dir)
  }
  b_r <- select_band_files(files)
  if (only_spectral_bands) {
    files <- b_r[['band']]
  } else {
    files <- c(b_r[['band']], b_r[['rest']])
  }

  names <- sort(unique(names(files)))
  b <- vector("list", length = length(names))
  names(b) <- names
  nexus <- get_nexus(out_dir)
  vf <- NULL
  names_1layer <- NULL
  resolution <- NULL
  for (n in names) {
    vfn <- paste0(n, ".vrt")
    f <- paste0(out_dir, nexus, vfn)
    vf <- c(vf, vfn)
    t <- terra::vrt(files[names(files) == n], f, overwrite = TRUE)
    # only tiles of the same raster
    if (terra::nlyr(t) == 1) {
      b[[n]] <- t
      names_1layer <- c(names_1layer, n)
      resolution <- c(resolution, terra::res(t)[1])
    }
  }
  b <- b[names_1layer]
  names <- names(b)
  names <- gsub("_10m", "", names)
  names <- gsub("_20m", "", names)
  names <- gsub("_60m", "", names)
  names(b) <- names
  r <- sort(unique(resolution))
  b2 <- vector("list", length = length(r))
  names(b2) <- paste0('r', r, 'm')
  b_resolution <- paste0('r', resolution, 'm')
  for (n in names(b2)) {
    b2[[n]] <- transform_to_multiband(bands = b[which(b_resolution == n)])
  }
  structure(list(
    bands = b2,
    out_dir = out_dir,
    virtual_files = vf
  ),
  class = "clc")
}


#' Get spatial resolutions
#'
#' Returns the spatial resolutions of the multi-band raster that make up the object.
#'
#' @param sr A `clc` object.
#'
#' @return A vector of strings.
#'
#' @family satellite definition
#' @seealso \code{\link{sat_untarzip}}
#'
#' @examples
#'
#' esa <- system.file("extdata", "esa", package = "clc")
#' sr <- clc(dir = esa)
#'
#' r <- sr |>
#'      get_spatial_resolution()
#'
#' @export
get_spatial_resolution <- function(sr)
  UseMethod("get_spatial_resolution")


#' @rdname get_spatial_resolution
#' @export
get_spatial_resolution.clc <- function(sr) {
  names(sr$bands)
}


#' Get band names
#'
#' Returns all names of the multi-band raster that make up the object.
#'
#' We can indicate the name of a certain spatial resolution to obtain only
#' its names.
#'
#' @param sr A `clc` object.
#' @param res A string, spatial resolution.
#'
#' @return A vector of strings.
#'
#' @family satellite definition
#' @seealso \code{\link{sat_untarzip}}
#'
#' @examples
#'
#' esa <- system.file("extdata", "esa", package = "clc")
#' sr <- clc(dir = esa, only_spectral_bands = FALSE)
#' r <- sr |>
#'      get_band_names()
#'
#' @export
get_band_names <- function(sr, res)
  UseMethod("get_band_names")

#' @rdname get_band_names
#' @export
get_band_names.clc <- function(sr, res = NULL) {
  res <- check_spatial_resolution(sr, res)
  b <- NULL
  for (r in res) {
    b <- c(b, names(sr$bands[[r]]))
  }
  sort(unique(b))
}


#' Get band names
#'
#' Returns the band names of the multi-band raster that make up the object.
#'
#' We can indicate the name of a certain spatial resolution to obtain only its
#' band names.
#'
#' @param sr A `clc` object.
#' @param res A string, spatial resolution.
#'
#' @return A vector of strings.
#'
#' @family satellite definition
#' @seealso \code{\link{sat_untarzip}}
#'
#' @examples
#'
#' esa <- system.file("extdata", "esa", package = "clc")
#' sr <- clc(dir = esa, only_spectral_bands = FALSE)
#' r <- sr |>
#'      get_spectral_band_names()
#'
#' @export
get_spectral_band_names <- function(sr, res)
  UseMethod("get_spectral_band_names")


#' @rdname get_spectral_band_names
#' @export
get_spectral_band_names.clc <- function(sr, res = NULL) {
  b <- get_band_names(sr, res)
  sbn <- sat_band_names()
  sort(intersect(b, sbn))
}


################################################################

#' Transforms a list of raster bands in a raster multi-band
#'
#' To generate a multi-band raster, the definition of the vector must be
#' executed in a single instruction, which is why it must be done through
#' meta programming.
#'
#' @param bands A list of raster bands.
#'
#' @return A raster band.
#'
#' @keywords internal
transform_to_multiband <- function(bands) {
  l <- length(bands)
  str <- "c(bands[[1]]"
  if (l > 1) {
    for (i in 2:l) {
      str <- paste0(str, sprintf(", bands[[%d]]", i))
    }
  }
  str <- paste0(str, ")")
  res <- eval(parse(text = str))
  names(res) <- names(bands)
  res
}


#' Find name to files
#'
#' Finds the name associated to a file name in a vector of named patterns.
#'
#' @param files A string vector.
#' @param patterns A string vector of values with names.
#'
#' @return A string vector.
#'
#' @keywords internal
find_name_to_files <- function(files, patterns) {
  names <- names(patterns)
  fn <- names(files)
  for (i in 1:length(patterns)) {
    r <- grepl(patterns[i], files, fixed = TRUE)
    if (sum(r) > 0) {
      fn[which(r)] <- names[i]
    }
  }
  names(files) <- fn
  files
}

#' Check spatial resolution
#'
#' Check if the indicated spatial resolution is one of those available.
#'
#' @param sr A `clc` object.
#' @param res A string, spatial resolution.
#' @param valid_null A boolean, resolution can be null.
#'
#' @return A string, spatial resolution.
#'
#' @keywords internal
check_spatial_resolution <-
  function(sr, res = NULL, valid_null = TRUE) {
    if (!valid_null) {
      stopifnot("A spatial resolution must be indicated." = !is.null(res))
    }
    if (is.null(res)) {
      res <- names(sr$bands)
    } else {
      res <- unique(res)
      for (r in res) {
        if (!(r %in% names(sr$bands))) {
          stop(sprintf("The spatial resolution '%s' is not available.", r))
        }
      }
    }
    res
  }


#' Check band
#'
#' Check if the indicated band is one of those available.
#'
#' @param sr A `clc` object.
#' @param res A string, spatial resolution.
#' @param bands A string, band names.
#'
#' @return boolean.
#'
#' @keywords internal
check_bands <- function(sr, res = NULL, bands = NULL) {
  res_bands <- get_band_names(sr, res)
  if (is.null(bands)) {
    bands <- res_bands
  } else {
    bands <- unique(bands)
    for (b in bands) {
      if (!(b %in% res_bands)) {
        stop(sprintf("The band '%s' is not available at the selected spatial resolutions.", b))
      }
    }
  }
  bands
}
