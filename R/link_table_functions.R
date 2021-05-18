#' Find common factors of N and raster dimensions.
#'
#' This allows clean partitioning of a raster N times. Will
#' return NA if no common factorization is possible.
#'
#' This function will infer a preference of x to y
#' depending on the dimensions of the raster. So if
#' x > y, x will get the larger factor.
#'
#' @param N (integer) target number to factor
#' @param factor_raster (raster::raster) raster for dimensions to factor
#'
#' @return (numeric) common factors for N and factor_raster
#'
#' @import data.table
factorize_raster_dim <- function(N, factor_raster) {
  # start with all possible factors
  factors <- data.table(x = 1:N)
  factors[, y := N/x]

  # get only integer
  factors <- factors[y %% 1 == 0]

  # make sure it is also a factor of raster dimensions
  factors <- factors[ncol(factor_raster) %% x == 0 & nrow(factor_raster) %% y == 0]

  # assign dimension preference
  weight <- ifelse(ncol(factor_raster) > nrow(factor_raster), -1, 1)
  factors[, diff := abs(x - y)]
  factors[x >= y, diff := diff + eval(weight)]
  factors[y > x, diff := diff - eval(weight)]

  # get most similarly-sized x and y
  data.table::setorder(factors, diff)
  factors$diff <- NULL

  dims <- as.numeric(factors[1])

  if (any(is.na(dims))) {
    stop(sprintf("Cannot partition into %s regions: does not divide into raster evenly.", N))
  }

  dims
}

#' Find all possible partition values over a raster.
#'
#' @param factor_raster (raster::raster) raster for dimensions to factor
#'
#' @return (numeric) possible partition values
#'
#' @import data.table
find_Ns <- function(factor_raster) {
  # all possible values
  nposs <- 1:ncell(factor_raster)

  # posible x values
  x <- nposs[ncol(factor_raster) %% nposs == 0]

  # possible y values
  y <- nposs[nrow(factor_raster) %% nposs == 0]

  # cartesian product of x and y
  d <- data.table::CJ(x = x, y = y)

  # multiply x * y to get N values
  d[, N := x * y]

  unique(d$N)
}

#' Compute area using GEOS (rgeos)
#'
#' sf::st_area does some approximation in its computation.
#' rgeos::gArea is more consistent.
#'
#' @param polys (sf) sf object to compute areas over
#' @param ... remaining arguments for rgeos::gArea()
#'
#' @return (numeric) areas
#'
#' @import rgeos
GEOS_area <- function(polys, ...) {
  rgeos::gArea(sf::as_Spatial(polys), ...)
}

#' Clean and extract geometries if necessary.
#'
#' In some scenarios (such as polygons being split by intersection),
#' GEOMETRYCOLLECTIONs are returned. They do not play well with
#' some libraries, so we need to extract them into polygons.
#'
#' We can also safely remove shapes with no area, such as points
#' and lines.
#'
#' @param sfo (sf) sf object with geometries to extract
#'
#' @return (sf) sf object with geometries extracted
#'
#' @import sf
clean_geometries <- function(sfo) {
  area_geometry_types <- c('POLYGON', 'MULTIPOLYGON')

  # split geometry collections, this defaults to polygon/multipolygon
  # and drops everything else
  if ('GEOMETRYCOLLECTION' %in% sf::st_geometry_type(sfo)) {
    sfo <- st_collection_extract(sfo)
  }

  # points and lines have no area, remove
  sfo[sf::st_geometry_type(sfo) %in% area_geometry_types, ]
}

#' Partition polys into grid.
#'
#' @param N (integer) number of pieces to break raster into
#' @param polys (sf) polys to divide
#' @param shape_raster (raster::raster) raster to align grid with
#' @param remove_empty (boolean) remove empty grid pieces?
#'
#' @return (list) grid tiles and their polygon intersections
#'
#' @import sf
#'
#' @export
partition_lt_grid <- function(N, polys, shape_raster, remove_empty = TRUE) {
  # make sure raster is partitionable into N pieces
  possible_N <- find_Ns(shape_raster)

  # N can't be partitioned... find closest partitonable N
  if (!(N %in% possible_N)) {
    old_N <- N
    N <- possible_N[which.min(abs(possible_N - N))]
    warning(sprintf("%s does not factor into the raster evenly. Will use %s instead.", old_N, N))
  }

  # get partition
  partition_dim <- factorize_raster_dim(N, shape_raster)

  # divide bbox of raster into grid
  raster_grid <- sf::st_make_grid(sf::st_as_sfc(sf::st_bbox(shape_raster)),
                                  n = partition_dim)

  # get grid intersections
  ints <- sf::st_intersects(raster_grid, polys)

  # remove grid sections that do not intersect with any polygon.
  if (remove_empty) {
    nonempty <- lapply(ints, length) > 0
    ints <- ints[nonempty]
    raster_grid <- raster_grid[nonempty]
  }

  list(grids = raster_grid, poly_ints = ints)
}

#' Build link table for grid tile.
#'
#' @param polys (sf) shapes to build link table over
#' @param poly_grid (sf) polygon to build pixel polygons in
#' @param shape_raster (raster::raster) raster that contains shapes for polygonizing
#'
#' @return (data.table) link table for grid tile or NULL if no applicable geometries remain
#'
#' @import raster
#' @import sf
#' @import spex
#'
#' @export
build_grid_lt <- function(polys, poly_grid, shape_raster) {
  # get grid square we'll be using
  region <- sf::st_sf(list(geometry = poly_grid))

  # subset to polys that intersect
  poly_ints <- sf::st_intersects(region, polys)
  cutout <- sf::st_intersection(region, polys[poly_ints[[1]], ])

  # no resulting intersection
  if (nrow(cutout) == 0) {
    stop("Given shape and grid do not align. Exiting.")
  }

  # clean cutout geometries
  cutout <- clean_geometries(cutout)

  # return NULL instead of empty data.table to avoid conflicting columns and
  # types in rbindlist downstream
  if (nrow(cutout) == 0) {
    message('No geometry with area found. Returning NULL.')
    return(NULL)
  }

  message(sprintf("Link table includes the following ADM0: %s",
                  paste(unique(cutout$NAME_0), collapse = ", ")))

  # crop world raster to cutout for pixel generation
  crop_raster <- raster::crop(shape_raster, cutout, snap = 'out')

  # this fix is for polygonize - it fails when you only
  # have one value in the raster, so we add an extra pixel
  # and remove it later
  if (ncell(crop_raster) == 1) {
    crop_extent <- extent(crop_raster)
    crop_extent[1] <- crop_extent[1] - res(crop_raster)[1]
    crop_raster <- raster::extend(crop_raster, crop_extent)
    crop_raster[is.na(crop_raster[])] <- -9999
  }

  # get pixel polygons
  pixels <- spex::polygonize(crop_raster)
  # part of polygonize fix
  if (ncell(crop_raster) == 1) pixels <- pixels[pixels$layer != -9999, ]
  pixels$start_area <- GEOS_area(pixels, byid = TRUE)

  ## compute intersection/link table
  # use only pixels that intersect with shapes
  pixel_ints <- unique(unlist(sf::st_intersects(cutout, pixels)))
  lt <- sf::st_intersection(cutout, pixels[pixel_ints,])

  # clean link table geometries
  lt <- clean_geometries(lt)

  lt$end_area <- GEOS_area(lt, byid = TRUE)
  lt$geometry <- NULL
  lt <- data.table(lt)
  lt[, c('ID', 'pixel_id') := layer]
  lt$layer <- NULL

  # aggregate end area for duplicates
  if (any(duplicated(lt, by = lt_env$ID_COLS))) {
    lt[, end_area := sum(end_area), by = eval(lt_env$ID_COLS)]
    lt <- unique(lt, by = lt_env$ID_COLS)
  }

  # sometimes rgeos and sf disagree on areas enough
  # that rgeos returns an area of 0
  lt <- lt[end_area > 0]

  lt
}

#' Finalize link table generation.
#'
#' Get area fractions, total_area, etc.
#'
#' @param lt (data.table) link table to finish
#'
#' @return (data.table) final link table
#'
#' @import data.table
#'
#' @export
finalize_lt <- function(lt) {
  # get area fractions
  lt[, area_fraction := end_area/start_area]

  # get grouping information (total area and n)
  lt[, n := .N, by = pixel_id]
  lt[, total_area := sum(area_fraction), by = pixel_id]
  lt$area_fraction <- as.numeric(lt$area_fraction)
  lt$total_area <- as.numeric(lt$total_area)

  ## fix area fractions w/ water
  # if there is only one admin area assigned and area fractions is less than 1, set area fraction to 1
  lt[n == 1 & area_fraction < 1, area_fraction := 1]

  # recompute area for pixels in multiple admin units and water proportionally
  lt[n > 1 & total_area < 1, area_fraction := area_fraction / total_area]

  # setkey for performance improvements
  data.table::setkey(lt, pixel_id)

  lt
}
