#' Validate link table borders. 
#' 
#' Get borders from shapefile and see if
#' they line up with shared pixels in the link table. Some discrepancy is 
#' expected due to the fact that adm units that share pixels in the link
#' may not directly border. ie. They are separated by a distance less than
#' the pixel polygon size.
#' 
#' To mitigate this, we first compare the link table with the polygon
#' borders. The link table should not be missing any borders from the
#' shapefile. Then we make sure the extra borders from the link table
#' are those that have water in the pixel.
#'
#' @param lt (date.table) link table
#' @param polys (sf) shapes used to generate link table
#' 
#' @return (logical)
#' 
#' @import sf
#' @import data.table
#' 
#' @export
validate_borders <- function(lt, polys) {
  # get list of borders from shapefile
  ints <- sf::st_intersects(polys)

  # convert intersection to ADM0-ADM0 mapping
  poly_neighbors <- data.table::as.data.table(ints)
  poly_neighbors[] <- lapply(poly_neighbors, function(i) polys$NAME_0[i])
  colnames(poly_neighbors) <- c('x', 'y')
  poly_neighbors <- poly_neighbors[x > y]
  
  # link table neighbors
  lt_neighbors <- merge(lt, lt, by = 'pixel_id')
  lt_neighbors <- lt_neighbors[, c('NAME_0.x', 'NAME_0.y')]
  colnames(lt_neighbors) <- c('x', 'y')
  lt_neighbors <- lt_neighbors[x > y]
  
  # set difference
  missing_from_lt <- data.table::fsetdiff(poly_neighbors, lt_neighbors)
  extra_in_lt <- data.table::fsetdiff(lt_neighbors, poly_neighbors)

  # get water border from lt
  lt_water_neighbors <- merge(lt, lt, by = 'pixel_id')
  lt_water_neighbors <- lt_water_neighbors[n.x > 1 & area_fraction.x < 1]
  lt_water_neighbors <- lt_water_neighbors[, c('NAME_0.x', 'NAME_0.y')]
  colnames(lt_water_neighbors) <- c('x', 'y')
  lt_water_neighbors <- lt_water_neighbors[x > y]
  
  # extra link table borders that cannot be accounted for due to shared water borders
  extra_not_touch_water <- data.table::fsetdiff(extra_in_lt, lt_water_neighbors)

  nrow(missing_from_lt) == 0 && nrow(extra_not_touch_water) == 0
}

#' Validate link table areas. 
#' 
#' See if computed areas in the link table
#' align with areas from the shapefile. Allow a threshold for rounding
#' errors/simple features discrepancies.
#' 
#' There is some approximation in area computations, so we need a threshold
#' to allow area loss/gain.
#'
#' @param lt (date.table) link table
#' @param polys (sf) shapes used to generate link table
#' @param fail_threshold (float) largest allowable ratio of area difference to area
#'
#' @return (logical)
#' 
#' @import data.table
#' 
#' @export
validate_area <- function(lt, polys, fail_threshold = 1e-5) {
  polys$area <- GEOS_area(polys, byid = TRUE)

  # sum link table areas
  lt_areas <- lt[, list(area = sum(end_area)), by = ADM2_CODE]
  
  # merge link table areas onto polygons
  m_areas <- merge(polys, lt_areas, 'ADM2_CODE')
  
  # convert to data.table
  m_areas$geometry <- NULL
  data.table::setDT(m_areas)
  m_areas$area.y <- as.numeric(m_areas$area.y)
  
  # get difference of lt and polygon areas
  m_areas$diff <- m_areas$area.y - m_areas$area.x 
  m_areas$diff_ratio <- apply(m_areas, 1, function(row) {
    areax <- as.numeric(row['area.x'])
    diff <- as.numeric(row['diff'])
    diff / areax
  })
  
  max(abs(m_areas$diff_ratio)) < fail_threshold
}

#'Ensure link table covers all admin regions
#'
#' @param lt (date.table) link table
#' @param polys (sf) shapes used to generate link table
#' 
#' @return (logical)
#' 
#' @export
validate_coverage <- function(lt, polys) {
  setequal(unique(polys$ADM2_CODE), unique(lt$ADM2_CODE))
}

#'Ensure link table has no duplicates
#'
#' @param lt (date.table) link table
#' 
#' @return (logical) no duplicates?
#' 
#' @export
validate_duplicates <- function(lt) {
  anyDuplicated(lt, by = lt_env$ID_COLS) == 0
}
