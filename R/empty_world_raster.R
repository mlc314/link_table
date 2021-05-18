#' @title Create raster representing world
#'
#' @description Creates a raster file consistent with LBD's world raster.
#' 
#' @param raster_agg_factor int. Default 1. 1 corresponds to 5km raster resolution, 2 corresponds to 10km resolution. No other resolutions are supported at this time. 
#' 
#' @export
empty_world_raster <- function(raster_agg_factor = 1, ...) {

  if ("whole_world" %in% names(list(...))) {
    message <- paste0("empty_world_raster no longer takes a whole_world argument - ",
                      "whole world rasters are now produced by default. The old whole_world = F ",
                      "option which produced a stage1+2 raster is no longer available")
    stop(message)
  }
  
  if(!(raster_agg_factor %in% c(1,2))) {
    stop("Only raster_agg_factor == 1 or 2 supported at this time")
  }
  
  result <- raster::raster(
    nrow = 4320/raster_agg_factor, ncol = 8640/raster_agg_factor,
    crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
    xmn = -180, xmx = 180, ymn = -90, ymx = 90,
    vals = NA
  )

  return(result)
}
