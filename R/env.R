#' @export
lt_env <- new.env()

# id columns - these are meant to be unique within the link table
# and are used frequently in the code
lt_env$ID_COLS <- c('pixel_id', 'ADM2_CODE')

# directory / files
lt_env$LT_BASE_SHAPEFILE_DIR <- Sys.getenv('LT_BASE_SHAPEFILE_DIR',
                                           '/path/to/11_geospatial/admin_shapefiles')
lt_env$LT_FILENAME <- 'lbd_standard_link.rds'
lt_env$IDR_FILENAME <- 'lbd_standard_id_raster.rds'