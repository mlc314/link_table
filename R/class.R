#' R6 class for link table
#' 
#' @description 
#' description
#' 
#' @details
#' details 
#' 
#' @import R6
#'
#' @export

LinkTable <- R6::R6Class("LinkTable",
  public = list(
    #' @field lt link table
    lt = NULL,
    
    #' @field id_raster id raster
    id_raster = NULL,
    
    #' @field pixel_owners pixel owner data.table
    pixel_owners = NULL,
    
    #' @description
    #' We need the link table and id raster for reference during
    #' raster operations.
    #' 
    #' @param lt data.table - link table
    #' @param id_raster id raster
    initialize = function(lt, id_raster) {
      self$lt <- lt
      self$id_raster <- id_raster
      self$pixel_owners <- self$get_pixel_ownership()
    },
    
    #' @description
    #' Advance through admin levels, geting the pixel owner
    #' then pruning. This ensures, for example, when getting the
    #' ADM1 pixel owner that the ADM1 is within the ADM0 owner. 
    #' Ownership is based on nested aggregated area fraction.
    #' 
    #' @return (data.table) pixel with owner at each level
    get_pixel_ownership = function() {
      agg_lt <- self$lt
      
      # only check owner in pixels with multiple regions
      agg_lt <- agg_lt[n > 1]
      whole_lt <- self$lt[n == 1]
      owner_fields <- c()
      
      # loop over each admin level
      for (i in 0:2) {
        group_field <- sprintf('ADM%s_CODE', i)
        owner_field <- sprintf('OWNER_%s', i)
        owner_fields <- c(owner_fields, owner_field)
        
        # aggregate link table area fraction on current admin level
        owner_lt <- agg_lt[, list(total = sum(area_fraction)), by = c('pixel_id', group_field)]
        
        # sort by aggregated total then
        # unique keeps only the first element by id
        # so we are keeping only the pixel id with the max total
        setorder(owner_lt, pixel_id, -total)
        owner_lt <- unique(owner_lt, by = 'pixel_id')
        
        # clean up columns
        owner_lt[[owner_field]] <- owner_lt[[group_field]]
        owner_lt[[group_field]] <- NULL
        owner_lt$total <- NULL
        
        # merge new owner column into link table
        agg_lt <- merge(agg_lt,
                        owner_lt, 
                        by.x = c('pixel_id', group_field), 
                        by.y = c('pixel_id', owner_field))
        
        # assign owner column
        agg_lt[[owner_field]] <- agg_lt[[group_field]]
        whole_lt[[owner_field]] <- whole_lt[[group_field]]
      }
      
      # keep only owner fields
      rbind(whole_lt, agg_lt)[, c('pixel_id', owner_fields), with = FALSE]
    },

    #' @description
    #' This function assumes that there are "OWNER_"
    #' fields corresponding to each ADM level. If any of the 
    #' assigned owners are missing from the link table,
    #' the pixel is dropped.
    #' 
    #' @return (data.table) with one row per pixel
    unique = function() {
      # get 'OWNER_' columns
      owner_cols <- colnames(self$pixel_owners)[grepl('OWNER', colnames(self$pixel_owners))]
      
      # loop over each admin level
      code_cols <- vapply(0:2, function(i) sprintf('ADM%s_CODE', i), character(1))
      
      # merge owner columns into link table
      unique_lt <- merge(self$lt, 
                         self$pixel_owners, 
                         by.x = c('pixel_id', code_cols),
                         by.y = c('pixel_id', owner_cols))
      
      unique_lt
    },
    
    #' @description
    #' Find missing pixels in a raster based 
    #' on values in the link table.
    #' 
    #' @param verify_raster raster to compare with link table
    #' 
    #' @return (list) missing: link table with entries for missing pixels,
    #'                extra: vector of extra pixel_ids that are not in the link table
    #'                
    #' @import raster
    verify_raster_coverage = function(verify_raster) {
      # find range of pixel id values
      id_range <- raster::values(raster::crop(self$id_raster, verify_raster))
      
      # use unique values
      unique_lt <- self$unique()
      
      # subset pixel range to those within link table
      valid_indexes <- id_range[id_range %in% unique_lt$pixel_id]

      # find which pixels are filled (non-NA) in the raster
      filled <- which(!is.na(raster::values(verify_raster)))
      
      # get pixel ids of non-NA pixels in the raster
      filled_indexes <- id_range[filled]
      
      # find pixels missing from raster
      missing_indexes <- setdiff(valid_indexes, filled_indexes)
      
      # find extra pixels
      extra_indexes <- setdiff(filled_indexes, valid_indexes)

      list(missing = unique_lt[pixel_id %in% missing_indexes],
           extra = extra_indexes)
    },
    
    #' @description
    #' Link table rows are converted to raster cells
    #' based on pixel_id. Before conversion, only unique
    #' pixels are kept based on nested greatest area ownership.
    #' 
    #' @param template_raster raster to crop output to
    #' @param field field to populate raster cells
    #' @param trim_result trim output raster?
    #' 
    #' @return (raster::raster)
    #' 
    #' @import raster
    to_raster = function(template_raster = NULL, 
                         field = 'ADM0_CODE',
                         trim_result = TRUE) {
      
      # ensure template raster conforms with id raster
      if (!is.null(template_raster) && 
          !raster::compareRaster(self$id_raster, template_raster, 
                                 extent = FALSE, rowcol = FALSE, res = TRUE)) {
          stop("Error: template raster does not match id raster. One of
               the following is inconsistent: projection, res, crs, or rotation")
      }
      
      lt <- self$unique()
      lt_raster <- self$id_raster
      
      # convert field class to value usable in raster
      if (is.character(lt[[field]])) {
        lt[[field]] <- as.factor(lt[[field]])
      } else if (class(lt[[field]]) == 'units') {
        lt[[field]] <- as.numeric(lt[[field]])
      }
      
      # assign values to raster
      raster::values(lt_raster) <- NA
      raster::values(lt_raster)[lt$pixel_id] <- lt[[field]]
      
      # crop to template raster
      if (!is.null(template_raster)) {
        lt_raster <- raster::crop(lt_raster, template_raster)
      }
      
      if (trim_result) lt_raster <- raster::trim(lt_raster)
      
      lt_raster
    },
    
    #' @description
    #' Each value in the raster is converted to 1.
    #' 
    #' @return (raster)
    to_mask = function() {
      mask_raster <- self$id_raster
      mask_raster[] <- NA
      mask_raster[self$lt$pixel_id] <- 1
      
      mask_raster
    },
    
    #' @description
    #' Overrides print() on this object
    print = function() {
      print("LinkTable R6 Object:")
      print(self$lt)
    }
  )
)

#' Instantiate LinkTable from shapefile version.
#' 
#' Factory function for LinkTable from shapefile version.
#' id raster is also inferred from shapefile directory.
#' 
#' @param version (character) shapefile version
#' 
#' @return (LinkTable) from shapefile version
#' 
#' @export
LinkTable$from_version <- function(version) {
  lt_file <- file.path(lt_env$LT_BASE_SHAPEFILE_DIR, 
                       version, 
                       lt_env$LT_FILENAME)
  
  idr_file <- file.path(lt_env$LT_BASE_SHAPEFILE_DIR, 
                        version, 
                        lt_env$IDR_FILENAME)
  
  if (file.exists(lt_file) && file.exists(idr_file)) {
    lt <- readRDS(lt_file)
    id_raster <- readRDS(idr_file)
    
    return(LinkTable$new(lt, id_raster))
  } else {
    stop(sprintf("Version: '%s' is not a valid shapefile version or the link table has not been generated there yet.", 
                 version))
  }
}

#' Instantiate LinkTable from files.
#' 
#' Factory function for LinkTable from file.
#' Requires link table and id raster files.
#' 
#' @param lt_file (character) link table absolute filepath
#' @param idr_file (character) id raster absolute filepath
#' 
#' @return (LinkTable) from file
#' 
#' @export
LinkTable$from_file <- function(lt_file, idr_file) {
  lt <- readRDS(lt_file)
  id_raster <- readRDS(idr_file)
  
  LinkTable$new(lt, id_raster)
}

#' Allow data.table functions over LinkTable.
#' 
#' @param lt_object (LinkTable) link table to be treated as data.table
#' @param ... arguments to be passed to data.table bracket function
#' 
#' @return (LinkTable) new LinkTable object with data.table modifications
#' 
#' @export
`[.LinkTable` <- function(lt_object, ...) {
  new_object <- lt_object$clone()
  new_object$lt <- new_object$lt[...] # do data.table stuff
  new_object # but return LinkTable
}
