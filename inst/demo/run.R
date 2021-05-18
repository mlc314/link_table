library(lt)

# limit number of files in each directory
files_per_directory <- 100

# get number of cores and task_id from environment
cores <- ifelse(Sys.getenv('SGE_HGR_fthread') != '', as.integer(Sys.getenv('SGE_HGR_fthread')), 1)
message(sprintf("Using %i core(s) for parallelization.", cores))
task_id <- ifelse(Sys.getenv('SGE_TASK_ID') != '', as.integer(Sys.getenv('SGE_TASK_ID')), NA)
message(sprintf("Task ID is %s.", task_id))

#' Print to shell with delimiter for parsing.
#' 
#' Helper function for capturing shell output, as the current 
#' IHME shell scripts print out all of the code.
#' 
#' @param msg (character) message to send to shell for capture
print_to_shell <- function(msg) {
  delimiter <- Sys.getenv('r_output_delimiter')
  
  if (delimiter == '') {
    warning("No environment variable for delimiter found.")
  }
  
  print(paste0(delimiter, msg))
}

#' Retrieve filename based on task id
#' 
#' Due to filesystem limitations, the temporary directory is split
#' into multiple subdirectories. This function allows for consistent
#' retrieval of filenames based on task id.
#' 
#' @param base_dir (character) base directory to for file
#' @param npartitions (integer) number of total partitions
#' @param task_id (integer) task id of current file
#' @param filename (character) file name
#'  
#' @return (character) path to file
get_sub_lt_file <- function(base_dir, npartitions, task_id, filename) {
  task_dir <- floor(task_id / files_per_directory) + 1
  
  file.path(base_dir, task_dir, filename)
}

#' Get base shapefile path.
#' 
#' The script needs this for ensuring shapefile directory integrity.
run_lt_shapedir <- function() {
  sf_dir <- get_admin_shape_dir(version = '')
  print_to_shell(sf_dir)
}

#' Set the empty world raster resolution as per aggregation factor
#'
#'@param aggFact (integer) factor used for aggregating raster
set_world_raster_resolution <- function(aggFact) {
  aggFact <- as.integer(aggFact)
  world_raster <- empty_world_raster(raster_agg_factor = aggFact)
  return(world_raster)
}

#' Set the resolution as per aggregation factor and name link_table and id_raster
#'
#'@param aggFact (integer) factor used for aggregating raster 
set_lt_name <- function(aggFact) {
  aggFact <- as.integer(aggFact)
  lt_env$LT_FILENAME <- sprintf('lbd_%ikm_link.rds', aggFact*5)
  lt_env$IDR_FILENAME <- sprintf('lbd_%ikm_id_raster.rds', aggFact*5)
}
  
#' Run partitioning step.
#' 
#' @param N (integer) number of partitions
#' @param shapedir (character) name/version of shapefile directory
#' @param tmpdir (character) absolute path of temporary directory where job is running
run_lt_partition <- function(N, shapedir, tmpdir, aggFact) {
  polys <- sf::st_read(get_admin_shapefile(admin_level = 2, version = shapedir), 
                       stringsAsFactors = FALSE)
  world_raster <- set_world_raster_resolution(aggFact)
  partition <- partition_lt_grid(N, polys, world_raster)
  grids <- partition$grids
  poly_ints <- partition$poly_ints
  npartitions <- length(grids)
  ndirectories <- floor(npartitions / files_per_directory) + 1
  
  for (i in 1:ndirectories) {
    dir.create(file.path(tmpdir, i))
  }
  for (i in 1:npartitions) {
    save_file <- get_sub_lt_file(tmpdir, npartitions, i, sprintf('grid_%s.rds', i))
    saveRDS(grids[i], save_file, compress = FALSE)
    save_file <- get_sub_lt_file(tmpdir, npartitions, i, sprintf('polys_%s.rds', i))
    saveRDS(polys[poly_ints[[i]],], save_file, compress = FALSE)
  }
}
  
#' Run build step.
#' 
#' @param shapedir (character) name/version of shapefile directory
#' @param npartitions (integer) number of actual partitions from partitioning step
run_lt_build <- function(tmpdir, npartitions, aggFact) {
  polys <- readRDS(get_sub_lt_file(tmpdir, npartitions, task_id, sprintf('polys_%s.rds', task_id)))
  poly_grid <- readRDS(get_sub_lt_file(tmpdir, npartitions, task_id, sprintf('grid_%s.rds', task_id)))
  world_raster <- set_world_raster_resolution(aggFact)
  world_raster[] <- 1:length(world_raster)
  
  lt <- build_grid_lt(polys, poly_grid, world_raster)
  
  save_file <- get_sub_lt_file(tmpdir, npartitions, task_id, sprintf('link_table_%s.rds', task_id))
  message(sprintf("Saving temporary file: %s", save_file))
  saveRDS(lt, save_file, compress = FALSE)
}
  
#' Combine sub jobs into final link table.
#' 
#' @param tmpdir (character) absolute path of temporary directory where job is running
#' @param npartitions (integer) number of actual partitions from partitioning step
run_lt_finalize <- function(tmpdir, npartitions, aggFact) {
  # read in partitioned tables
  file_paths <- lapply(1:npartitions, function(i) {
    get_sub_lt_file(tmpdir, npartitions, i, sprintf('link_table_%s.rds', i))
  })
  
  registerDoParallel(cores = cores)
  lt <- foreach(i = 1:length(file_paths), .final = rbindlist) %dopar%
    readRDS(file_paths[[i]])

  lt <- finalize_lt(lt)
  lt <- assign_pixel_owners(lt)
  
  set_lt_name(aggFact)
  save_file <- file.path(tmpdir, lt_env$LT_FILENAME)
  saveRDS(lt, save_file, compress = FALSE)
}

#' Ensure link table was built properly.
#' 
#' Email the user of the results.
#' 
#' @param N (integer) number of partitions
#' @param npartitions (integer) number of actual partitions from partitioning step
#' @param tmpdir (character) absolute path of temporary directory where job is running
#' @param shapedir (character) name/version of shapefile directory
run_lt_check <- function(npartitions, tmpdir, shapedir, aggFact) {
  set_lt_name(aggFact)
  # if link table exists, no need to check further
  if (file.exists(file.path(tmpdir, lt_env$LT_FILENAME))) {
    # save link table in shapefile dir
    lt <- readRDS(file.path(tmpdir, lt_env$LT_FILENAME))
    idr <- set_world_raster_resolution(aggFact)
    raster::values(idr) <- 1:length(idr)
    
    lt_file_dir <- file.path(get_admin_shape_dir(version = shapedir), "link_table")
    if (!dir.exists(lt_file_dir)) {
      dir.create(lt_file_dir)
    }

    lt_file <- file.path(lt_file_dir, lt_env$LT_FILENAME)
    idr_file <- file.path(lt_file_dir, lt_env$IDR_FILENAME)
    
    saveRDS(lt, lt_file, compress = FALSE)
    saveRDS(idr, idr_file, compress = FALSE) 
    
    msg <- sprintf("Link table job SUCCEEDED. Table saved to %s.", file.path(shapedir, "link_table", lt_env$LT_FILENAME))
  } else {
    # check if all sub link tables were created properly
    missing <- c()
    for (i in 1:npartitions) {
      if (!file.exists(get_sub_lt_file(tmpdir, npartitions, i, sprintf('link_table_%s.rds', i)))) {
        missing <- c(missing, i)
      }
    }
    
    if (length(missing) > 0) {
      msg <- sprintf("The following build tasks failed: %s.",
                     paste(sort(as.integer(missing)), collapse = ', '))
    }
    
    # partition error
    else if (length(list.dirs(tmpdir)) - 1 != floor(npartitions / files_per_directory) + 1) {
      msg <- "Partitioning failed."
    }
    
    # check if sub link tables were combined correctly
    else if (!file.exists(file.path(tmpdir, lt_env$LT_FILENAME))) {
      msg <- "Link table is missing from temporary directory. Combination task failed."
    }
    
    else {
      msg <- "Something went wrong. Link table is missing."
    }
    
    msg <- sprintf("Link table job FAILED. REASON: %s", msg)
  }
  
  print_to_shell(msg)
}

#' Ensure link table accurately represents the shapefile it was created from.
#' 
#' Email the user of the results.
#' 
#' @param shapedir (character) name/version of shapefile directory
run_lt_validate <- function(shapedir, aggFact) {
  set_lt_name(aggFact)
  lt <- readRDS(file.path(get_admin_shape_dir(version = shapedir), "link_table", lt_env$LT_FILENAME))
  polys <- sf::st_read(get_admin_shapefile(admin_level = 2, version = shapedir), stringsAsFactors = FALSE)
  
  msgs <- c()
  
  if (!validate_borders(lt, polys)) {
    msgs <- c(msgs, "Borders do not match.")
  }
  
  if (!validate_area(lt, polys)) {
    msgs <- c(msgs, "Areas are incorrect.")
  }
  
  if (!validate_coverage(lt, polys)) {
    msgs <- c(msgs, "Link table is missing some regions.")
  }
  
  if (!validate_duplicates(lt)) {
    msgs <- c(msgs, "Link table contains duplicate entries.")
  }
  
  if (is.null(msgs)) {
    print_to_shell('Link table validated successfully!')
  } else {
    print_to_shell(sprintf('Link Table validation failed: %s', paste(msgs, collapse = ', ')))
  }
}

# Collect command-line arguments and run function based on the
# value of the 'command' option.
# eg. command == 'partition' -> call run_lt_partition(args)
parser <- ArgumentParser()
parser$add_argument('-c', '--command',
                    help = "Link table command to run.")
parser$add_argument('-d', '--tmpdir',
                    help = "Temporary directory to read/write.")
parser$add_argument('-s', '--shapedir',
                    help = "Shapefile Directory.")
parser$add_argument('-n', '--N', type = 'integer',
                    help = "Number of partitions.")
parser$add_argument('-p', '--npartitions',
                    help = "number of actual partitions.")
parser$add_argument('-a', '--aggFact',
                    help = "Aggregation factor.")
parser$add_argument('--no-save', action = 'store_true', default = NULL,
                    help = "for SingR - ignore.")
args <- parser$parse_args()

# remove NULL values
args <- Filter(Negate(is.null), args)

do.call(get(sprintf('run_lt_%s', args$command)), args[names(args) != 'command'])
