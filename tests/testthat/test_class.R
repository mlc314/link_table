context("Link Table Class")

test_that("class setup", {
  ltn <<- LinkTable$from_version('current')
  mlt <<- ltn[NAME_0 == 'Mali']
  mali_raster <<- mlt$to_raster()
})

test_that("verify_raster_coverage", {
  missing <- mlt$verify_raster_coverage(mali_raster)$missing
  expect_equal(nrow(missing), 0)
  
  # null out a value, ensuring it is missing
  first <- which(!is.na(mali_raster[]))[1]
  mali_raster[first] <- NA
  
  missing <- mlt$verify_raster_coverage(mali_raster)$missing
  expect_equal(nrow(missing), 1)
  
  # add extra value
  first_na <- which(is.na(mali_raster[]))[1]
  mali_raster[first_na] <- 1
  
  extra <- mlt$verify_raster_coverage(mali_raster)$extra
  expect_equal(length(extra), 1)
})

test_that("unique is actually unique", {
  expect_gte(anyDuplicated(mlt$lt, by = c('pixel_id', 'ADM2_CODE')), 0)
  expect_equal(anyDuplicated(mlt$unique(), by = 'pixel_id'), 0)
})

test_that("pixel ownership cascades properly", {
  test_lt <- data.table(pixel_id = c(1, 1, 1, 1),
                        ADM0_CODE = c(10, 20, 20, 20), 
                        ADM1_CODE = c(101, 201, 201, 202), 
                        ADM2_CODE = c(1012, 2011, 2012, 2021),
                        area_fraction = c(0.49, 0.13, 0.14, 0.24),
                        n = c(4, 4, 4, 4))
  
  test_idr <- raster::raster(ncol = 1, nrow = 1)
  test_idr[] <- 1
  
  test_ltn <- LinkTable$new(test_lt, test_idr)
  
  expect_true(all(unlist(test_ltn$pixel_owners) == c(1, 20, 201, 2012)))
})
