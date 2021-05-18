context("Setup")

test_that("setup world raster and polys", {
  world_raster <<- raster::raster(nrow = 4320, ncol = 8640,
                                 xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                                 vals = NA)
  
  world_raster[] <<- 1:length(world_raster)
  
  # TODO: consider not relying on j drive
  polys <<- sf::st_read('/path/to/admin_shapefiles/current/lbd_standard_admin_2.shp', 
                       stringsAsFactors = FALSE)
})

context("Partition Tests")

test_that("find_Ns produces correct factors", {
  # test with specific raster
  test_raster <- raster::raster(nrow = 10, ncol = 10)
  test_Ns <- find_Ns(test_raster)
  expect_equal(sort(test_Ns), c(1, 2, 4, 5, 10, 20, 25, 50, 100))
  
  # general test on random raster size
  rx <- sample(100, 1)
  ry <- sample(100, 1)
  test_raster <- raster::raster(nrow = ry, ncol = rx)
  test_Ns <- find_Ns(test_raster)
  expect_true(all(c(1, rx, ry, rx * ry) %in% test_Ns))
})

test_that("factorize_raster_dim gives proper dimensions", {
  inout <- list(
    list(400, c(20, 20)),
    list(600, c(30, 20))
  )
  
  for (l in inout) {
    input = l[[1]]
    output = l[[2]]
    expect_equal(factorize_raster_dim(input, world_raster), output)
  }
  
  # can't be factored
  expect_error(factorize_raster_dim(1000, world_raster))
  
  # test with y > x
  test_raster <- raster::raster(ncol = 20, nrow = 40)
  expect_equal(factorize_raster_dim(20, test_raster), c(4, 5))
})

# grid aligns with whole raster cells (pixels are not divided by grid)
test_that("partitions align", {
  world_partition <- partition_lt_grid(100, polys, world_raster, remove_empty = FALSE)
  
  comparisons <- lapply(world_partition$grids, function(grid) {
    grid_bbox <- st_bbox(grid)
    grid_extent <- extent(grid_bbox[1], grid_bbox[3], grid_bbox[2], grid_bbox[4])
    snapin <- raster::crop(world_raster, grid_extent, snap = 'in')
    snapout <- raster::crop(world_raster, grid_extent, snap = 'out')
    raster::compareRaster(snapin, snapout, res = T, orig = T, values = T)
  })
  
  expect_true(all(unlist(comparisons)))
})

context("Link Table Generation Example")

test_that("Link table setup", {
  peru_polys <<- polys[polys$NAME_0 == 'Peru',]
  peru_raster <<- raster::crop(world_raster, peru_polys, snap = 'out')
  
  N <<- 4
  
  partition <<- partition_lt_grid(N, peru_polys, peru_raster, remove_empty = FALSE)
  peru_grid_1 <<- partition$grids[1]
  peru_polys_1 <<- sf::st_intersection(peru_polys, peru_grid_1)
  
  peru_lt_1 <<- build_grid_lt(peru_polys, peru_grid_1, peru_raster)
  peru_lt_1_final <<- finalize_lt(peru_lt_1)
})

test_that("partition_lt_grid", {
  # bounding box of raster and grid are the same
  expect_equal(sf::st_bbox(peru_raster), sf::st_bbox(partition$grids))
  
  # number of grids is lte N
  expect_lte(length(partition$grids), N)
  
  # make sure all polygons are in the grid
  grid_union <- sf::st_union(partition$grids)
  expect_true(all(lapply(sf::st_within(peru_polys, grid_union), length) > 0))
  
  # and that no grids are empty 
  expect_true(all(lapply(sf::st_intersects(partition$grids, peru_polys), length) > 0))
})

test_that("build_lt_grid", {
  # make sure pixel ids are covered
  expect_true(all(peru_lt_1$pixel_id %in% peru_raster[]))
  
  # misaligned grid and polygon should error
  expect_error(build_grid_lt(tail(peru_polys, 1), peru_grid_1, peru_raster))
})

test_that("finalize_lt", {
  # all necessary columns were added
  expect_true(all(c('area_fraction', 'total_area') %in% colnames(peru_lt_1_final)))
  
  # no area ratios surpass 1
  # some do by a very tiny fraction and expect_equal does not work on vectors
  expect_true(!any(round(peru_lt_1_final$total_area, 10) > 1))
  
  # sum of area fractions is 1 for each pixel
  total_fraction <- peru_lt_1_final[, list(total_fraction = sum(area_fraction)), by = 'pixel_id']$total_fraction
  expect_equal(total_fraction, rep(1, length(total_fraction)))
})

test_that("link table validates", {
  expect_true(validate_borders(peru_lt_1_final, peru_polys_1))
  expect_true(validate_area(peru_lt_1_final, peru_polys_1))
  expect_true(validate_coverage(peru_lt_1_final, peru_polys_1))
  expect_true(validate_duplicates(peru_lt_1_final))
})
