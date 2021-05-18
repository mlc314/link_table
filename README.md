# Link Table Generation in Parallel

## NOTE: This code is meant for viewing only. A number of file paths have been removed so the code will not run.

### Overview

The link table is a tool used to assign metadata and area coverage from polygons in a shapefile to cells in a raster grid, via a spatial overlay. The resulting link table is a table where each row represents a pixel-admin unit overlay (e.g. a pixel containing 3 admin units would have 3 rows) with the associated metadata from the shapefile. Each cell in the raster is assigned a pixel_id which links the base raster (see `empty_world_raster.R`) to the link table.  

This package generates the link table in parallel through partitioning. Most of the parameters are set in `inst/demo/launch.sh`.
Currently two resolutions(5kmx5km and 10kmx10km) link tables are generated. 

The following pattern is followed:

1. Partition the shapefile by ADM0 geometry area.
2. Build a link table for each partition.
3. Combine the partitioned link tables together.
4. Cleanup/ensure script ran successfully.
5. Validate link table.

The first 2 steps run in parallel for each partition, then steps 3-5 are run once all partitions have run successfully. 

### Use
See run scripts in inst/demo

    .launch.sh -d <directory> -n <number of partitions> -q <queue> -p <project>

Where `directory` is a child directory within the base admin shapefile directory.

Example:

    ./launch.sh -d 2019_08_01 -n 5000

will generate link tables for `/path/to/admin_shapefiles/2019_08_01` and place the new link tables and id rasters 
at `/path/to/admin_shapefiles/2019_08_01/link_table`:

1. lbd_5km_link.rds
2. lbd_5km_id_raster.rds
3. lbd_10km_link.rds
4. lbd_10km_id_raster.rds

`cleanup.sh` will create symlinks in the admin shape directory as in the following, if the standard files are absent. This will maintain backward compatibility for previous function calls.

* lbd_standard_link.rds -> link_table/lbd_5km_link.rds
* lbd_standard_id_raster.rds -> link_table/lbd_5km_id_raster.rds

With 5000 partitions, you can expect the runtime to be approximately 30 minutes, when the cluster is not busy.
