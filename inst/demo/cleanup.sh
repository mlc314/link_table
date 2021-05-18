#!/bin/bash

if [[ -z $base_shapefile_dir || -z $shape_dir || -z $temp_dir ]]; then
    echo "Missing environment variables from launch script."
    exit 1
fi

if [[ $aggFact == 1 ]]
then
  LT_FILENAME="lbd_5km_link.rds"
  # Create symlinks at admin_shape_dir for backward compatibility
  ln -s "link_table/link_5km_link.rds" "$base_shapefile_dir/$shape_dir/lbd_standard_link.rds"
  ln -s "link_table/link_5km_id_raster.rds" "$base_shapefile_dir/$shape_dir/lbd_standard_id_raster.rds"
else
  LT_FILENAME="lbd_10km_link.rds"
fi

# if files are the same, remove temporary directory
if cmp -s "$temp_dir/$LT_FILENAME" "$base_shapefile_dir/$shape_dir/link_table/$LT_FILENAME";
then
   rm -r "$temp_dir"
fi
