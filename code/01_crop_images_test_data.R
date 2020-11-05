library(tidyverse)
library(sf)
library(raster)


crop_type = "out" # out = gets overlapping pixels, 'near' = default, only central pixels


# Load in individual tree crown (ITC) bounding boxes, formatted as shapefiles
# Separate files for each site, so we need to combine them into one dataset
itc_bb_mlbs = read_sf("./data/IDTREES_competition_test/task2/ITC/test_MLBS.shp")
itc_bb_osbs = read_sf("./data/IDTREES_competition_test/task2/ITC/test_OSBS.shp")
itc_bb_tall = read_sf("./data/IDTREES_competition_test/task2/ITC/test_TALL.shp")

itc_bb_mlbs # 71 features
itc_bb_osbs # 318 features
itc_bb_tall # 196 features

# plot(itc_bb_mlbs)
# plot(itc_bb_osbs)
# plot(itc_bb_tall)

# Transform TALl to same coordinate system  
itc_bb_tall = st_transform(itc_bb_tall, crs = 32617)

# Join itcs together
itc_bb = rbind(itc_bb_mlbs, itc_bb_osbs, itc_bb_tall)
itc_bb # 585 features

## Output information as CSV
# write_csv(itc_bb, "./data/IDTREES_competition_test/task2/testing_ITC_info.csv")


# Look for duplicates individuals
sum(table(itc_bb$indvdID) > 1) # No duplicates


# If running on the cluster using job array read in which individual to focus on
# Read in command line arguments
args = commandArgs(trailingOnly=TRUE)
job_array_ID = as.numeric(args[1])   #  first argument is row number of individual

ind = itc_bb$indvdID[job_array_ID]

# Print status
cat("Working on individual: ", ind, " \n")


# Load in raster data

# Hyperspectral raster - need to read in as rasterStack
hsi = stack(paste0("./data/IDTREES_competition_test/task2/RemoteSensing/HSI/",  itc_bb$plotID[itc_bb$indvdID == ind]))

# Canopy height
ch = stack(paste0("./data/IDTREES_competition_test/task2/RemoteSensing/CHM/",  itc_bb$plotID[itc_bb$indvdID == ind]))

# RGB
rgb = stack(paste0("./data/IDTREES_competition_test/task2/RemoteSensing/RGB/",  itc_bb$plotID[itc_bb$indvdID == ind]))

# Reproject rasters for TALL sites
if(grepl("TALL", ind)){
  hsi = projectRaster(hsi, crs = crs(itc_bb))
  ch = projectRaster(ch, crs = crs(itc_bb))
  rgb = projectRaster(rgb, crs = crs(itc_bb))
}


# Get bounding box of individual
itc_sub <- itc_bb[itc_bb$indvdID == ind,]

# Crop rasters
hsi_crop = crop(hsi, itc_sub, snap = crop_type)
ch_crop = crop(ch, itc_sub, snap = crop_type)
rgb_crop = crop(rgb, itc_sub, snap = crop_type)

# Mask rasters
hsi_mask = mask(hsi, itc_sub)
ch_mask = mask(ch, itc_sub)
rgb_mask = mask(rgb, itc_sub)


# Make paths to folder that will hold cropped images - all rasters in a single directory

# Hsi
hsi_full_spp_dir ="./data/IDTREES_competition_test/task2/RemoteSensing_processed/HSI_by_species_full/"
hsi_cropped_spp_dir = "./data/IDTREES_competition_test/task2/RemoteSensing_processed/HSI_by_species_cropped/"
hsi_masked_spp_dir = "./data/IDTREES_competition_test/task2/RemoteSensing_processed/HSI_by_species_masked/"

dir.create(hsi_full_spp_dir, recursive = TRUE)
dir.create(hsi_masked_spp_dir, recursive = TRUE)
dir.create(hsi_cropped_spp_dir, recursive = TRUE)

# CHM
ch_full_spp_dir = "./data/IDTREES_competition_test/task2/RemoteSensing_processed/CHM_by_species_full/"
ch_cropped_spp_dir = "./data/IDTREES_competition_test/task2/RemoteSensing_processed/CHM_by_species_cropped/"
ch_masked_spp_dir = "./data/IDTREES_competition_test/task2/RemoteSensing_processed/CHM_by_species_masked/"

dir.create(ch_full_spp_dir, recursive = TRUE)
dir.create(ch_masked_spp_dir, recursive = TRUE)
dir.create(ch_cropped_spp_dir, recursive = TRUE)


# RGB
rgb_full_spp_dir = "./data/IDTREES_competition_test/task2/RemoteSensing_processed/RGB_by_species_full/"
rgb_cropped_spp_dir = "./data/IDTREES_competition_test/task2/RemoteSensing_processed/RGB_by_species_cropped/"
rgb_masked_spp_dir = "./data/IDTREES_competition_test/task2/RemoteSensing_processed/RGB_by_species_masked/"

dir.create(rgb_full_spp_dir, recursive = TRUE)
dir.create(rgb_masked_spp_dir, recursive = TRUE)
dir.create(rgb_cropped_spp_dir, recursive = TRUE)


# Write rasters to file
writeRaster(hsi, filename = paste0(hsi_full_spp_dir, "/", ind, ".tif"),
            overwrite = TRUE)
writeRaster(hsi_crop, filename = paste0(hsi_cropped_spp_dir, "/", ind, ".tif"),
            overwrite = TRUE)
writeRaster(hsi_mask, filename = paste0(hsi_masked_spp_dir, "/", ind, ".tif"),
            overwrite = TRUE)

writeRaster(ch, filename = paste0(ch_full_spp_dir, "/", ind, ".tif"),
            overwrite = TRUE)
writeRaster(ch_crop, filename = paste0(ch_cropped_spp_dir, "/", ind, ".tif"),
            overwrite = TRUE)
writeRaster(ch_mask, filename = paste0(ch_masked_spp_dir, "/", ind, ".tif"),
            overwrite = TRUE)

writeRaster(rgb, filename = paste0(rgb_full_spp_dir, "/", ind, ".tif"),
            overwrite = TRUE)
writeRaster(rgb_crop, filename = paste0(rgb_cropped_spp_dir, "/", ind, ".tif"),
            overwrite = TRUE)
writeRaster(rgb_mask, filename = paste0(rgb_masked_spp_dir, "/", ind, ".tif"),
            overwrite = TRUE)

bitmap(paste0("./plots/testing/", ind, ".png" ), width = 15, height = 15, res = 300, units = "cm")
  par(mfrow = c(3, 3))
  
  # First row
  plot(hsi[[1]])
  title("Hyperspectral")
  plot(itc_sub, add = TRUE, col = rgb(.5, .5, .5, .5))
  plot(ch)
  title("Canopy height")
  plot(itc_sub, add = TRUE, col = rgb(.5, .5, .5, .5))
  plotRGB(rgb, margins = TRUE)
  
  # Second row
  plot(hsi_crop[[1]])
  title("Hyperspectral - cropped")
  plot(ch_crop)
  title("Canopy height - cropped")
  plotRGB(rgb_crop, margins = TRUE)
  
  # Third row
  plot(hsi_mask[[1]])
  title("Hyperspectral - masked")
  plot(ch_mask)
  title("Canopy height - masked")
  plotRGB(rgb_mask, margins = TRUE)

dev.off()


# Lagniappe code

# Installing units package on cluster
# https://github.com/r-quantities/units/issues/1
# UDUNITS2_LIBS=/home/lmb242/udunits/local/lib
# UDUNITS2_INCLUDE=/home/lmb242/udunits/local/include
# LD_LIBRARY_PATH=/home/lmb242/udunits/local/lib:$LD_LIBRARY_PATH
# install.packages("units", configure.args="--with-udunits2-lib=/home/lmb242/udunits/local/lib --with-udunits2-include=/home/lmb242/udunits/local/include")
# 


