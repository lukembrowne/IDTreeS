library(tidyverse)
library(sf)
library(raster)


crop_type = "out" # out = gets overlapping pixels, 'near' = default, only central pixels


# load file that has individual tree information
tree_dat = read_csv("./data/train/Field/train_data.csv")
tree_dat

# load file that links tree data with remote sensing files
rs_link = read_csv("./data/train/Field/itc_rsFile.csv")
rs_link

rs_link$indvdID %in% tree_dat$indvdID

rs_link$indvdID[!rs_link$indvdID %in% tree_dat$indvdID]


# Find a duplicate rows - where an individual tree crown spans two remote sensing images
# This causes issues with processing the data, and I just remove them to keep things simple
rs_link %>%
  group_by(indvdID) %>%
  tally() %>%
  filter(n > 1)

tree_dat %>%
  group_by(indvdID) %>%
  tally() %>%
  filter(n > 1)


# Turn into wide format with 2 columns for rsfiles
rs_link_wide <- tibble(indvdID = rep("", length(unique(rs_link$indvdID))),
                           rsFile1 = "", 
                           rsFile2 = "")
row = 1
for(ind in unique(tree_dat$indvdID)){
  
  files = rs_link$rsFile[rs_link$indvdID == ind]

  rs_link_wide[row, "indvdID"] <-  ind
  rs_link_wide[row, "rsFile1"] <-  files[1]
  rs_link_wide[row, "rsFile2"] <-  files[2]
  
  row = row + 1
  
}

rs_link_wide


# Drop duplicate rows
# rs_link <- rs_link %>%
#   distinct(indvdID, .keep_all = TRUE)
tree_dat <- tree_dat %>%
  distinct(indvdID, .keep_all = TRUE)

# Join remote sensing file names to individual tree data
tree_dat = left_join(tree_dat, rs_link_wide)
tree_dat$rsFile1 # Should be a column name with file name of relevant remote sensing file

# Write to file
write_csv(tree_dat, "./data/train/Field/tree_data_joined.csv")

# Load in individual tree crown (ITC) bounding boxes, formatted as shapefiles
# Geopandas package lets us load this data in as a dataframe
# Separate files for each site, so we need to combine them into one dataset
itc_bb_mlbs = read_sf("./data/train/ITC/train_MLBS.shp")
itc_bb_osbs = read_sf("./data/train/ITC/train_OSBS.shp")

plot(itc_bb_mlbs)
plot(itc_bb_osbs)

# Join itcs together
itc_bb = rbind(itc_bb_mlbs, itc_bb_osbs)
itc_bb


# Loop through individual trees and plot and crop remote sensing images
# for (ind in tree_dat$indvdID){

# Or if running on the cluster using job array read in which individual to focus on
# Read in command line arguments
args = commandArgs(trailingOnly=TRUE)
job_array_ID = as.numeric(args[1])   #  first argument is row number of individual
ind = tree_dat$indvdID[job_array_ID]

  # Print status
  cat("Working on individual: ", ind, " \n")
  
  
  # Load in raster data
  
    # Hyperspectral raster - need to read in as rasterStack
    hsi = stack(paste0("./data/train/RemoteSensing/HSI/",  tree_dat$rsFile1[tree_dat$indvdID == ind]))

    # Canopy height
    ch = stack(paste0("./data/train/RemoteSensing/CHM/",  tree_dat$rsFile1[tree_dat$indvdID == ind]))

    # RGB
    rgb = stack(paste0("./data/train/RemoteSensing/RGB/",  tree_dat$rsFile1[tree_dat$indvdID == ind]))
    
    # If there is another remote sensing file, load and merge with first file
    if(!is.na(tree_dat$rsFile2[tree_dat$indvdID == ind])){
      
      # Hyperspectral raster - need to read in as rasterStack
      hsi2 = stack(paste0("./data/train/RemoteSensing/HSI/",  tree_dat$rsFile2[tree_dat$indvdID == ind]))
      hsi = raster::merge(hsi, hsi2)
      
      # Canopy height
      ch2 = stack(paste0("./data/train/RemoteSensing/CHM/",  tree_dat$rsFile2[tree_dat$indvdID == ind]))
      ch = raster::merge(ch, ch2)
      
      # RGB
      rgb2 = stack(paste0("./data/train/RemoteSensing/RGB/",  tree_dat$rsFile2[tree_dat$indvdID == ind]))
      rgb = raster::merge(rgb, rgb2)
      
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
  
  
  # Make paths to folder that will hold cropped images, with sub directories by species
  
  # Get species code
  spp_code = tree_dat$taxonID[tree_dat$indvdID == ind]
  

  # Hsi
  hsi_full_spp_dir = paste0("./data/train/RemoteSensing_processed/HSI_by_species_full/", spp_code)
  hsi_cropped_spp_dir = paste0("./data/train/RemoteSensing_processed/HSI_by_species_cropped/", spp_code)
  hsi_masked_spp_dir = paste0("./data/train/RemoteSensing_processed/HSI_by_species_masked/", spp_code)
  
  dir.create(hsi_full_spp_dir, recursive = TRUE)
  dir.create(hsi_masked_spp_dir, recursive = TRUE)
  dir.create(hsi_cropped_spp_dir, recursive = TRUE)
  
  # CHM
  ch_full_spp_dir = paste0("./data/train/RemoteSensing_processed/CHM_by_species_full/", spp_code)
  ch_cropped_spp_dir = paste0("./data/train/RemoteSensing_processed/CHM_by_species_cropped/", spp_code)
  ch_masked_spp_dir = paste0("./data/train/RemoteSensing_processed/CHM_by_species_masked/", spp_code)
  
  dir.create(ch_full_spp_dir, recursive = TRUE)
  dir.create(ch_masked_spp_dir, recursive = TRUE)
  dir.create(ch_cropped_spp_dir, recursive = TRUE)
  
  
  # RGB
  rgb_full_spp_dir = paste0("./data/train/RemoteSensing_processed/RGB_by_species_full/", spp_code)
  rgb_cropped_spp_dir = paste0("./data/train/RemoteSensing_processed/RGB_by_species_cropped/", spp_code)
  rgb_masked_spp_dir = paste0("./data/train/RemoteSensing_processed/RGB_by_species_masked/", spp_code)
  
  dir.create(rgb_full_spp_dir, recursive = TRUE)
  dir.create(rgb_masked_spp_dir, recursive = TRUE)
  dir.create(rgb_cropped_spp_dir, recursive = TRUE)
  
  
  # Write rasters to file
  writeRaster(hsi, filename = paste0(hsi_full_spp_dir, "/", spp_code, "_", ind, ".tif"),
              overwrite = TRUE)
  writeRaster(hsi_crop, filename = paste0(hsi_cropped_spp_dir, "/", spp_code, "_", ind, ".tif"),
              overwrite = TRUE)
  writeRaster(hsi_mask, filename = paste0(hsi_masked_spp_dir, "/", spp_code, "_", ind, ".tif"),
              overwrite = TRUE)
  
  writeRaster(ch, filename = paste0(ch_full_spp_dir, "/", spp_code, "_", ind, ".tif"),
              overwrite = TRUE)
  writeRaster(ch_crop, filename = paste0(ch_cropped_spp_dir, "/", spp_code, "_", ind, ".tif"),
              overwrite = TRUE)
  writeRaster(ch_mask, filename = paste0(ch_masked_spp_dir, "/", spp_code, "_", ind, ".tif"),
              overwrite = TRUE)
  
  writeRaster(rgb, filename = paste0(rgb_full_spp_dir, "/", spp_code, "_", ind, ".tif"),
              overwrite = TRUE)
  writeRaster(rgb_crop, filename = paste0(rgb_cropped_spp_dir, "/", spp_code, "_", ind, ".tif"),
              overwrite = TRUE)
  writeRaster(rgb_mask, filename = paste0(rgb_masked_spp_dir, "/", spp_code, "_", ind, ".tif"),
              overwrite = TRUE)
  
   bitmap(paste0("./plots/", spp_code, "_", ind, ".png" ), width = 15, height = 15, res = 300, units = "cm")
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

# } # End loop through indidividuals


   
   
   # Lagniappe code
   
 # Installing units package on cluster
   # https://github.com/r-quantities/units/issues/1
   # UDUNITS2_LIBS=/home/lmb242/udunits/local/lib
   # UDUNITS2_INCLUDE=/home/lmb242/udunits/local/include
   # LD_LIBRARY_PATH=/home/lmb242/udunits/local/lib:$LD_LIBRARY_PATH
   # install.packages("units", configure.args="--with-udunits2-lib=/home/lmb242/udunits/local/lib --with-udunits2-include=/home/lmb242/udunits/local/include")
   # 
   
   
