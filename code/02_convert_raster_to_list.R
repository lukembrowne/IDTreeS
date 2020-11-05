
# This script loads in the processed rasters as lists in R and then saves to a file that can be easily loaded
# Idea is to only have to run this once


# Load libraries
library(tidyverse)
library(tiff)



# Read in individual tree info
tree_dat <- read_csv('./data/train/Field/tree_data_joined.csv')


# Initialize lists that will hold raster data
hsi_list_full <- list()
hsi_list_masked <- list()
hsi_list_cropped <- list()

ch_list_full <- list()
ch_list_masked <- list()
ch_list_cropped <- list()

rgb_list_full <- list()
rgb_list_masked <- list()
rgb_list_cropped <- list()


x = 1 # Initialize
# Loop through individuals and load in rasters as array, save into list
for(ind in tree_dat$indvdID){
  
  if(x %% 5 == 0){
    
    cat("Working on image number: ", x, " ... \n")
  }
  
  # Read in tiff - way faster than Raster, reads in as array
  hsi_tiff_full <- readTIFF(grep(paste0("_", ind, ".tif$"), list.files("./data/train/RemoteSensing_processed/HSI_by_species_full/",
                                                                  recursive = TRUE, full.names = TRUE), value = TRUE))
  hsi_tiff_masked <- readTIFF(grep(paste0("_", ind, ".tif$"), list.files("./data/train/RemoteSensing_processed/HSI_by_species_masked/",
                                                                       recursive = TRUE, full.names = TRUE), value = TRUE))
  hsi_tiff_cropped <- readTIFF(grep(paste0("_", ind, ".tif$"), list.files("./data/train/RemoteSensing_processed/HSI_by_species_cropped/",
                                                                       recursive = TRUE, full.names = TRUE), value = TRUE))
  
  ch_tiff_full <- readTIFF(grep(paste0("_", ind, ".tif$"), list.files("./data/train/RemoteSensing_processed/CHM_by_species_full/",
                                                                       recursive = TRUE, full.names = TRUE), value = TRUE))
  ch_tiff_masked <- readTIFF(grep(paste0("_", ind, ".tif$"), list.files("./data/train/RemoteSensing_processed/CHM_by_species_masked/",
                                                                         recursive = TRUE, full.names = TRUE), value = TRUE))
  ch_tiff_cropped <- readTIFF(grep(paste0("_", ind, ".tif$"), list.files("./data/train/RemoteSensing_processed/CHM_by_species_cropped/",
                                                                          recursive = TRUE, full.names = TRUE), value = TRUE))
  
  rgb_tiff_full <- readTIFF(grep(paste0("_", ind, ".tif$"), list.files("./data/train/RemoteSensing_processed/RGB_by_species_full/",
                                                                      recursive = TRUE, full.names = TRUE), value = TRUE))
  rgb_tiff_masked <- readTIFF(grep(paste0("_", ind, ".tif$"), list.files("./data/train/RemoteSensing_processed/RGB_by_species_masked/",
                                                                        recursive = TRUE, full.names = TRUE), value = TRUE))
  rgb_tiff_cropped <- readTIFF(grep(paste0("_", ind, ".tif$"), list.files("./data/train/RemoteSensing_processed/RGB_by_species_cropped/",
                                                                         recursive = TRUE, full.names = TRUE), value = TRUE))
  
  # Save in array
  hsi_list_full[[x]] <- hsi_tiff_full
  hsi_list_masked[[x]] <- hsi_tiff_masked
  hsi_list_cropped[[x]] <- hsi_tiff_cropped
  
  ch_list_full[[x]] <- ch_tiff_full
  ch_list_masked[[x]] <- ch_tiff_masked
  ch_list_cropped[[x]] <- ch_tiff_cropped
  
  rgb_list_full[[x]] <- rgb_tiff_full
  rgb_list_masked[[x]] <- rgb_tiff_masked
  rgb_list_cropped[[x]] <- rgb_tiff_cropped
  
  # Increment counter
  x = x + 1
  
}

# Check on dimensions
dim(hsi_list_full[[1]])
dim(hsi_list_masked[[1]])
dim(hsi_list_cropped[[1]])

dim(ch_list_full[[1]])
dim(ch_list_masked[[1]])
dim(ch_list_cropped[[1]])

dim(rgb_list_full[[1]])
dim(rgb_list_masked[[1]])
dim(rgb_list_cropped[[1]])



# Save to file
save(hsi_list_full, file = "./data/train/RemoteSensing_processed/hsi_list_full.Rdata")
save(hsi_list_cropped, file = "./data/train/RemoteSensing_processed/hsi_list_cropped.Rdata")
save(hsi_list_masked, file = "./data/train/RemoteSensing_processed/hsi_list_masked.Rdata")

save(ch_list_full, file = "./data/train/RemoteSensing_processed/ch_list_full.Rdata")
save(ch_list_cropped, file = "./data/train/RemoteSensing_processed/ch_list_cropped.Rdata")
save(ch_list_masked, file = "./data/train/RemoteSensing_processed/ch_list_masked.Rdata")

save(rgb_list_full, file = "./data/train/RemoteSensing_processed/rgb_list_full.Rdata")
save(rgb_list_cropped, file = "./data/train/RemoteSensing_processed/rgb_list_cropped.Rdata")
save(rgb_list_masked, file = "./data/train/RemoteSensing_processed/rgb_list_masked.Rdata")



