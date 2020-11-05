
# This script loads in the processed rasters as lists in R and then saves to a file that can be easily loaded
# Idea is to only have to run this once


# Load libraries
library(tidyverse)
library(tiff)



# Read in individual tree info
tree_dat_test <- read_csv('./data/IDTREES_competition_test/task2/testing_ITC_info.csv')
head(tree_dat_test)


# Initialize lists that will hold raster data
hsi_list_full_test <- list()
hsi_list_masked_test <- list()
hsi_list_cropped_test <- list()

ch_list_full_test <- list()
ch_list_masked_test <- list()
ch_list_cropped_test <- list()

rgb_list_full_test <- list()
rgb_list_masked_test <- list()
rgb_list_cropped_test <- list()


x = 1 # Initialize
# Loop through individuals and load in rasters as array, save into list
for(ind in tree_dat_test$indvdID){
  
  if(x %% 5 == 0){
    
    cat("Working on image number: ", x, " ... \n")
  }
  
  # Read in tiff - way faster than Raster, reads in as array
  hsi_tiff_full <- readTIFF(grep(paste0(ind, ".tif$"),
                                 list.files("./data/IDTREES_competition_test/task2/RemoteSensing_processed/HSI_by_species_full/",
                                                                  recursive = TRUE, full.names = TRUE), value = TRUE))
  hsi_tiff_masked <- readTIFF(grep(paste0(ind, ".tif$"),
                                   list.files("./data/IDTREES_competition_test/task2/RemoteSensing_processed/HSI_by_species_masked/",
                                                                       recursive = TRUE, full.names = TRUE), value = TRUE))
  hsi_tiff_cropped <- readTIFF(grep(paste0(ind, ".tif$"), 
                                    list.files("./data/IDTREES_competition_test/task2/RemoteSensing_processed/HSI_by_species_cropped/",
                                                                       recursive = TRUE, full.names = TRUE), value = TRUE))
  
  ch_tiff_full <- readTIFF(grep(paste0(ind, ".tif$"), 
                                list.files("./data/IDTREES_competition_test/task2/RemoteSensing_processed/CHM_by_species_full/",
                                                                       recursive = TRUE, full.names = TRUE), value = TRUE))
  ch_tiff_masked <- readTIFF(grep(paste0(ind, ".tif$"), 
                                  list.files("./data/IDTREES_competition_test/task2/RemoteSensing_processed/CHM_by_species_masked/",
                                                                         recursive = TRUE, full.names = TRUE), value = TRUE))
  ch_tiff_cropped <- readTIFF(grep(paste0(ind, ".tif$"),
                                   list.files("./data/IDTREES_competition_test/task2/RemoteSensing_processed/CHM_by_species_cropped/",
                                                                          recursive = TRUE, full.names = TRUE), value = TRUE))
  
  rgb_tiff_full <- readTIFF(grep(paste0(ind, ".tif$"),
                                 list.files("./data/IDTREES_competition_test/task2/RemoteSensing_processed/RGB_by_species_full/",
                                                                      recursive = TRUE, full.names = TRUE), value = TRUE))
  rgb_tiff_masked <- readTIFF(grep(paste0(ind, ".tif$"),
                                   list.files("./data/IDTREES_competition_test/task2/RemoteSensing_processed/RGB_by_species_masked/",
                                                                        recursive = TRUE, full.names = TRUE), value = TRUE))
  rgb_tiff_cropped <- readTIFF(grep(paste0(ind, ".tif$"),
                                    list.files("./data/IDTREES_competition_test/task2/RemoteSensing_processed/RGB_by_species_cropped/",
                                                                         recursive = TRUE, full.names = TRUE), value = TRUE))
  
  # Save in array
  hsi_list_full_test[[x]] <- hsi_tiff_full
  hsi_list_masked_test[[x]] <- hsi_tiff_masked
  hsi_list_cropped_test[[x]] <- hsi_tiff_cropped
  
  ch_list_full_test[[x]] <- ch_tiff_full
  ch_list_masked_test[[x]] <- ch_tiff_masked
  ch_list_cropped_test[[x]] <- ch_tiff_cropped
  
  rgb_list_full_test[[x]] <- rgb_tiff_full
  rgb_list_masked_test[[x]] <- rgb_tiff_masked
  rgb_list_cropped_test[[x]] <- rgb_tiff_cropped
  
  # Increment counter
  x = x + 1
  
}

# Check on dimensions
dim(hsi_list_full_test[[1]])
dim(hsi_list_masked_test[[1]])
dim(hsi_list_cropped_test[[1]])

dim(ch_list_full_test[[1]])
dim(ch_list_masked_test[[1]])
dim(ch_list_cropped_test[[1]])

dim(rgb_list_full_test[[1]])
dim(rgb_list_masked_test[[1]])
dim(rgb_list_cropped_test[[1]])



# Save to file
save(hsi_list_full_test, file = "./data/IDTREES_competition_test/task2/RemoteSensing_processed/hsi_list_full_test.Rdata")
save(hsi_list_cropped_test, file = "./data/IDTREES_competition_test/task2/RemoteSensing_processed/hsi_list_cropped_test.Rdata")
save(hsi_list_masked_test, file = "./data/IDTREES_competition_test/task2/RemoteSensing_processed/hsi_list_masked_test.Rdata")

save(ch_list_full_test, file = "./data/IDTREES_competition_test/task2/RemoteSensing_processed/ch_list_full_test.Rdata")
save(ch_list_cropped_test, file = "./data/IDTREES_competition_test/task2/RemoteSensing_processed/ch_list_cropped_test.Rdata")
save(ch_list_masked_test, file = "./data/IDTREES_competition_test/task2/RemoteSensing_processed/ch_list_masked_test.Rdata")

save(rgb_list_full_test, file = "./data/IDTREES_competition_test/task2/RemoteSensing_processed/rgb_list_full_test.Rdata")
save(rgb_list_cropped_test, file = "./data/IDTREES_competition_test/task2/RemoteSensing_processed/rgb_list_cropped_test.Rdata")
save(rgb_list_masked_test, file = "./data/IDTREES_competition_test/task2/RemoteSensing_processed/rgb_list_masked_test.Rdata")



