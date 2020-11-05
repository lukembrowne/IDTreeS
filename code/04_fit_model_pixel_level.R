
# Load libraries
library(tidyverse)
library(keras)
library(caret)
library(xgboost)


# Generate a params file for grid search ----------------------------------

params <- expand_grid(threshold_other = c(3, 5), # Group species with less than ### images as 'Other'
                      min_canopy_height = c(1, 3), # Filter out pixels below and equal to this canopy height
                      min_pixel_count = c(0), # Filter out individuals with less than this number of pixels
                      testing_split =  .7, # Fraction in testing set vs. validation set, use 1.0 to not use cross validation
                      pca_sd_thresh = c(3, 5), # how many SD away from average to filter by in PCA?
                      pca_bands_filter = c(50), # How many PCs to do filtering on?
                      pc_components_model = c(25, 50, 369), # Which PCs to include in model? Starts at 1 up to this number
                      transform_bands_to_pca = c(TRUE), # Use raw HSI bands or transformed by PCA?
                      resample_min = c(0),
                      
                      # Xgboost parameters
                      xgb_nfolds = c(5),
                      xgb_eta =  c(.1, .3, .5, .9),
                      xgb_nrounds = 5000,
                      xgb_max_depth =  c(4, 7, 10, 15),
                      xgb_min_child_weight = c(1, 4, 7),
                      xgb_subsample = c(.33, .66, 1),
                      xgb_colsample_bytree = c(.5, 1)
)
                      
# params

dim(params)

## Subset to just a single parameter combination
params <- params %>%
  sample_n(1)

cat("Parameter list... \n\n")
as.data.frame(params)


# Set seed
set.seed(42)


## Plot results of grid search

  grid_out <- read_csv("./grid_search_out.csv",
                       guess_max = 50000,
                       col_names = c(colnames(params),
                                     "train_merror_mean",
                                     "test_merror_mean",
                                     "best_ntreelimit", "best_iteration",
                                     "accuracy_valid_set", "accuracy_MLBS", "accuracy_OSBS")
                       # col_names = c(colnames(params))
  )

  dim(grid_out)

  grid_out$id <- 1:nrow(grid_out)

  grid_out %>%
    arrange(test_merror_mean) %>%
    as.data.frame(.)

  grid_out %>%
    filter(resample_min == 0) %>%
    arrange(test_merror_mean) %>%
    as.data.frame(.)

  grid_out %>%
    arrange(desc(test_merror_mean)) %>%
    as.data.frame(.)

  grid_out_factor <- grid_out
  grid_out_factor <- dplyr::select(grid_out_factor, accuracy_valid_set, everything()) %>%
    as.data.frame()
  for(col in 2:ncol(grid_out_factor)){
    grid_out_factor[, col] <- factor(grid_out_factor[, col])
  }
  str(grid_out_factor)

  lm1 <- lm(accuracy_valid_set ~ threshold_other + min_pixel_count + pca_sd_thresh + pc_components_model + transform_bands_to_pca + resample_min + xgb_eta + xgb_max_depth + xgb_min_child_weight + xgb_subsample + xgb_colsample_bytree, data = grid_out_factor)
  summary(lm1)
  #visreg::visreg(lm1, partial = FALSE)


  # Set best run as params
   params <- grid_out[which.max(grid_out$accuracy_valid_set),]
   params <- grid_out[grid_out$id==986,] # 62% accuracy but underpredict other


  # Best run without resampling
#
  grid_sub <- grid_out %>%
    filter(resample_min == 500) 
#
#   params <- grid_out[grid_sub$id[which.min(grid_sub$test_merror_mean)], ]


# Working with cropped hyperspectral data - separating out each pixel in tree crown

# Load in list data 
load("./data/train/RemoteSensing_processed/hsi_list_cropped.Rdata") # Hyperspectral data
load("./data/train/RemoteSensing_processed/ch_list_cropped.Rdata") # Canopy height data

# Read in individual tree info
tree_dat_all <- read_csv('./data/train/Field/tree_data_joined.csv')



#  Remove dead trees? -----------------------------------------------------

  
  # table(tree_dat_all$plantStatus)
  # 
  # dead_remove <- which(tree_dat_all$plantStatus %in% c("Dead, broken bole", "Standing dead"))
  # length(dead_remove)
  # 
  # tree_dat_all <- tree_dat_all[-dead_remove,]
  # ch_list_cropped <- ch_list_cropped[-dead_remove]
  # hsi_list_cropped <- hsi_list_cropped[-dead_remove]
  # 
  # dim(tree_dat_all)
  # length(ch_list_cropped)
  # length(hsi_list_cropped)
  # 
  

# Give names to list ------------------------------------------------------

names(hsi_list_cropped) <- tree_dat_all$indvdID
names(ch_list_cropped) <- tree_dat_all$indvdID



# Look at species composition per site ------------------------------------
sp_comp <- read_csv("./data/train/Field/train_data.csv")
table(sp_comp$siteID, sp_comp$taxonID)


sp_comp %>%
  group_by(siteID, taxonID) %>%
  tally() 


# Group species with less than ### images as 'Other' ------------------------

other_species = tree_dat_all %>%
  group_by(taxonID) %>%
  tally() %>%
  filter(n < params$threshold_other) %>%
  pull(taxonID)


tree_dat_all$taxonID[tree_dat_all$taxonID %in% other_species] <- 'Other'
table(tree_dat_all$taxonID)




# Subset to a single site? -------------------------------------------------

tree_dat <- tree_dat_all

# %>%
  # filter(siteID %in% site) 



# Use some images from the other site as 'other species? ------------------

# images_per_species <- 5
# 
# tree_dat <- bind_rows(tree_dat,
#                       tree_dat_all %>%
#                         filter(siteID != site) %>%
#                         filter(!(taxonID %in% tree_dat$taxonID)) %>%
#                         group_by(taxonID) %>%
#                         sample_n(images_per_species, replace = TRUE) %>%
#                         ungroup() %>%
#                         mutate(taxonID = 'Other')
#                       )
# 
# table(tree_dat$siteID, tree_dat$taxonID)


# Separate out into individual level pixels, long format ------------------

  initial = TRUE
  for(ind in tree_dat$indvdID){
    
    # cat("Working on: ", ind, " ... \n")
    
    index = which(ind == names(hsi_list_cropped))
    

    # If only one pixel
    if(nrow(hsi_list_cropped[[index]][,, 1, drop = F]) == 1 & ncol(hsi_list_cropped[[index]][,, 1, drop = F]) == 1 ){
  
      cat("Only one pixel... \n")
      out_temp <- as_tibble(matrix(apply(hsi_list_cropped[[index]], c(3), function(x) c(x)), nrow = 1)) %>%
        mutate(pixel = 1:nrow(.),
               indvdID = ind,
               ch = c(ch_list_cropped[[index]]))
      
    } else {
  
    # Convert to rows are pixels and columns are bands
    out_temp <- as_tibble(apply(hsi_list_cropped[[index]], c(3), function(x) c(x))) %>%
                mutate(pixel = 1:nrow(.),
                       indvdID = ind,
                       ch = c(ch_list_cropped[[index]]))
    
    }
    
    
    # hsi_list_cropped[[index]][ ,, 34]
    # ch_list_cropped[[index]]
    # 
    # apply(hsi_list_cropped[[index]], c(3), function(x) c(x))[, 34]
    # apply(ch_list_cropped[[index]], c(2), function(x) c(x))
    # c(ch_list_cropped[[index]])
    
    if(initial == TRUE){
      out = out_temp
      initial = FALSE
    } else {
      out = bind_rows(out, out_temp)
    }
    
    if("value" %in% colnames(out)){
      stop()
    }
    
  }
  
  # head(out)
  # dim(out)
  # table(out$pixel)
  # colnames(out)
  
  # Check for missing data - shouldn't be any
  # any(apply(out, MARGIN = 2, function(x) sum(is.na(x))) > 0) # If true, means there is missing data
  

  # Remove pixels with negative values
  neg_values <- apply(dplyr::select(out, -c(indvdID, pixel)), 1, function(x) any(x < 0))
  # neg_values
  sum(neg_values)
  
  out <- out[!neg_values, ]
  
  
  # Remove pixels with canopy height below threshold
  low_ch <- out$ch <= params$min_canopy_height
  sum(low_ch) # Total pixels with 0 CH
  
  out <- out[!low_ch, ]
  
  # Remove canopy_height column
  out$ch <- NULL
  
  # Add species ID
  out2 <- left_join(out, dplyr::select(tree_dat, indvdID, taxonID))
  
  

# Remove individuals with low pixel count? --------------------------------

  # Calculate pixels per individual
  # out2 %>%
  #   group_by(indvdID) %>%
  #   tally() %>%
  #   arrange(n) %>%
  #   print(n = 50)
  # 
  
  low_pix <- out2 %>%
    group_by(indvdID) %>%
    tally() %>%
    filter(n < params$min_pixel_count) %>%
    pull(indvdID)
  
  cat("Filtering ", length(low_pix), " individuals because of pixel count below ", params$min_pixel_count, " ... \n")
  
  tree_dat <- tree_dat %>%
    filter(!(indvdID %in% low_pix))
  
  out <- out[!(out$indvdID %in% low_pix), ]
  out2 <- out2[!(out2$indvdID %in% low_pix), ]
  
  

#  Reshape tree_dat to match pixel level ----------------------------------

  tree_dat_pixel <- left_join(dplyr::select(out, indvdID, pixel), tree_dat)
  dim(tree_dat_pixel)
  dim(out2)
  
  dplyr::select(out, indvdID, pixel) %>%
    group_by(indvdID) %>%
    summarise(max_pix = max(pixel))
  
  
  dplyr::select(tree_dat_pixel, indvdID, pixel) %>%
    group_by(indvdID) %>%
    summarise(max_pix = max(pixel))
  
  
  # make sure individuals are in the same row order
  tree_dat_pixel <- tree_dat_pixel[match(out2$indvdID, tree_dat_pixel$indvdID),]
  all(tree_dat_pixel$indvdID == out2$indvdID)
  


# Plot spectra by species -------------------------------------------------
  
  hsi_df_long <- out2 %>%
    pivot_longer(-c(taxonID, indvdID, pixel), names_to = "wavelength", values_to = "reflectance")
  head(hsi_df_long)

  hsi_df_long$wavelength <- as.numeric(str_remove(hsi_df_long$wavelength, pattern = "V"))

  # Averaged by species
  hsi_df_long_mean <- hsi_df_long %>%
    group_by(taxonID, wavelength) %>%
    summarise_all(mean)

  ggplot(hsi_df_long_mean, aes(wavelength, reflectance, color = taxonID)) +
    geom_line() + theme_bw()

  
  
# Plot canopy height by species -------------------------------------------

  
  # ggplot(out2, aes(x = taxonID, y = ch, fill = taxonID)) + 
  #   geom_boxplot() + geom_jitter(alpha = 0.2) + theme_bw()
  # 
  
  

# Explore dead trees ------------------------------------------------------
    
      
    # table(tree_dat$plantStatus)  
    #   
    # tree_dat[grepl("Dead", tree_dat$plantStatus), ]
    #   
    #   
    # tree_dat[grepl("dead", tree_dat$plantStatus), ]
    # 
    # # Spectra of dead trees vs live treeds
    # 
    # status_df_long <- left_join(hsi_df_long, tree_dat %>% select(indvdID, plantStatus))
    # 
    # # Average by species and status
    # # Averaged by species
    # status_df_long_mean <- status_df_long %>%
    #   group_by(taxonID, plantStatus, wavelength) %>%
    #   summarise_all(mean)
    # 
    # # Individual
    # # ggplot(status_df_long, aes(wavelength, reflectance, color = plantStatus, group = indvdID)) + 
    # #   geom_line(alpha = .2) + theme_bw() + facet_wrap(~taxonID)
    # 
    # # Average
    # ggplot(status_df_long_mean, aes(wavelength, reflectance, color = plantStatus)) + 
    #   geom_line() + theme_bw() + facet_wrap(~taxonID)
    # 
    # status_df_long %>%
    #   distinct(indvdID, .keep_all = TRUE) %>%
    #   group_by(taxonID, plantStatus) %>%
    #   tally() %>%
    #   pivot_wider(., names_from = plantStatus, values_from = n) %>%
    #   data.frame()
    
  

# Explore shaded trees ----------------------------------------------------

  # shadow_df_long <- out2 %>%
  #        dplyr::select(-ch) %>%
  #         pivot_longer(-c(taxonID, indvdID, pixel), names_to = "wavelength", values_to = "reflectance") %>%
  #        left_join(., tree_dat %>% select(indvdID, canopyPosition))
  # 
  # shadow_df_long$wavelength <- as.numeric(str_remove(shadow_df_long$wavelength, pattern = "V"))
  # 
  # shadow_df_long$canopyPosition[is.na(shadow_df_long$canopyPosition)] <- "UNK"
  # 
  # # Average by species and shadow
  # # Averaged by species
  # shadow_df_long_mean <- shadow_df_long %>%
  #   group_by(taxonID, canopyPosition, wavelength) %>%
  #   summarise_all(mean)
  # 
  # shadow_df_long_mean
  # 
  # # Individual
  # # ggplot(shadow_df_long, aes(wavelength, reflectance, color = canopyPosition, group = indvdID)) +
  # #   geom_line(alpha = .2) + theme_bw() + facet_wrap(~taxonID)
  # 
  # # Average
  # ggplot(shadow_df_long_mean, aes(wavelength, reflectance, color = canopyPosition)) +
  #   geom_line() + theme_bw() + facet_wrap(~taxonID)
  # 
  # shadow_df_long %>%
  #   distinct(indvdID, .keep_all = TRUE) %>%
  #   group_by(taxonID, canopyPosition) %>%
  #   tally() %>%
  #   pivot_wider(., names_from = canopyPosition, values_from = n) %>%
  #   data.frame()
  # 
  # # Individual species
  # shadow_df_long %>%
  #   filter(taxonID == "PIPA2") %>% 
  #   group_by(indvdID, wavelength, canopyPosition) %>%
  #   summarise_all(mean) %>%
  # ggplot(., aes(wavelength, reflectance, color = canopyPosition, group = indvdID)) +
  #   geom_line(alpha = .2) + theme_bw()
  # 

  
  
# Calculate class labels and training and testing set  --------------------------------------------------
  
  y_all <- to_categorical(as.numeric(factor(tree_dat_pixel$taxonID)) - 1) # Starts with 0 indexing
  dim(y_all)
  
  
  # Add column to set whether individual is in training or testing set
  # Do split at tree crown level rather than pixel level so that each tree crown is fully in the training or validation set
  
  tree_dat$training = "valid"
  tree_dat$training[caret::createDataPartition(tree_dat$taxonID, p = params$testing_split)$Resample1] = "training"
  table(tree_dat$training, tree_dat$taxonID)
  
  
  table(tree_dat$training, tree_dat$siteID)
  
  training_ind <- which(tree_dat_pixel$indvdID %in% tree_dat$indvdID[tree_dat$training == "training"])
  valid_ind <- which(tree_dat_pixel$indvdID %in% tree_dat$indvdID[tree_dat$training == "valid"])
  length(training_ind)
  length(valid_ind)

  
  

# Subset data and class labels into training and testing -----------
  
  tree_dat_training <- tree_dat_pixel[training_ind, ]
  tree_dat_valid <- tree_dat_pixel[valid_ind, ] 
  
  dim(tree_dat_training)
  dim(tree_dat_valid)

  
  hsi_train = out2[training_ind, ] %>%
    select(-indvdID, -pixel,  -taxonID)
  y_train = y_all[training_ind, ]
  
  dim(hsi_train)[1] == nrow(y_train) # Should be true
  nrow(tree_dat_training) == nrow(y_train) # Should be true
  
  # if(ncol(hsi_train) != 369) {
  #   stop("Number of bands is wrong")
  # }
  
  # Subset validation data
  hsi_valid = out2[valid_ind, ] %>%
    select(-indvdID, -pixel,  -taxonID)
  
  y_valid = y_all[valid_ind, ]
  
  dim(hsi_valid)[1] == nrow(y_valid) # Should be true
  nrow(tree_dat_valid) == nrow(y_valid) # Should be true
  
  
  # if(ncol(hsi_valid) != 369) {
  #   stop("Number of bands is wrong")
  # }


  

#  Filter out outliers based on PCA ---------------------------------------
  

  pc_train <- prcomp(hsi_train, scale. = TRUE)
  pc_valid <- predict(pc_train, hsi_valid)

  to_remove_train <- NULL
  to_remove_valid <- NULL


  for(col in 1:params$pca_bands_filter){ # Set how many bands to filter based off of here


    temp_train <- which(abs(pc_train$x[, col]) >  (params$pca_sd_thresh*pc_train$sdev[col]))
    to_remove_train <- c(to_remove_train, temp_train)

    temp_valid <- which(abs(pc_valid[, col]) >  (params$pca_sd_thresh*pc_train$sdev[col]))
    to_remove_valid <- c(to_remove_valid, temp_valid)

  }

  cat("Removing ",  length(unique(to_remove_train)), " pixels from the training set based on PCA filtering.. \n")
  cat("Removing ",  length(unique(to_remove_valid)), " pixels from the validation set based on PCA filtering.. \n")


  if(length(to_remove_train) >= 1){
    hsi_train <- hsi_train[-unique(to_remove_train),]
    y_train <- y_train[-unique(to_remove_train),]
    tree_dat_training <- tree_dat_training[-unique(to_remove_train),]
  }
  
  if(length(to_remove_valid) >= 1){
    hsi_valid <- hsi_valid[-unique(to_remove_valid),]
    y_valid <- y_valid[-unique(to_remove_valid),]
    tree_dat_valid <- tree_dat_valid[-unique(to_remove_valid),]
  }
  
  
  

# Row standardize hyperspectral -------------------------------------------

 # hsi_train2 <- apply(hsi_train, MARGIN = 2, function(x) x - mean(x))  
 #  dim(hsi_train2)
 #  rowMeans(hsi_train2[1:10,])
 #  
 #  hsi_train2 <- as.data.frame(hsi_train[, -370])
 #  
 #  hsi_train2 <- as.matrix(hsi_train[, -370])
 #  means <- rowMeans(hsi_train2)
 #  
 #  for(row in 1:nrow(hsi_train2)){
 #    if(row %% 10 == 0)  cat("Working on row", row, " ... \n")
 #    hsi_train2[row, ] = hsi_train2[row, ] - means[row]
 #  }
 #  
 #  hsi_valid2 <- as.data.frame(hsi_valid[, -370])
 #  for(row in 1:nrow(hsi_valid2)){
 #    if(row %% 1000 == 0)  cat("Working on row", row, " ... \n")
 #    hsi_valid2[row, ] = hsi_valid2[row, ] - rowMeans(hsi_valid2[row, ])
 #  }
 #  
 #  hsi
 #  
 #  hsi_train2 <- as.matrix(hsi_train[, -370])
 #  hsi_train2 <- t(hsi_train2)
 #  dim(hsi_train2)
 #  hsi_train2 <- scale(hsi_train2, center = TRUE, scale = FALSE)
 #  hsi_train2 <- t(hsi_train2)
 #  dim(hsi_train2)
 #  rowMeans(hsi_train2[1:10,])
 #  colMeans(hsi_train2[, 1:10])
 #  
 #  hsi_valid2 <- as.matrix(hsi_valid[, -370])
 #  hsi_valid2 <- t(hsi_valid2)
 #  dim(hsi_valid2)
 #  hsi_valid2 <- scale(hsi_valid2, center = TRUE, scale = FALSE)
 #  hsi_valid2 <- t(hsi_valid2)
 #  dim(hsi_valid2)
 #  rowMeans(hsi_valid2[1:10,])
 #  colMeans(hsi_valid2[, 1:10])
  

# Transform hyperspectral bands to PCA ------------------------------------

  pc_train2 <- prcomp(hsi_train, scale. = TRUE)
  
#  summary(pc_train2)
  
  # Loadings of the first PC
   # plot(pc_train2$rotation[, 1], type = "l")
   # plot(pc_train2$rotation[, 2], type = "l")
   
  
  hsi_pca_train <- pc_train2$x
  
  # Apply PCA fit from training data to validation data
  hsi_pca_valid <- predict(pc_train2, hsi_valid)
  

  

# Resample training set to make a more even class distribution -------------------------

  # How many pixels per species
  sort(table(tree_dat_training$taxonID))
  
  low_spp <- names(table(tree_dat_training$taxonID))[table(tree_dat_training$taxonID) < params$resample_min]
  low_spp


  if(params$resample_min > 0) {
    resample_row_indices <- tree_dat_training %>%
      mutate(row_id = 1:nrow(.)) %>%
      filter(taxonID %in% low_spp) %>%
      group_by(taxonID) %>%
      sample_n(size = params$resample_min, replace = T) %>%
      pull(row_id)
    
    resample_row_indices <- c(resample_row_indices, tree_dat_training %>%
                                 mutate(row_id = 1:nrow(.)) %>%
                                 filter(!(taxonID %in% low_spp)) %>%
                                 pull(row_id))
    
  } else {
    resample_row_indices <- 1:nrow(tree_dat_training)
  }
  
  sort(table(tree_dat_training$taxonID[resample_row_indices]))



# Assign class weights ----------------------------------------------------

# Optional named list mapping indices (integers) to a weight (float) value, used for weighting the loss function (during training only). This can be useful to tell the model to "pay more attention" to samples from an under-represented class.

# levels(factor(tree_dat$taxonID))
# sums <- colSums(y_train)
# sums
# sums <- sums / sum(sums)
# sums <- 1 / sums
# sums
# 
# sums <- colSums(y_train)
# sums
# sums <- (1/sums)*sum(sums)/2
# sums
# 
# 
# class_weights <- list()
# for(x in 1:ncol(y_train)){
#   class_weights[[x]] <- sums[x]
# }
# class_weights
# 
# names(class_weights)
# names(class_weights) <- as.character(0:(ncol(y_train)- 1))
# names(class_weights) <- as.character(1:ncol(y_train))
# names(class_weights)

# Build and fit NN model -----------------------------------------------------


# model =  keras_model_sequential() 
# model %>% 
#   layer_dense(units = 256, activation = 'relu', input_shape = c(length(pc_components_model))) %>%
#   layer_batch_normalization() %>%
#   layer_dropout(rate = 0.2) %>%
#   layer_dense(units = 100) %>%
#   layer_dropout(rate = 0.2) %>%
#   layer_dense(units = ncol(y_all), activation = 'softmax')
# model
# 
# compile(model, loss = "categorical_crossentropy", 
#         optimizer = 'adam', metrics = "accuracy")
# 
# 
# history = keras::fit(model, 
#                     as.matrix(hsi_pca_train[, pc_components_model]),
#                      y = as.matrix(y_train[, ]), 
#                      epochs = 100,
#                    validation_data = list(as.matrix(hsi_pca_valid[, pc_components_model]), as.matrix(y_valid)),
#                      batch_size = 1000,
#                      # callbacks = list(
#                      #   callback_early_stopping(monitor = "val_accuracy", 
#                      #                           patience = 50,
#                      #                           restore_best_weights = TRUE),
#                      #   callback_reduce_lr_on_plateau())
# )



# Test out conv1d on raw hsi data
# 
# model =  keras_model_sequential() 
# model %>% 
#   layer_conv_1d(filters = 50, kernel_size = 1, activation = 'relu', input_shape = c(ncol(hsi_train), 1)) %>%
#   layer_batch_normalization() %>%
#   layer_dropout(rate = 0.2) %>%
#   layer_dense(units = 100) %>%
#   layer_dropout(rate = 0.2) %>%
#   layer_flatten() %>%
#   layer_dense(units = ncol(y_all), activation = 'softmax')
# model
# 
# # Example model
# model =  keras_model_sequential() 
# model %>%
# layer_conv_1d(filters=25, kernel_size=5,  activation = "relu",  input_shape = c(ncol(hsi_train), 1)) %>%
#   #layer_global_max_pooling_1d() %>%
#   #layer_max_pooling_1d(pool_size = 4) %>%
#   layer_flatten() %>% 
#   layer_dropout(rate = 0.2) %>%
#   #layer_dense(units = 10, activation = 'relu') %>%
#   layer_dense(units = ncol(y_all), activation = 'softmax')
# model
# 
# compile(model, loss = "categorical_crossentropy", 
#         optimizer = 'adam', metrics = "accuracy")
# 
# 
# # Example model 2
# model = keras_model_sequential()  %>%
#   layer_conv_1d(filters = 64, kernel_size = 3, activation ='relu',  input_shape = c(ncol(hsi_train), 1)) %>%
#   layer_conv_1d(filters = 64, kernel_size = 3, activation ='relu') %>%
#   layer_dropout(rate = 0.5) %>%
#   layer_max_pooling_1d(pool_size=2) %>%
#   layer_flatten() %>%
#   layer_dense(units = 100, activation='relu') %>%
#   layer_dense(units = ncol(y_all), activation = 'softmax') %>%
#   compile(loss = "categorical_crossentropy", 
#           optimizer = 'adam', metrics = "accuracy")
# model
# 
#   
# 
# history = keras::fit(model, 
#                      array_reshape(as.matrix(hsi_train), c(dim(as.matrix(hsi_train)), 1)),
#                      y = as.matrix(y_train[, ]), 
#                      epochs = 50,
#                      validation_data = list(array_reshape(as.matrix(hsi_valid),
#                                                           c(dim(as.matrix(hsi_valid)), 1)),
#                                             as.matrix(y_valid)),
#                      batch_size = 1000,
#                      # callbacks = list(
#                      #   callback_early_stopping(monitor = "val_accuracy", 
#                      #                           patience = 50,
#                      #                           restore_best_weights = TRUE),
#                      #   callback_reduce_lr_on_plateau())
# )
# 
# 
# pred = model %>%
#   predict(array_reshape(as.matrix(hsi_valid),
#                         c(dim(as.matrix(hsi_valid)), 1)))
# 


# 
# 
## Testing out Hyperparameter tuning
# 
# save(hsi_pca_train,
#      y_train,
#      hsi_pca_valid,
#      y_valid,
#      file = "./test.Rdata")
# 
# 
# library(tfruns)
# 
# runs <- tuning_run("code/04_parameter_tuning_test.R",
#                    flags = list(
#                      nodes1 = c(64, 128, 256),
#                      nodes2 = c(64, 128, 256),
#                      nodes3 = c(64, 128, 256),
#                      dropout1 = c(0.2, 0.3, 0.4),
#                      dropout2 = c(0.2, 0.3, 0.4),
#                      dropout3 = c(0.2, 0.3, 0.4),
#                      lr_annealing = c(0.1, 0.05),
#                      pc_components_model = c(5, 50, 100)
#                    ),
#                    sample = 0.05
# )
# 
# View(runs)
# 
# runs <- ls_runs()
# View(runs)
# 
# latest_run()
# compare_runs()
# 
# 
# # Fit top model
# top_run <- ls_runs( order = metric_val_accuracy)[1, ]
# 
# model <- keras_model_sequential() %>%
#   layer_dense(units = top_run$flag_nodes1, activation = "relu", input_shape = top_run$flag_pc_components_model) %>%
#   layer_batch_normalization() %>%
#   layer_dropout(rate = top_run$flag_dropout1) %>%
#   layer_dense(units = top_run$flag_nodes2, activation = "relu") %>%
#   layer_batch_normalization() %>%
#   layer_dropout(rate = top_run$flag_dropout2) %>%
#   layer_dense(units = top_run$flag_nodes3, activation = "relu") %>%
#   layer_batch_normalization() %>%
#   layer_dropout(rate = top_run$flag_dropout3) %>%
#   layer_dense(units = ncol(y_all), activation = "softmax") %>%
#   compile(
#     loss = 'categorical_crossentropy',
#     metrics = c('accuracy'),
#     optimizer = 'adam'
#   ) 
# 
# history =  keras::fit(model,
#             x = as.matrix(hsi_pca_train[, 1:top_run$flag_pc_components_model]),
#             y = as.matrix(y_train[, ]),
#             epochs = 35,
#             batch_size = 128,
#             validation_data = list(as.matrix(hsi_pca_valid[, 1:top_run$flag_pc_components_model]), as.matrix(y_valid)),  
#             callbacks = list(
#               callback_early_stopping(patience = 5),
#               callback_reduce_lr_on_plateau(factor = top_run$flag_lr_annealing)),
#     verbose = TRUE )
# 
# model









# Scaling up prediction from pixel level to tree crown -------------------------------------
# 
# # Generate model predictions on validation data
#   pred = model %>%
#     predict(hsi_pca_valid[, pc_components_model])
#   
#   # Convert to tibble and reformat column names and make column for individualID
#   pred <- as_tibble(pred)
#   colnames(pred) <- levels(factor(tree_dat$taxonID))
#   pred$indvdID <- tree_dat_valid$indvdID
#   pred$pixel <- tree_dat_valid$pixel
#   pred$true_spp <- tree_dat_valid$taxonID
#   
#  
#   
#   
#    ## Top prediction by majority vote
#   top <- pred %>%
#     select(indvdID, pixel, true_spp)
#       
#   for(row in 1:nrow(pred)){
#     temp <- which.max(pred[row, 1:length(levels(factor(tree_dat$taxonID)))])
#     
#     top$taxonID_maj[row] = names(temp)
#     top$prob_maj[row] = as.data.frame(pred)[row, temp]
#   } 
#   head(top)
#   
#   pred_majority <- top %>%
#     group_by(indvdID, taxonID_maj) %>%
#     tally() %>%
#     top_n(1) %>%
#     distinct(indvdID, .keep_all = TRUE)
#     
#     
# 
#   # Top prediction by average probability across pixels
#   pred_avg <- pred %>%
#     group_by(indvdID, true_spp) %>%
#     summarise_all(mean) %>%
#     dplyr::select(-pixel) %>%
#     pivot_longer(-c(indvdID, true_spp), names_to = "taxonID_avg", values_to = "prob_avg") %>%
#     group_by(indvdID) %>%
#     top_n(1)
#   
#   
#   
#   # Join average and majority votes
#   top_preds <- left_join(pred_majority, pred_avg)
#   head(top_preds)
#   dim(top_preds)
#   
#   
#   
#   ## Accuracy
# 
#   # Majority vote
#   sum(top_preds$taxonID_maj == top_preds$true_spp) / nrow(top_preds)
#   
#   # Average vote
#   sum(top_preds$taxonID_avg == top_preds$true_spp) / nrow(top_preds)
#   
#   # How much do they agree?
#   sum(top_preds$taxonID_avg == top_preds$taxonID_maj) / nrow(top_preds)
#   
#   
#   # Plot confusion matrix
#   confusionMatrix(factor(top_preds$true_spp, levels(factor(tree_dat$taxonID))),
#                   factor(top_preds$taxonID_avg, levels(factor(tree_dat$taxonID))))
#   
#   
#   
# # 
# #   
# #   # Explore where model got it wrong
# #   
#   
#   top_preds$wrong_pred <- "wrong"
#   top_preds$wrong_pred[top_preds$taxonID_avg == top_preds$true_spp] <- "right"
#   table(top_preds$wrong_pred)
#   
#   tree_dat_post <- left_join(tree_dat, dplyr::select(top_preds, -taxonID_avg, -taxonID_maj,  -true_spp))
#   table(tree_dat_post$wrong_pred)
# 
#   # Accuracy by site
#   tree_dat_post %>%
#     filter(!is.na(wrong_pred)) %>%
#     group_by(siteID) %>%
#     summarise(accuracy = sum(wrong_pred == "right", na.rm = TRUE) / length(wrong_pred))
#   
#   
#   
#   ggplot(tree_dat_post, aes(wrong_pred, elevation )) + geom_boxplot() + geom_point() + theme_bw(15)
# 
#   tree_dat_post$wrong_bin <- as.numeric(factor(tree_dat_post$wrong_pred)) - 1
#   table(tree_dat_post$wrong_bin)
# 
#   gm1 <- glm(wrong_bin ~ canopyPosition, data = tree_dat_post)
#   summary(gm1)
#   visreg::visreg(gm1)
# 
# 
#   gm1 <- glm(wrong_bin ~ siteID, data = tree_dat_post)
#   summary(gm1)
#   visreg::visreg(gm1)
# 
# 
#   gm1 <- glm(wrong_bin ~ nlcdClass, data = tree_dat_post)
#   summary(gm1)
#   visreg::visreg(gm1)
# 
# 
#   gm1 <- glm(wrong_bin ~ plantStatus, data = tree_dat_post)
#   summary(gm1)
#   visreg::visreg(gm1)
# 
# 
#   gm1 <- glm(wrong_bin ~height, data = tree_dat_post)
#   summary(gm1)
#   visreg::visreg(gm1)
# 
#   table(tree_dat$nlcdClass, tree_dat$siteID)


# Generate model predictions on training data
    # pred = model %>%
    #   predict(hsi_pca_train[, pc_components_model])
    # 
    # # Convert to tibble and reformat column names and make column for individualID
    # pred <- as_tibble(pred)
    # colnames(pred) <- levels(factor(tree_dat$taxonID))
    # pred$indvdID <- tree_dat_training$indvdID
    # pred$pixel <- tree_dat_training$pixel
    # pred$true_spp <- tree_dat_training$taxonID
    # 
    # 
    # pred_avg <- pred %>%
    #   group_by(indvdID, true_spp) %>%
    #   summarise_all(mean) %>%
    #   dplyr::select(-pixel)
    # 
    # pred_avg2 <- pred_avg %>%
    #   pivot_longer(-c(indvdID, true_spp), names_to = "taxonID", values_to = "prob") %>%
    #   group_by(indvdID) %>%
    #   top_n(5) %>%
    #   arrange(indvdID, desc(prob))
    # 
    # pred_avg2
    # 
    # ## Accuracy
    # top_preds <- pred_avg %>%
    #   pivot_longer(-c(indvdID, true_spp), names_to = "taxonID", values_to = "prob") %>%
    #   group_by(indvdID) %>%
    #   top_n(1)
    # 
    # sum(top_preds$taxonID == top_preds$true_spp) / nrow(top_preds)
    # 
    # 
    
    
    
    
    
# Predict on testing data -------------------------------------------------
    
    # # Load in testing data
    # load("./data/IDTREES_competition_test/task2/RemoteSensing_processed/hsi_list_cropped_test.Rdata")
    # load("./data/IDTREES_competition_test/task2/RemoteSensing_processed/ch_list_cropped_test.Rdata")
    # 
    # 
    # # Read in individual tree info
    # tree_dat_test <- read_csv('./data/IDTREES_competition_test/task2/testing_ITC_info.csv')
    # 
    # # Give names to list
    # names(hsi_list_cropped_test) <- tree_dat_test$indvdID
    # names(ch_list_cropped_test) <- tree_dat_test$indvdID
    # 
    # 
    # 
    # initial = TRUE
    # for(ind in tree_dat_test$indvdID){
    #   
    #   cat("Working on: ", ind, " ... \n")
    #   
    #   index = which(ind == names(hsi_list_cropped_test))
    #   
    #   
    #   # If only one pixel
    #   if(nrow(hsi_list_cropped_test[[index]][,, 1, drop = F]) == 1 & ncol(hsi_list_cropped_test[[index]][,, 1, drop = F]) == 1 ){
    #     
    #     cat("Only one pixel... \n")
    #     out_test_temp <- as_tibble(matrix(apply(hsi_list_cropped_test[[index]], c(3), function(x) c(x)), nrow = 1)) %>%
    #       mutate(pixel = 1:nrow(.),
    #              indvdID = ind,
    #              ch = c(ch_list_cropped_test[[index]]))
    #     
    #   } else {
    #     
    #     # Convert to rows are pixels and columns are bands
    #     out_test_temp <- as_tibble(apply(hsi_list_cropped_test[[index]], c(3), function(x) c(x))) %>%
    #       mutate(pixel = 1:nrow(.),
    #              indvdID = ind,
    #              ch = c(ch_list_cropped_test[[index]]))
    #     
    #   }
    #   
    #   if(initial == TRUE){
    #     out_test = out_test_temp
    #     initial = FALSE
    #   } else {
    #     out_test = bind_rows(out_test, out_test_temp)
    #   }
    #   
    #   if("value" %in% colnames(out_test)){
    #     stop()
    #   }
    #   
    # }
    # 
    # head(out_test)
    # dim(out_test)
    # table(out_test$pixel)
    # colnames(out_test)
    # length(table(out_test$indvdID))
    # 
    # # Check for missing data - shouldn't be any
    # # any(apply(out, MARGIN = 2, function(x) sum(is.na(x))) > 0) # If true, means there is missing data
    # 
    # # Remove pixels with negative values
    # neg_values <- apply(dplyr::select(out_test, -c(indvdID, pixel)), 1, function(x) any(x < 0))
    # neg_values
    # sum(neg_values)
    # 
    # out_test <- out_test[-which(neg_values), ]
    # dim(out_test)
    # 
    # 
    # # Add species ID
    # out2_test <- left_join(out_test, dplyr::select(tree_dat_test, indvdID))
    # 
    # # Reshape tree_dat to match pixel level
    # tree_dat_pixel_test <- inner_join(tree_dat_test, dplyr::select(out_test, indvdID, pixel))
    # dim(tree_dat_pixel_test)
    # dim(out2_test)
    # 
    # 
    # # make sure individuals are in the same row order
    # all(tree_dat_pixel_test$indvdID == out2_test$indvdID)
    # 
    # 
    # 
    # # Subset validation data
    # hsi_test = out2_test %>%
    #   select(-indvdID, -pixel)

    
  # Plot spectra by individual -------------------------------------------------
    # 
    # hsi_df_long_test <- out2_test %>%
    #   pivot_longer(-c(indvdID, pixel), names_to = "wavelength", values_to = "reflectance")
    # head(hsi_df_long_test)
    # 
    # hsi_df_long_test$wavelength <- as.numeric(str_remove(hsi_df_long_test$wavelength, pattern = "V"))
    # head(hsi_df_long_test)
    # 
    # # Averaged by individual across pixels
    # hsi_df_long_mean_test <- hsi_df_long_test %>%
    #   group_by(indvdID, wavelength) %>%
    #   summarise_all(mean)
    # 
    # summary(hsi_df_long_mean_test$reflectance)
    # hist(hsi_df_long_mean_test$reflectance)
    # 
    # ggplot(hsi_df_long_mean_test, aes(wavelength, reflectance, color = indvdID)) + 
    #   geom_line() + theme_bw() + theme(legend.position = "none")
    
    

    
    
    
#  Filter out outliers based on PCA ---------------------------------------
    
    # # Subset validation data
    # hsi_test = out2_test %>%
    #   select(-indvdID, -pixel)
    # 
    # pc_test <- predict(pc_train, hsi_test)
    # 
    # to_remove_test <- NULL
    # 
    # thresh = 3 # how many SD away from average to filter by?
    # 
    # for(col in 1:20){ # Set how many bands to filter based off of here
    #   
    #   temp_test <- which(abs(pc_test[, col]) >  (thresh*pc_train$sdev[col]))
    #   to_remove_test <- c(to_remove_test, temp_test)
    #   
    # }
    # 
    # cat("Removing ",  length(unique(to_remove_test)), " pixels from the test set based on PCA filtering.. \n")

    # hsi_train <- hsi_train[-unique(to_remove_train),]
    # y_train <- y_train[-unique(to_remove_train),]
    # 
    # hsi_valid <- hsi_valid[-unique(to_remove_valid),]
    # y_valid <- y_valid[-unique(to_remove_valid),]
    # 
    # 
    
    
    
    # Transform hyperspectral bands to PCA ------------------------------------
    
    # # Apply PCA fit from training data to testation data
    # hsi_pca_test <- predict(pc_train2, hsi_test)
    # 
    # # tree_dat_training <- tree_dat_training[-unique(to_remove_train),]
    # # tree_dat_valid <- tree_dat_valid[-unique(to_remove_valid),]
    # # 
    # 
    # 
    # 
    # # predict on model
    # 
    # # Make model predictions and format for submission
    # # Generate model predictions on validation data
    # pred = model %>%
    #   predict(hsi_pca_test[, pc_components_model])
    # 
    # pred
    # 
    # # Convert to tibble and reformat column names and make column for individualID
    # pred <- as_tibble(pred)
    # colnames(pred) <- levels(factor(tree_dat_training$taxonID))
    #   pred$indvdID <- tree_dat_pixel_test$indvdID
    # pred$pixel <- tree_dat_pixel_test$pixel
    # 
    # 
    # 
    # 
    # # Set 0 prob for PIEL at TALL
    # for(row in 1:nrow(pred)){
    #   if(row %% 1000 == 0) cat("Working on row ... ", row, " ... \n")
    #   if(grepl("TALL", pred$indvdID[row])){
    #     pred[row, "PIEL"] <- 0
    #     pred[row, 1:length(levels(factor(tree_dat_training$taxonID)))] <- pred[row, 1:length(levels(factor(tree_dat_training$taxonID)))] / sum(pred[row, 1:length(levels(factor(tree_dat_training$taxonID)))])
    #     # scale to sum to 1
    #     
    #   }
    # }
    # 
    # # # Sum predictions to 1
    # # pred2 <- apply(pred[, 1:length(levels(factor(tree_dat_training$taxonID)))], 2, function(x) x / sum(x))
    # # pred2 <- as.data.frame(pred2)
    # # pred2 <- tibble(pred2,
    # #                indvdID = tree_dat_pixel_test$indvdID,
    # #                pixel = tree_dat_pixel_test$pixel)
    # # pred <- pred2
    # 
    # 
    # # Average across pixels
    # pred_avg <- pred %>%
    #   group_by(indvdID) %>%
    #   summarise_all(mean) %>%
    #   dplyr::select(-pixel)
    # 
 

    
    # Convert to tibble and reformat column names and make column for individualID

    # Get most likely species 
    # pred_avg$most_likely =  apply(dplyr::select(pred_avg, -c(indvdID)),
    #                               MARGIN = 1, function(x) colnames(dplyr::select(pred_avg, -c(indvdID)))[which.max(x)])
    # pred_avg$siteID = str_sub(tree_dat_test$indvdID, start = 1,  end = 4)
    # 
    # # Overall most likely
    # pred_avg_overall= pred_avg %>%
    #   # filter(siteID == site) %>%
    #   group_by(most_likely) %>%
    #   tally() %>% 
    #   rename("predicted_n" = n) %>%
    #   full_join(., tree_dat %>%
    #               group_by(taxonID) %>%
    #               # filter(siteID == site) %>% # Filter to just site where mode lwas built
    #               tally() %>%
    #               rename("dataset_n" = n),
    #             by = c("most_likely" = "taxonID")) %>%
    #   arrange(most_likely)
    # 
    # pred_avg_overall %>%
    #   print(n = 100)
    # 
    # # Top prediction by ITC
    # pred_itc <- pred %>%
    #   group_by(indvdID) %>%
    #   summarise_all(mean) %>%
    #   dplyr::select(-pixel) %>%
    #   pivot_longer(-c(indvdID), names_to = "taxonID_avg", values_to = "prob_avg") %>%
    #   group_by(indvdID) %>%
    #   top_n(1) %>%
    #   mutate(siteID = str_sub(indvdID, 1, 4))
    # 
    # 
    # 
    # 
    # # Top predicted by site
    # pred_avg_site = pred_avg %>%
    #   group_by(siteID, most_likely) %>%
    #   tally() %>% 
    #   rename("predicted_n" = n) %>%
    #   full_join(., tree_dat %>%
    #               group_by(siteID, taxonID) %>%
    #               tally() %>%
    #               rename("dataset_n" = n,
    #                      "site" = siteID,
    #                      "most_likely" = taxonID)) %>%
    #   arrange(most_likely, site)
    # 
    # pred_avg_site %>%
    #   print(n = 100)
    # 
    # 
    # pred_avg %>%
    #   group_by(siteID, most_likely) %>%
    #   tally() %>%
    #   arrange(most_likely) %>%
    #   pivot_wider(names_from = siteID, values_from = n)
    # 
    # 
    # 
    # 
    # 
    # # the submission file must contain the following columns/attributes:
    # # indvdID: the matching ID from the ITC data
    # # taxonID: the predicted taxonomic code
    # # probability: the probability that the crown belongs to the associated genus or species. The
    # # probabilities for a given ITC ID (including the “Other” category) will be normalized to sum to 1 if
    # # the submitted values do not already.
    # 
    # # Pivot to longer format for submission
    # pred_long <- pred %>%
    #   group_by(indvdID) %>%
    #   summarise_all(mean) %>%
    #   dplyr::select(-pixel) %>%
    #   pivot_longer(-indvdID, names_to = "taxonID", values_to = "probability")
    # 
    # head(pred_long)
    # 
    # # How many predictions per individual?
    # pred_long %>%
    #   group_by(indvdID) %>%
    #   tally()
    # 
    # # Probs sum to 1?
    # pred_long %>%
    #   group_by(indvdID) %>%
    #   summarise(sum = sum(probability)) %>%
    #   print(n = 600)
    # 
    # 
    # # Write to file
    # write_csv(pred_long, "./submissions/task2_submission.csv")
    # 
    # 
    
    
    

# Gradient boosting with xgboost ------------------------------------------
  
  
if(params$transform_bands_to_pca == TRUE) {
 xgdata = hsi_pca_train[resample_row_indices, 1:params$pc_components_model]
} else {
  xgdata = as.matrix(hsi_train[resample_row_indices, ])
}   
  
  
# CV
# boost.cv <- xgb.cv(data = xgdata,
#                  label = as.numeric(factor(tree_dat_training$taxonID[resample_row_indices])) - 1,
#                  max_depth = params$xgb_max_depth,
#                  eta = params$xgb_eta,
#                  min_child_weight = params$xgb_min_child_weight,
#                  subsample = params$xgb_subsample,
#                  colsample_bytree = params$xgb_colsample_bytree,
#                  nrounds = params$xgb_nrounds,
#                  nfold = 5,
#                  early_stopping_rounds = 10, # stop if no improvement for 15 consecutive trees,
#                  objective = "multi:softprob",
#                  stratified = TRUE,
#                  seed = 42,
#                  num_class = length(table( factor(tree_dat_training$taxonID[resample_row_indices]))))
# 
# boost.cv


## Save error rates

params$train_merror_mean <- min(boost.cv$evaluation_log$train_merror_mean)
params$test_merror_mean <- min(boost.cv$evaluation_log$test_merror_mean)
params$best_ntreelimit <- boost.cv$best_ntreelimit
params$best_iteration <- boost.cv$best_iteration

 
set.seed(42)
# ## Run final model
boost <- xgboost(data = xgdata,
                 label = as.numeric(factor(tree_dat_training$taxonID[resample_row_indices])) - 1,
                 max_depth = params$xgb_max_depth,
                 eta = params$xgb_eta,
                 min_child_weight = params$xgb_min_child_weight,
                 subsample = params$xgb_subsample,
                 colsample_bytree = params$xgb_colsample_bytree,
                 nrounds = params$best_ntreelimit,
                 early_stopping_rounds = 10, # stop if no improvement for 15 consecutive trees,
                 objective = "multi:softprob",
                 num_class = length(table( factor(tree_dat_training$taxonID[resample_row_indices]))))

boost



## Predictions on validation data

if(params$transform_bands_to_pca == TRUE) {
  pred = predict(boost, hsi_pca_valid[, 1:params$pc_components_model])
} else {
  pred = predict(boost, as.matrix(hsi_valid))
}   


pred <- matrix(pred, nrow = length(table( factor(tree_dat_training$taxonID))),
                          ncol=length(pred)/length(table( factor(tree_dat_training$taxonID)))) %>%
  t() %>%
  data.frame()

head(pred)


pred <- as_tibble(pred)
head(pred)

colnames(pred) <- levels(factor(tree_dat_training$taxonID))
head(pred)

pred$indvdID <- tree_dat_valid$indvdID
pred$pixel <- tree_dat_valid$pixel
pred$true_spp <- tree_dat_valid$taxonID


pred_avg <- pred %>%
  group_by(indvdID, true_spp) %>%
  summarise_all(mean) %>%
  dplyr::select(-pixel)

pred_avg2 <- pred_avg %>%
  pivot_longer(-c(indvdID, true_spp), names_to = "taxonID", values_to = "prob") %>%
  group_by(indvdID) %>%
  top_n(5) %>%
  arrange(indvdID, desc(prob))

pred_avg2

## Accuracy
top_preds <- pred_avg %>%
  pivot_longer(-c(indvdID, true_spp), names_to = "taxonID", values_to = "prob") %>%
  group_by(indvdID) %>%
  top_n(1)

sum(top_preds$taxonID == top_preds$true_spp) / nrow(top_preds)


# Save to params file
params$accuracy_valid_set <- sum(top_preds$taxonID == top_preds$true_spp) / nrow(top_preds)

top_preds$wrong_pred <- "wrong"
top_preds$wrong_pred[top_preds$taxonID == top_preds$true_spp] <- "right"
table(top_preds$wrong_pred)


# Plot confusion matrix
confusionMatrix(factor(top_preds$true_spp, levels(factor(tree_dat$taxonID))),
                factor(top_preds$taxonID, levels(factor(tree_dat$taxonID))),
                mode = "prec_recall")



  tree_dat_post <- left_join(tree_dat, dplyr::select(top_preds, -taxonID, -true_spp))
  table(tree_dat_post$wrong_pred)

  # Accuracy by site
  tree_dat_post %>%
    filter(!is.na(wrong_pred)) %>%
    group_by(siteID) %>%
    summarise(accuracy = sum(wrong_pred == "right", na.rm = TRUE) / length(wrong_pred))
  
  
 params$accuracy_MLBS <- tree_dat_post %>%
   filter(!is.na(wrong_pred) & siteID == "MLBS") %>%
   group_by(siteID) %>%
   summarise(accuracy = sum(wrong_pred == "right", na.rm = TRUE) / length(wrong_pred)) %>%
   pull(accuracy)
 
 params$accuracy_OSBS <- tree_dat_post %>%
   filter(!is.na(wrong_pred) & siteID == "OSBS") %>%
   group_by(siteID) %>%
   summarise(accuracy = sum(wrong_pred == "right", na.rm = TRUE) / length(wrong_pred)) %>%
   pull(accuracy)



 
 ## Output parameters to file
 write_csv(params, "./grid_search_output/grid_search_out.csv", append = TRUE)
 
 



# Predict on testing data with Xgboost -------------------------------------------------

# Load in testing data
load("./data/IDTREES_competition_test/task2/RemoteSensing_processed/hsi_list_cropped_test.Rdata")
load("./data/IDTREES_competition_test/task2/RemoteSensing_processed/ch_list_cropped_test.Rdata")


# Read in individual tree info
tree_dat_test <- read_csv('./data/IDTREES_competition_test/task2/testing_ITC_info.csv')

# Give names to list
names(hsi_list_cropped_test) <- tree_dat_test$indvdID
names(ch_list_cropped_test) <- tree_dat_test$indvdID



initial = TRUE
for(ind in tree_dat_test$indvdID){

  cat("Working on: ", ind, " ... \n")

  index = which(ind == names(hsi_list_cropped_test))


  # If only one pixel
  if(nrow(hsi_list_cropped_test[[index]][,, 1, drop = F]) == 1 & ncol(hsi_list_cropped_test[[index]][,, 1, drop = F]) == 1 ){

    cat("Only one pixel... \n")
    out_test_temp <- as_tibble(matrix(apply(hsi_list_cropped_test[[index]], c(3), function(x) c(x)), nrow = 1)) %>%
      mutate(pixel = 1:nrow(.),
             indvdID = ind,
             ch = c(ch_list_cropped_test[[index]]))

  } else {

    # Convert to rows are pixels and columns are bands
    out_test_temp <- as_tibble(apply(hsi_list_cropped_test[[index]], c(3), function(x) c(x))) %>%
      mutate(pixel = 1:nrow(.),
             indvdID = ind,
             ch = c(ch_list_cropped_test[[index]]))

  }

  if(initial == TRUE){
    out_test = out_test_temp
    initial = FALSE
  } else {
    out_test = bind_rows(out_test, out_test_temp)
  }

  if("value" %in% colnames(out_test)){
    stop()
  }

}

head(out_test)
dim(out_test)
table(out_test$pixel)
colnames(out_test)
length(table(out_test$indvdID))

# Check for missing data - shouldn't be any
# any(apply(out, MARGIN = 2, function(x) sum(is.na(x))) > 0) # If true, means there is missing data

# Remove pixels with negative values
neg_values <- apply(dplyr::select(out_test, -c(indvdID, pixel)), 1, function(x) any(x < 0))
neg_values
sum(neg_values)

out_test <- out_test[-which(neg_values), ]
dim(out_test)

# Remove pixels with canopy height below threshold
low_ch_test <- out_test$ch <= params$min_canopy_height
sum(low_ch_test) # Total pixels with 0 CH

out_test <- out_test[!low_ch_test, ]

# Remove canopy_height column
out_test$ch <- NULL


# Add species ID
out2_test <- left_join(out_test, dplyr::select(tree_dat_test, indvdID))

# Reshape tree_dat to match pixel level
tree_dat_pixel_test <- inner_join(tree_dat_test, dplyr::select(out_test, indvdID, pixel))
dim(tree_dat_pixel_test)
dim(out2_test)


# make sure individuals are in the same row order
all(tree_dat_pixel_test$indvdID == out2_test$indvdID)



# Subset validation data
hsi_test = out2_test %>%
  select(-indvdID, -pixel)



#  Filter out outliers based on PCA ---------------------------------------

    #
    # pc_test <- predict(pc_train, hsi_test)
    #
    # to_remove_test <- NULL
    #
    #
    # for(col in 1:params$pca_bands_filter){ # Set how many bands to filter based off of here
    #
    #   temp_test <- which(abs(pc_test[, col]) >  (params$pca_sd_thresh*pc_train$sdev[col]))
    #   to_remove_test <- c(to_remove_test, temp_test)
    #
    # }
    #
    # cat("Removing ",  length(unique(to_remove_test)), " pixels from the training set based on PCA filtering.. \n")
    #
    #
    # if(length(to_remove_test) >= 1){
    #   hsi_test <- hsi_test[-unique(to_remove_test),]
    #   tree_dat_pixel_test <- tree_dat_pixel_test[-unique(to_remove_test),]
    # }
    #
    # # How many individuals left after filtering?? Add back in as 'other' for those that were filtered
    # length(table(tree_dat_pixel_test$indvdID))
    # set_to_other <- tree_dat_test$indvdID[!(tree_dat_test$indvdID %in% tree_dat_pixel_test$indvdID)]
    # set_to_other




# Transform hyperspectral bands to PCA ------------------------------------

# Apply PCA fit from training data to testation data
hsi_pca_test <- predict(pc_train2, hsi_test)

# predict on model

# Make model predictions and format for submission
# Generate model predictions on validation data

if(params$transform_bands_to_pca == TRUE) {
  xgdata_test = hsi_pca_test[, 1:params$pc_components_model]
} else {
  xgdata_test = as.matrix(hsi_test)
}



pred = predict(boost, xgdata_test)

pred

pred <- matrix(pred, nrow = length(table( factor(tree_dat_training$taxonID[resample_row_indices]))),
                          ncol=length(pred)/length(table( factor(tree_dat_training$taxonID[resample_row_indices])))) %>%
  t() %>%
  data.frame()

head(pred)

# Convert to tibble and reformat column names and make column for individualID
pred <- as_tibble(pred)
colnames(pred) <- levels(factor(tree_dat_training$taxonID[resample_row_indices]))
pred$indvdID <- tree_dat_pixel_test$indvdID
pred$pixel <- tree_dat_pixel_test$pixel



# Average across pixels
pred_avg <- pred %>%
  group_by(indvdID) %>%
  summarise_all(mean) %>%
  dplyr::select(-pixel)




# Convert to tibble and reformat column names and make column for individualID

# Get most likely species
pred_avg$most_likely =  apply(dplyr::select(pred_avg, -c(indvdID)),
                              MARGIN = 1, function(x) colnames(dplyr::select(pred_avg, -c(indvdID)))[which.max(x)])
pred_avg$siteID = str_sub(pred_avg$indvdID, start = 1,  end = 4)

# Overall most likely
pred_avg_overall= pred_avg %>%
  # filter(siteID == site) %>%
  group_by(most_likely) %>%
  tally() %>%
  rename("predicted_n" = n) %>%
  full_join(., tree_dat %>%
              group_by(taxonID) %>%
              # filter(siteID == site) %>% # Filter to just site where mode lwas built
              tally() %>%
              rename("dataset_n" = n),
            by = c("most_likely" = "taxonID")) %>%
  arrange(most_likely)


support <- read_csv("./submissions/task2_evaluation_2020_07_29.csv",
                    col_names = c("most_likely", "precision", "recall", "f1-score", "support"))


pred_avg_overall %>%
  left_join(., support) %>%
  print(n = 100)

# Top prediction by ITC
pred_itc <- pred %>%
  group_by(indvdID) %>%
  summarise_all(mean) %>%
  dplyr::select(-pixel) %>%
  pivot_longer(-c(indvdID), names_to = "taxonID_avg", values_to = "prob_avg") %>%
  group_by(indvdID) %>%
  top_n(1) %>%
  mutate(siteID = str_sub(indvdID, 1, 4))




# Top predicted by site
pred_avg_site = pred_avg %>%
  group_by(siteID, most_likely) %>%
  tally() %>%
  rename("predicted_n" = n) %>%
  full_join(., tree_dat %>%
              group_by(siteID, taxonID) %>%
              tally() %>%
              rename("dataset_n" = n,
                     "site" = siteID,
                     "most_likely" = taxonID)) %>%
  arrange(most_likely, site)

pred_avg_site %>%
  print(n = 100)


pred_avg %>%
  group_by(siteID, most_likely) %>%
  tally() %>%
  arrange(most_likely) %>%
  pivot_wider(names_from = siteID, values_from = n)





# the submission file must contain the following columns/attributes:
# indvdID: the matching ID from the ITC data
# taxonID: the predicted taxonomic code
# probability: the probability that the crown belongs to the associated genus or species. The
# probabilities for a given ITC ID (including the “Other” category) will be normalized to sum to 1 if
# the submitted values do not already.

# Pivot to longer format for submission
pred_long <- pred %>%
  group_by(indvdID) %>%
  summarise_all(mean) %>%
  dplyr::select(-pixel) %>%
  pivot_longer(-indvdID, names_to = "taxonID", values_to = "probability")

head(pred_long)

# How many predictions per individual?
pred_long %>%
  group_by(indvdID) %>%
  tally()

# Probs sum to 1?
pred_long %>%
  group_by(indvdID) %>%
  summarise(sum = sum(probability)) %>%
  print(n = 600)


# Write to file
write_csv(pred_long, "./submissions/task2_submission.csv")

# 
pred2 <- read_csv("./submissions/task2_submission_masjalapenos_2020_07_29.csv")


head(pred2)

t = pred2 %>%
  group_by(indvdID) %>%
  top_n(1, probability)
table(t$taxonID)




