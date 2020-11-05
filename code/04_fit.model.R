
# Load libraries
library(tidyverse)
library(keras)
library(caret)


# Working with cropped hyperspectral data - average down to just one pixel per tree

# Load in list data 
load("./data/train/RemoteSensing_processed/hsi_list_cropped.Rdata")

# Read in individual tree info
tree_dat <- read_csv('./data/train/Field/tree_data_joined.csv')
tree_dat <- tree_dat %>% separate(col = scientificName,into = c("Genus","Species"),remove = F,sep = " ")




#### tinkering with class labels  ####

##### 0. No change | Labels remain 33 ####
t0_y_all <- to_categorical(as.numeric(factor(tree_dat$taxonID)) - 1) # Starts with 0 indexing

##### 1. Labels at the genus level | Labels reduced from 33 to 17 ####
t1_tree_dat <- tree_dat %>% mutate(t1_taxonID = substr(x = .$Genus,start = 1,stop = 5))
#sort(unique(t1_tree_dat$scientificName))
#ignore warning, warning indicates subspecies/variety/person describing it, etc. not important here

t1_y_all <- to_categorical(as.numeric(factor(t1_tree_dat$t1_taxonID)) - 1 ) # Starts with 0 indexing
dim(t1_y_all) # Class labels for all images
colSums(t1_y_all)

##### 2. Removing trees with < 20 stems && with congeners | Labels reduced from 33 to 19 ####

# a) Why 20 individuals? Natural break in data at ~20 individuals
#tree_dat %>% group_by(scientificName) %>% summarise(tt = n()) %>% arrange(tt) %>%  as.data.frame()

# b) Why exclude rare species with congeners? Assuming species from the same genus have similar
# spectral signatures, rare congeners may get misclassified as its congener, rare or common (assuming 
# the algorithm is sensitive to abundance, not unlike ecological mass-effect) whereas rare species,
# even if they are singletons, may have a unique enough spectral signature for the algo to discern

t2_tree_dat <- 
  tree_dat %>% 
  left_join(x = .,
            y = tree_dat %>% group_by(scientificName) %>% summarise(cc = n()),
            by = "scientificName") %>% 
  left_join(x = .
            ,y = tree_dat %>% group_by(Genus) %>% summarise(nsp = n_distinct(Species)),
            by = "Genus") %>% 
  mutate(chooseSP = ifelse(test = cc > 20, "yes",
                           ifelse(test = nsp < 2, "yes","no"))) %>% 
  mutate(t2_taxonID = paste0(substr(Genus,1,3),substr(Species,1,3))) %>% 
  filter(chooseSP == "yes")

t2_y_all <- to_categorical(as.numeric(factor(t2_tree_dat$t2_taxonID)) - 1) # Starts with 0 indexing
dim(t2_y_all) # Class labels for all images
colSums(t2_y_all)



##### 3. Both 1 and 2 from above | Labels reduced from 33 to 16 ####

t3_tree_dat <- 
  tree_dat %>% 
  left_join(x = .,
            y = tree_dat %>% group_by(scientificName) %>% summarise(cc = n()),
            by = "scientificName") %>% 
  left_join(x = .
            ,y = tree_dat %>% group_by(Genus) %>% summarise(nsp = n_distinct(Species)),
            by = "Genus") %>% 
  mutate(chooseSP = ifelse(test = cc > 20, "yes",
                           ifelse(test = nsp < 2, "yes","no"))) %>% 
  filter(chooseSP == "yes") %>% 
  mutate(t3_taxonID = Genus)

t3_y_all <- to_categorical(as.numeric(factor(t3_tree_dat$t3_taxonID)) - 1) # Starts with 0 indexing
dim(t3_y_all) # Class labels for all images
colSums(t3_y_all)

###############

# Take the average reflectance value separately for each band of the cropped image
hsi_list_cropped_mean = lapply(hsi_list_cropped, function(x) apply(x, c(3), median))
str(hsi_list_cropped_mean)
hsi_list_cropped_mean[[1]]

# Convert from list to matrix
hsi_array_cropped = matrix(unlist(hsi_list_cropped_mean), nrow = length(hsi_list_cropped), ncol = 369, byrow = TRUE)
dim(hsi_array_cropped)
hsi_array_cropped[1, ] # For first individual

hsi_list_cropped_mean[[1]] == hsi_array_cropped[1, ] # Should be all true




# Plot spectra by species -------------------------------------------------

  hsi_df <- as_tibble(hsi_array_cropped)
  hsi_df$taxonID = tree_dat$taxonID
  
  hsi_df_long <- hsi_df %>%
    pivot_longer(-taxonID, names_to = "wavelength", values_to = "reflectance")
  head(hsi_df_long)
  
  hsi_df_long$wavelength <- as.numeric(str_remove(hsi_df_long$wavelength, pattern = "V"))
  
  # individuals
  ggplot(hsi_df_long, aes(wavelength, reflectance)) + 
    geom_line() + theme_bw()
  
  
  # Averaged by species
  hsi_df_long_mean <- hsi_df_long %>%
    group_by(taxonID, wavelength) %>%
    summarise_all(mean)
  
  ggplot(hsi_df_long_mean, aes(wavelength, reflectance, color = taxonID)) + 
    geom_line() + theme_bw()
  

# Simulate unknown species ----------------------------------------------


    # ranges <- apply(hsi_array_cropped, MARGIN = 2, function(x) c(min(x), max(x)))
    # ranges
    # hist(ranges[1, ])
    # hist(ranges[2, ])
    # min(ranges[1, ])
    # max(ranges[2, ])
    # 
    # 
    # simulate_spectra = function(n_ind){
    #   
    #   # Initialize output matrix
    #   out <- matrix(NA, nrow = n_ind, ncol = 369)
    #   
    #   for(x in 1:369){
    #     out[ , x] = runif(n = n_ind, min = ranges[1, x], max = ranges[2, x] )
    #   }
    #   out 
    #   
    #   return(out)
    # }
    # 
    # 
    # # Bind to array
    # 
    # n_sim <- 50
    # 
    # hsi_array_cropped <- rbind(hsi_array_cropped,  
    #                            simulate_spectra(n_ind = n_sim))
    # 
    # # Add to tree dat
    # tree_dat <- bind_rows(tree_dat,
    #                       tibble(taxonID = rep("Other", n_sim)))

  
  

# Group species with less than 4 images as 'Other' ------------------------

other_species = tree_dat %>%
    group_by(taxonID) %>%
    tally() %>%
    filter(n < 4) %>%
    pull(taxonID)
  
  
tree_dat$taxonID[tree_dat$taxonID %in% other_species] <- 'Other'
table(tree_dat$taxonID)

y_all <- to_categorical(as.numeric(factor(tree_dat$taxonID)) - 1)
dim(y_all)
colSums(y_all)
  

# Class labels for all images ---------------------------------------------

# y_all <- t0_y_all # replace with t[i]_y_all, where i is the tinkering option above 
# # Starts with 0 indexing
# dim(y_all)
# colSums(y_all)


# Create train test split, balanced between spieces -----------------------


# Add column to set whether individual is in training or testing set
testing_split = 0.7

tree_dat$training = "valid"
tree_dat$training[caret::createDataPartition(tree_dat$taxonID, p = testing_split)$Resample1] = "training"
table(tree_dat$training, tree_dat$taxonID)

table(tree_dat$training, tree_dat$nlcdClass)



# Subset raster data and class labels into training and testing
  hsi_train = hsi_array_cropped[tree_dat$training == "training",]
  dim(hsi_train)
  y_train = y_all[tree_dat$training == "training", ]
  
  dim(hsi_train)[1] == nrow(y_train) # Should be true

# Subset validation data
  hsi_valid = hsi_array_cropped[tree_dat$training == "valid",]
  y_valid = y_all[tree_dat$training == "valid", ]

  dim(hsi_valid)[1] == nrow(y_valid) # Should be true


# Build and fit model -----------------------------------------------------


model =  keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(369),
              kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = ncol(y_all), activation = 'softmax')
model


compile(model, loss = "categorical_crossentropy", 
        optimizer = 'adam', metrics = "accuracy")


history = keras::fit(model, 
                     hsi_train, 
                     y = y_train, 
                     epochs = 150,
                     validation_data = list(hsi_valid, y_valid),
                     batch_size = 50,
                     # callbacks = list(
                     #   callback_early_stopping(monitor = "val_accuracy", 
                     #                           patience = 50,
                     #                           restore_best_weights = TRUE),
                     #   callback_reduce_lr_on_plateau())
                     )


model %>% evaluate(x = hsi_valid, y = y_valid)


# Other potential layers
# layer_dropout(0.6) %>%
# layer_dense(units = 369, activation = 'relu', input_shape = c(369),  kernel_regularizer = regularizer_l2(l = 0.001)) %>%

  
# calculate baseline performance - what if model only predicted most common class?
top_spp = sort(table(tree_dat$taxonID[tree_dat$training == "valid"]), decreasing = T)[1]

names(top_spp)

sum(rep(names(top_spp), length(tree_dat$taxonID[tree_dat$training == "valid"])) == tree_dat$taxonID[tree_dat$training == "valid"]) /  length(tree_dat$taxonID[tree_dat$training == "valid"])




 # Print model predictions on validation data -------------------------------------------------


pred = model %>%
  predict(hsi_valid)


correct = 0
for (row in 1:nrow(pred)){
  
  # Find highest probability in prediction
  pred_sp = which(pred[row, ] == max(pred[row, ]))
  pred_sp = levels(factor(tree_dat$taxonID))[pred_sp]
  
  prob = round(max(pred[row, ]), 2)
  
  
  # Find true species
  true_sp = which(y_valid[row, ] == 1)
  true_sp = levels(factor(tree_dat$taxonID))[true_sp]
  
  
  # Print
  cat("Predicted species is: ", pred_sp, " at prob: ", prob, " | True species is: ", true_sp, "... \n" )
  
  # If correct
  if(pred_sp == true_sp){
    correct = correct + 1
  }
  
}

# Calculate accuracy
correct / nrow(y_valid)




# Format predictions for self evaluation ---------------------------------------

# the submission file must contain the following columns/attributes:
# indvdID: the matching ID from the ITC data
# taxonID: the predicted taxonomic code
# probability: the probability that the crown belongs to the associated genus or species. The
# probabilities for a given ITC ID (including the “Other” category) will be normalized to sum to 1 if
# the submitted values do not already.

# Generate model predictions on validation data
pred = model %>%
  predict(hsi_valid)

# Convert to tibble and reformat column names and make column for individualID
pred <- as_tibble(pred)
colnames(pred) <- levels(factor(tree_dat$taxonID))
pred$indvdID <- tree_dat$indvdID[tree_dat$training == "valid"]

pred

# Pivot to longer format for submission
pred_long <- pred %>%
  pivot_longer(-indvdID, names_to = "taxonID", values_to = "probability")

head(pred_long)

# How many predictions per individual?
pred_long %>%
  group_by(indvdID) %>%
  tally()


# Write to file
write_csv(pred_long, "./self-evaluation/task2_submission.csv")


# Output truth for self-evaluation ----------------------------------------

# columns
# indvdID
# speciesID
# genusID

truth <- tibble(ID = tree_dat$indvdID[tree_dat$training == "valid"],
                speciesID = tree_dat$taxonID[tree_dat$training == "valid"],
                genusID = str_sub(tree_dat$taxonID[tree_dat$training == "valid"], 1, 2)) # Genus ID is first two letters
 
write_csv(truth, "./self-evaluation/task2_ground.csv")











# Predict on testing data -------------------------------------------------

 # Load in testing data
 load("./data/IDTREES_competition_test/task2/RemoteSensing_processed/hsi_list_cropped_test.Rdata")

  # Read in individual tree info
  tree_dat_testing <- read_csv('./data/IDTREES_competition_test/task2/testing_ITC_info.csv')


  # Take the average reflectance value separately for each band of the cropped image
  hsi_list_cropped_mean_test = lapply(hsi_list_cropped_test, function(x) apply(x, c(3), median))
  str(hsi_list_cropped_mean_test)

  # Convert from list to array
  hsi_array_cropped_test = matrix(unlist(hsi_list_cropped_mean_test), nrow = length(hsi_list_cropped_test), ncol = 369, byrow = TRUE)
  dim(hsi_array_cropped_test)
  hsi_array_cropped_test[1, ] # For first individual
  
  hsi_list_cropped_mean_test[[1]] == hsi_array_cropped_test[1, ] # Should be all true
  
  
  
  # Make model predictions and format for submission
  # Generate model predictions on validation data
  pred = model %>%
    predict(hsi_array_cropped_test)
  


  # the submission file must contain the following columns/attributes:
  # indvdID: the matching ID from the ITC data
  # taxonID: the predicted taxonomic code
  # probability: the probability that the crown belongs to the associated genus or species. The
  # probabilities for a given ITC ID (including the “Other” category) will be normalized to sum to 1 if
  # the submitted values do not already.
  

  # Convert to tibble and reformat column names and make column for individualID
  pred <- as_tibble(pred)
  colnames(pred) <- levels(factor(tree_dat$taxonID))
  pred$indvdID <- tree_dat_testing$indvdID
  
  pred
  
  
  # Get most likely species 
  pred_sum = pred
  pred_sum$most_likely =  apply(pred, MARGIN = 1, function(x) colnames(pred)[which.max(x)])
  pred_sum$site = str_sub(tree_dat_testing$indvdID, start = 1,  end = 4)
  
  # Overall most likely
  pred_sum_overall= pred_sum %>%
                group_by(most_likely) %>%
                tally() %>% 
                rename("predicted_n" = n) %>%
                full_join(., tree_dat %>%
                            group_by(taxonID) %>%
                            tally() %>%
                            rename("dataset_n" = n),
                          by = c("most_likely" = "taxonID")) %>%
    arrange(most_likely)
  
  pred_sum_overall %>%
    print(n = 100)
              
  

  # Top predicted by site
  pred_sum_site = pred_sum %>%
    group_by(site, most_likely) %>%
    tally() %>% 
    rename("predicted_n" = n) %>%
    full_join(., tree_dat %>%
                group_by(siteID, taxonID) %>%
                tally() %>%
                rename("dataset_n" = n,
                       "site" = siteID,
                       "most_likely" = taxonID)) %>%
    arrange(most_likely, site)
  
  pred_sum_site %>%
    print(n = 100)
  
  
  pred_sum %>%
    group_by(site, most_likely) %>%
    tally() %>%
    arrange(most_likely) %>%
    pivot_wider(names_from = site, values_from = n)

  
  # Pivot to longer format for submission
  pred_long <- pred %>%
    pivot_longer(-indvdID, names_to = "taxonID", values_to = "probability")
  
  head(pred_long)
  
  # How many predictions per individual?
  pred_long %>%
    group_by(indvdID) %>%
    tally()
  
  
  # Write to file
  write_csv(pred_long, "./submissions/task2_submission.csv")
  
  
  
  
  
  
  
# Testing out random forest -----------------------------------------------
  
  library(randomForest)
  
  f <- randomForest(x = hsi_train,
                    y = factor(tree_dat$taxonID[tree_dat$training == "training"],
                               levels = levels(factor(tree_dat$taxonID))),
                    xtest = hsi_valid,
                    ytest = factor(tree_dat$taxonID[tree_dat$training == "valid"],
                                   levels = levels(factor(tree_dat$taxonID))),
                    ntree = 1000,
                    sampsize = 100)
  
  f
  tail(f$err.rate)
  tail(f$test$err.rate)
  

  
  # Testing out PCA
  pc <-  prcomp(hsi_array_cropped)
  pc$x
  
  f <- randomForest(x = pc$x,
                    y = factor(tree_dat$taxonID),
                    ntree = 500, 
                    sampsize = 500)
  
  f
  tail(f$err.rate)


# Old code ----------------------------------------------------------------

# 
# # Load in list data 
# load("./data/train/RemoteSensing_processed/rasters_as_lists.Rdata")
# 
# # Read in individual tree info
# tree_dat <- read_csv('./data/train/Field/tree_data_joined.csv')
# 
# 
# # Class labels for all images
# y_all <- to_categorical(as.numeric(factor(tree_dat$taxonID)) -1 ) # Starts with 0 indexing
# dim(y_all)
# colSums(y_all)
# 
# # Convert hsi data to array
# hsi_array_full = array(unlist(hsi_list_full), dim = c(length(hsi_list_full), 
#                                          nrow(hsi_list_full[[1]]), 
#                                          ncol(hsi_list_full[[1]]),
#                                          369))
# dim(hsi_array_full)
# hsi_array_full[1, ,, 10]
# 
# 
# 
# # Create train test split
# # Add column to set whether individual is in training or testing set
# testing_split = 0.6
# 
# tree_dat$training = "valid"
# tree_dat$training[caret::createDataPartition(tree_dat$taxonID, p = testing_split)$Resample1] = "training"
# table(tree_dat$training, tree_dat$taxonID)
# 
# 
# 
# 
# # Subset raster data into training and testing
# hsi_train = hsi_array_full[tree_dat$training == "training",,,]
# dim(hsi_train)
# 
# # class labels
# y_train = y_all[tree_dat$training == "training", ]
# 
# dim(hsi_train)[1] == nrow(y_train) # Should be true
# 
# 
# hsi_valid = hsi_array_full[tree_dat$training == "valid",,,]
# dim(hsi_valid)
# 
# y_valid = y_all[tree_dat$training == "valid", ]
# 
# dim(hsi_valid)[1] == nrow(y_valid) # Should be true
# 
# 
# # Build and fit model -----------------------------------------------------
# 
# 
# model =  keras_model_sequential() 
# model %>% 
#   layer_conv_2d(filters = 10,
#                 kernel_size = 2,
#                 input_shape = c(20, 20, 369),
#                 data_format = "channels_last") %>%
#   layer_flatten() %>%
#   layer_dense(units = 33, activation = 'softmax')
# model
# 
# 
# compile(model, loss = "categorical_crossentropy", optimizer = optimizer_rmsprop(), metrics = "accuracy")
# 
# 
# history = keras::fit(model, 
#                      hsi_train, 
#                      y = y_train, 
#                      epochs = 30,
#                      validation_data = list(hsi_valid, y_valid),
#                      batch_size = 50)
# 
# 


# 
# # 
# devtools::install_github("rstudio/keras")
# library(keras)
# install_keras()
# # Miniconda has been successfully installed at '/Users/luke/Library/r-miniconda'.
# 
# 
# mnist <- dataset_mnist()
# x_train <- mnist$train$x
# y_train <- mnist$train$y
# x_test <- mnist$test$x
# y_test <- mnist$test$y
# # reshape
# x_train <- array_reshape(x_train, c(nrow(x_train), 784))
# x_test <- array_reshape(x_test, c(nrow(x_test), 784))
# # rescale
# x_train <- x_train / 255
# x_test <- x_test / 255
# y_train <- to_categorical(y_train, 10)
# y_test <- to_categorical(y_test, 10)
# 
# 
# model <- keras_model_sequential() 
# model %>% 
#   layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>% 
#   layer_dropout(rate = 0.4) %>% 
#   layer_dense(units = 128, activation = 'relu') %>%
#   layer_dropout(rate = 0.3) %>%
#   layer_dense(units = 10, activation = 'softmax')
# 
# model %>% compile(
#   loss = 'categorical_crossentropy',
#   optimizer = optimizer_rmsprop(),
#   metrics = c('accuracy')
# )
# 
# history <- model %>% fit(
#   x_train, y_train, 
#   epochs = 30, batch_size = 128, 
#   validation_split = 0.2
# )
# 
# 
# 
# 
# 
# 
# # Read in individual tree info
# tree_dat <- read_csv('./data/train/Field/tree_data_joined.csv')
# 
# 
# # Loop through individuals and load in rasters as array
# hsi_array <- array(data = NA, dim = c(nrow(tree_dat), 20, 20, 369))
# x = 1
# for(ind in tree_dat$indvdID){
#   
#   if(x %% 100 == 0){
#   
#   cat("Working on image number: ", x, " ... \n")
#   }
#   
#   hsi_path <- grep(paste0("_", ind, ".tif$"),
#                    list.files("./data/train/RemoteSensing_processed/HSI_by_species_full/",
#                    recursive = TRUE,
#                    full.names = TRUE),
#                    value = TRUE)
#   # cat("Path to HSI:", hsi_path, "... \n")
#   # hsi <- stack(hsi_path)
#   
#   hsi <- readTIFF(hsi_path)
#   # 
#   # Save in array
#   hsi_array[x,,,] <- hsi
#   
#   # Increment counter
#   x = x + 1
#   
# }
# 
# dim(hsi_array)
# 
# 
# # Class labels
# y <- to_categorical(as.numeric(factor(tree_dat$taxonID)))
# y
# dim(y)
# 
# model =  keras_model_sequential() 
# model %>% 
#   layer_conv_2d(filters = 10,
#                 kernel_size = 2,
#                 input_shape = c(20, 20, 369),
#                 data_format = "channels_last") %>%
#   layer_flatten() %>%
#   layer_dense(units = 34, activation = 'softmax')
# model
# 
# 
# compile(model, loss = "categorical_crossentropy", optimizer = optimizer_rmsprop(), metrics = "accuracy")
# 
# 
# history = keras::fit(model,  hsi_array, y = y, epochs = 20, batch_size = 50)
# 
# 
# 
# ## Reshaping
# 
# hsi_reshape <- array_reshape(hsi_array, c(nrow(hsi_array), 20*20))
# dim(hsi_reshape)
# 
# 
# model <- keras_model_sequential() 
# model %>% 
#   layer_dense(units = 256, activation = 'relu', input_shape = c(400)) %>% 
#   layer_dropout(rate = 0.4) %>% 
#   layer_dense(units = 128, activation = 'relu') %>%
#   layer_dropout(rate = 0.3) %>%
#   layer_dense(units = 34, activation = 'softmax')
# 
# model %>% compile(
#   loss = 'categorical_crossentropy',
#   optimizer = optimizer_rmsprop(),
#   metrics = c('accuracy')
# )
# 
# history <- model %>% fit(
#   hsi_reshape, y,
#   epochs = 5, batch_size = 128,
#   validation_split = .2)
# 
# # history <- model %>% fit(
# #   x_train, y_train, 
# #   epochs = 30, batch_size = 128,
# #   validation_split = .2)


