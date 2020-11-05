library(keras)

load("./test.Rdata")

FLAGS <- flags(
  # Nodes
  flag_numeric("nodes1", 256),
  flag_numeric("nodes2", 128),
  flag_numeric("nodes3", 64),
  # Dropout
  flag_numeric("dropout1", 0.4),
  flag_numeric("dropout2", 0.3),
  flag_numeric("dropout3", 0.2),
  # Learning paramaters
  flag_numeric("lr_annealing", 0.1),
  flag_numeric("pc_components", 50)
)



model <- keras_model_sequential() %>%
  layer_dense(units = FLAGS$nodes1, activation = "relu", input_shape = FLAGS$pc_components) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = FLAGS$dropout1) %>%
  layer_dense(units = FLAGS$nodes2, activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = FLAGS$dropout2) %>%
  layer_dense(units = FLAGS$nodes3, activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = FLAGS$dropout3) %>%
  layer_dense(units = ncol(y_all), activation = "softmax") %>%
  compile(
    loss = 'categorical_crossentropy',
    metrics = c('accuracy'),
    optimizer = 'adam'
  ) %>%
  fit(
    x = as.matrix(hsi_pca_train[, 1:FLAGS$pc_components]),
    y = as.matrix(y_train[, ]),
    epochs = 35,
    batch_size = 128,
    validation_data = list(as.matrix(hsi_pca_valid[, 1:FLAGS$pc_components]), as.matrix(y_valid)),  
    callbacks = list(
      callback_early_stopping(patience = 5),
      callback_reduce_lr_on_plateau(factor = FLAGS$lr_annealing)
    ),
    verbose = FALSE
  )

model