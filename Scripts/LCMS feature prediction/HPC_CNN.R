


# Load necessary libraries
library(caret)
library(SummarizedExperiment)
library(plantspec)

# load & setup keras3
library(reticulate)

# singularity:
reticulate::use_virtualenv("/root/.virtualenvs/r-keras")

# rstudio server
#use_virtualenv("r-reticulate")

library(keras3); 
tensorflow::as_tensor("Hello World")

# Source the SE functions
# source("/Users/methungeorge/Desktop/IPB/all_scripts/LCMS_summarizedExperiment.R")
# source("/Users/methungeorge/Desktop/IPB/all_scripts/NIRS_summarizedExperiment_sue.R")
source("/home/mgeorge/HPC/Scripts/sue/LCMS_summarizedExperiment.R")
source("/home/mgeorge/HPC/Scripts/sue/NIRS_summarizedExperiment_sue.R")



# Read commandline arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("No metabolite index provided. Usage: Rscript HPC_RF.R <MetaboliteIndex>")
}

# Convert the first argument to a numeric index
metabolite_index <- as.numeric(args[1])
if (is.na(metabolite_index)) {
  stop("Invalid metabolite index. Please provide a numeric value.")
}

# metabolite_index <- 7


# Extract predictor and response data
predictor_data <- assays(NIRS_se)$counts  # Predictors: NIRS data
response_data <- assays(LCMS_se)$intensities  # Response: LCMS data

# Transpose data
predictor_data <- t(predictor_data)
response_data <- t(response_data)

# Modify row names
modify_rownames <- function(x) {
  components <- strsplit(sub("^pos_", "", x), "_")[[1]]
  return(paste(components[1:4], collapse = "_"))
}
rownames(predictor_data) <- sapply(rownames(predictor_data), modify_rownames)
rownames(response_data) <- sapply(rownames(response_data), modify_rownames)

# Find common row names
common_rows <- intersect(rownames(predictor_data), rownames(response_data))
predictor_data_subset <- predictor_data[common_rows, , drop = FALSE]
response_data_subset <- response_data[common_rows, , drop = FALSE]

# Check if row names are aligned
if (!all(rownames(predictor_data_subset) == rownames(response_data_subset))) {
  stop("Row names are not aligned. Please check input data.")
}

# Filter response variables with fewer zeros
filter_columns_with_fewer_zeros <- function(data, max_zeros) {
  zero_counts <- apply(data, 2, function(x) sum(x == 0))
  filtered_data <- data[, zero_counts <= max_zeros, drop = FALSE]
  return(filtered_data)
}

max_zeros <- 10
response_filtered <- filter_columns_with_fewer_zeros(response_data_subset, max_zeros)

# Get the metabolite name corresponding to the given index
metabolite_list <- colnames(response_filtered)
if (metabolite_index < 1 || metabolite_index > length(metabolite_list)) {
  stop(paste("Metabolite index out of range. Choose a number between 1 and ", length(metabolite_list)))
}

metabolite <- metabolite_list[metabolite_index]
response_vector <- response_filtered[, metabolite]

# Scale predictor and response data
predictor_scaled <- scale(predictor_data_subset)
response_scaled <- scale(response_vector)


response_sample <- response_scaled

set.seed(42)
train_index <- !(subdivideDataset(spectra = predictor_scaled,
                                  component = response_sample,
                                  method = "PCAKS",
                                  p = 0.3,
                                  type = "validation"))

predictor_train <- predictor_scaled[train_index, ]
response_train <- response_sample[train_index]
predictor_test <- predictor_scaled[!train_index, ]
response_test <- response_sample[!train_index]

#### Convolutional neural network (CNN) ####


model.cnn <- keras_model_sequential(input_shape = c(NULL, 2151, 1)) %>%
  layer_conv_1d(filter = 2, kernel_size = 50, kernel_initializer=initializer_glorot_uniform(seed=0)) %>%
  layer_batch_normalization() %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_flatten() %>%
  layer_dense(128, kernel_initializer=initializer_glorot_uniform(seed=0)) %>%
  layer_dense(32, kernel_initializer=initializer_glorot_uniform(seed=0)) %>%
  layer_dense(8, kernel_initializer=initializer_glorot_uniform(seed=0)) %>%
  layer_dense(1, kernel_initializer=initializer_glorot_uniform(seed=0))

model.cnn %>% compile(
  optimizer = "adam",
  loss = "mean_squared_error")

# Define early stopping callback
early_stopping <- callback_early_stopping(
  monitor = "val_loss",  # Monitor validation loss
  patience = 500,          
  restore_best_weights = TRUE  # Restore weights from the best epoch
)

start_time_cnn <- Sys.time()
history <- model.cnn %>% fit(x=as.matrix(predictor_train), y=as.matrix(response_train),
                             epochs = 5000,
                             verbose = 1,
                             validation_split = 0.2,
                             shuffle = FALSE,
                             callbacks = list(early_stopping)
)

end_time_cnn <- Sys.time()

training_time <- end_time_cnn - start_time_cnn

# predict
predictions <- predict(model.cnn, as.matrix(predictor_test))

# R2
R2_test <- cor(predictions, as.matrix(response_test))^2; R2_test

# RMSE
predictions <- as.numeric(predictions)
v <- as.numeric(response_test[[1]])
rmse_cnn <- sqrt(mean((predictor_test - response_test)^2))


results <- data.frame(
  Index = metabolite_index,
  Metabolite = metabolite, 
  R2 = R2_test,
  RMSE = rmse_cnn,
  TrainingTime = training_time
)


output_file <- paste0("CNN_GPU_Results_LCMS_", metabolite_index, ".csv")
write.csv(results, output_file, row.names = FALSE)

# Print status message
cat(paste0("CNN analysis for metabolite ", metabolite, " (index: ", metabolite_index, "), completed. Results saved to '", output_file, "'.\n"))













