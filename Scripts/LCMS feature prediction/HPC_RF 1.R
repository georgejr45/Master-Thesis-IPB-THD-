# Load necessary libraries
library(caret)
library(SummarizedExperiment)
library(plantspec)

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


#   
# # Select one row at a time as response variable
# response_sample <- response_scaled 
  
# Split data into train and test sets
set.seed(42)
train_index <- !(subdivideDataset(spectra = predictor_scaled,
                                    component = response_scaled,
                                    method = "PCAKS",
                                    p = 0.3,
                                    type = "validation"))
  
predictor_train <- predictor_scaled[train_index, ]
response_train <- response_scaled[train_index]
predictor_test <- predictor_scaled[!train_index, ]
response_test <- response_scaled[!train_index]
  
# Create a training dataset
train_data <- data.frame(Response = response_train, predictor_train)
  
# Train Random Forest model
train_control <- trainControl(method = "cv", number = 10)
start_time <- Sys.time()
rf_model <- train(Response ~ ., data = train_data, method = "rf", ntree = 500, importance = TRUE, trControl = train_control)
end_time <- Sys.time()
training_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
# Make predictions and calculate performance metrics
colnames(predictor_test) <- paste0("X", colnames(predictor_test))
predictions <- predict(rf_model, newdata = predictor_test)
metrics <- postResample(predictions, response_test)
  
# Save results to a CSV file (with metabolite index and observation index)
results <- data.frame(
    Index = metabolite,
    Metabolite = metabolite, 
    R2 = metrics[["Rsquared"]],
    RMSE = metrics[["RMSE"]],
    TrainingTime = training_time
  )
  
output_file <- paste0("RF_Results_LCMS_", metabolite_index, ".csv")
write.csv(results, output_file, row.names = FALSE)
  
# Print status message
cat(paste0("Random Forest analysis for metabolite ", metabolite, " (index: ", metabolite_index, "), completed. Results saved to '", output_file, "'.\n"))

