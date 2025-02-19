# R script to perform PLSR on Sue Marr's data from SE, It outputs a csv file with file names, R2, RMSE and training time.

# load the libraries
library(caret)
library(SummarizedExperiment)
library(plantspec)

# source the SE functions
# source("/Users/methungeorge/Desktop/IPB/all_scripts/LCMS_summarizedExperiment.R")
# source("/Users/methungeorge/Desktop/IPB/all_scripts/NIRS_summarizedExperiment_sue.R")
source("/home/mgeorge/HPC/Scripts/sue/LCMS_summarizedExperiment.R")
source("/home/mgeorge/HPC/Scripts/sue/NIRS_summarizedExperiment_sue.R")

# Extract assay data
predictor_data <- assays(NIRS_se)$counts # NIRS is the predictor
response_data <- assays(LCMS_se)$intensities # LCMS is the response

# transpose the data
predictor_data <- t(predictor_data)
response_data <- t(response_data)

# Modify the row names
# Make the sample names structured similar in both LCMS and NIRS
modify_rownames <- function(x) {
  components <- strsplit(sub("^pos_", "", x), "_")[[1]]
  return(paste(components[1:4], collapse ="_"))
}

rownames(predictor_data) <- sapply(rownames(predictor_data), modify_rownames)
rownames(response_data) <- sapply(rownames(response_data), modify_rownames)


# Find the common row names 
common_rows <- intersect(rownames(predictor_data), rownames(response_data))

# Subset the predictor_data and response_data matrices to include only the common rows
predictor_data_subset <- predictor_data[common_rows, , drop = FALSE]
response_data_subset <- response_data[common_rows, , drop = FALSE]

# Check if the row names are now aligned and in the same order
if (all(rownames(predictor_data_subset) == rownames(response_data_subset))) {
  print("Row names are aligned!")
} else {
  print("Row names are not aligned.")
}



# Extract columns (m/z) of response data frame by selecting the ones with fewer zero values
# Function to filter columns with fewer zeros
filter_columns_with_fewer_zeros <- function(data, max_zeros) {
  zero_counts <- apply(data, 2, function(x) sum(x == 0))  # Count zeros in each column
  filtered_data <- data[, zero_counts <= max_zeros, drop = FALSE]  # Keep columns with zeros <= max_zeros
  return(filtered_data)
}

# Apply the function to response_data
max_zeros <- 10
response_filtered <- filter_columns_with_fewer_zeros(response_data_subset, max_zeros)


# scale
predictor_scaled <- scale(predictor_data_subset)
response_scaled <- scale(response_filtered)


# Initialize results storage
results <- data.frame(Metabolite = character(), R2 = numeric(), RMSE = numeric(), TrainingTime = numeric())

# Loop over each metabolite 
for (metabolite in colnames(response_scaled)) {
  response_vector <- response_scaled[, metabolite]
  
  # Split into test and train
  set.seed(42)
  train_index <- !(subdivideDataset(spectra = predictor_scaled,
                                   component = response_vector,
                                   method = "PCAKS",
                                   p = 0.3,
                                   type = "validation"))
  predictor_train <- predictor_scaled[train_index, ]
  response_train <- response_vector[train_index]
  predictor_test <- predictor_scaled[!train_index, ]
  response_test <- response_vector[!train_index]
  
  # Create a training dataset
  train_data <- data.frame(Response = response_train, predictor_train)
  
  # Train the PLSR model
  train_control <- trainControl(method = "cv", number = 20)
  start_time <- Sys.time()
  pls_model <- train(Response ~ ., data = train_data, method = "pls", tuneLength = 20, trControl = train_control)
  end_time <- Sys.time()
  training_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  colnames(predictor_test) <- paste0("X", colnames(predictor_test))
  
  # Make predictions and calculate metrics
  predictions <- predict(pls_model, newdata = predictor_test)
  metrics <- postResample(predictions, response_test)
  
  # Append Results
  results <- rbind(results, data.frame(
    Metabolite = metabolite,
    R2 = metrics[["Rsquared"]],
    RMSE = metrics[["RMSE"]],
    TrainingTime = training_time ))
  
}


# Save results to a CSV file
write.csv(results, "new2_PLSR_Results_LCMS.csv", row.names = FALSE)

# Print completion message
cat("PLSR analysis completed. Results saved as 'new2_PLSR_Results_LCMS.csv'.\n")

