#' Perform Partial Least Squares Regression (PLSR)
#'
#' This function performs Partial Least Squares Regression (PLSR) on spectral data to predict a given trait.
#' The input data undergoes preprocessing with Standard Normal Variate (SNV) transformation and scaling.
#' The function splits the data into training and test sets using the PCAKS method for validation.
#' The model is trained using cross-validation, and the training time is recorded.
#'
#' @param input A matrix or data frame containing spectral data. Rows correspond to samples, and columns correspond to spectral features.
#' @param trait A numeric vector containing the trait values to be predicted. It must have the same number of rows as `input`.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{plsr_model}}{A `caret` object containing the trained PLSR model.}
#'   \item{\code{training_time}}{A `difftime` object representing the time taken to train the model.}
#'   \item{\code{Y_test}}{A numeric vector containing the scaled test trait values.}
#'   \item{\code{X_test}}{A numeric vector containing the scaled test spectral values.}
#' }
perform_PLSR <- function(input, trait) {
  library(pls)
  library(caret)
  library(plantspec)
  
  # Split the data
  dimnames(input) <- NULL
  set.seed(0)
  training_set <- !(subdivideDataset(
    spectra = input,
    component = trait,
    method = "PCAKS",
    p = 0.3,
    type = "validation"
  ))
  
  # Center and Scale the input data using Standard Normal Variate (SNV)
  SNV <- function(spectra) {
    spectra <- as.matrix(spectra)
    spectrat <- t(spectra)
    spectrat_snv <- scale(spectrat, center = TRUE, scale = TRUE)
    spectra_snv <- t(spectrat_snv)
    return(spectra_snv)
  }
  
  input <- data.frame(SNV(input))
  input <- as.matrix(input)
  
  # Scale the trait
  trait_scaled <- scale(trait)
  
  # Split data into training and test sets based on your training_set index
  X_train <- input[training_set, ]
  Y_train <- trait_scaled[training_set]
  X_test <- input[!training_set, ]
  Y_test <- trait_scaled[!training_set]
  
  # Create a data frame for the train function
  train_data <- data.frame(trait = Y_train, X_train)
  
  # Train the PLSR model with cross-validation
  train_control <- trainControl(method = "cv", number = 20)
  start_time_plsr <- Sys.time()
  plsr_model <- train(
    trait ~ ., 
    data = train_data, 
    method = "pls", 
    tuneLength = 20, 
    trControl = train_control
  )
  end_time_plsr <- Sys.time()
  
  training_time_plsr <- end_time_plsr - start_time_plsr
  
  # Return the model, training time, and Y_test
  return(list(
    plsr_model = plsr_model,
    training_time = training_time_plsr,
    Y_test = Y_test,
    X_test = X_test
  ))
}

  
# result <- perform_PLSR(input = spectral_data, trait = trait_values)
# 
## Access the outputs
# model <- result$plsr_model
# training_time <- result$training_time
# Y_test <- result$Y_test
# X_test <- result$X_test
