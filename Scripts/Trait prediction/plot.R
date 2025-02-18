
# This function is created as a helper function to make the vignette less crowded.
## It takes the output from predict function and also the necessary information for the plotand generate a ggplot as output


plot_gg <- function(predictions, actual, title, x_label, y_label, 
                                     point_color = "#f03b20", line_color = "#756bb1") {
  library(ggplot2)
  
  # Convert inputs to numeric to ensure proper plotting
  actual <- as.numeric(actual)
  predictions <- as.numeric(predictions)
  
  # Create a data frame with Actual and Predicted values
  pred_df <- data.frame(
    Actual = actual,
    Predicted = predictions
  )
  
  # Generate the ggplot
  plot <- ggplot(pred_df, aes(x = Actual, y = Predicted)) +
    geom_point(color = point_color, alpha = 0.7) +
    geom_abline(slope = 1, intercept = 0, color = line_color, linetype = "dashed") +
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal()
  
  return(plot)
}

# p1 <- plot_gg(predictions_pls, Y_test, 
#                                title = "Predicted Vs Actual SLA (PLSR)",
#                                x_label = "Actual SLA", 
#                                y_label = "Predicted SLA",
#                                point_color = "#2b8cbe", 
#                                line_color = "#e34a33")
# p1
# 
