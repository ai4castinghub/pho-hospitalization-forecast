library(lubridate)
library(dplyr)
library(readr)
library(MMWRweek)
library(tidyverse)

# Load the dataset and process it efficiently
df_hhs <- read_csv('target-data/season_2024_2025/hospitalization-data.csv') %>%
  mutate(date = as_date(time, format = "%d-%m-%Y")) %>%
  arrange(date) %>%
  mutate(mmwr_week = MMWRweek(date)$MMWRweek)

# Define model names and date range
model_output_dir <- "model-output"
model_names <- list.dirs(model_output_dir, full.names = FALSE, recursive = FALSE)

# Fetch current weeks' reference data for models
reference_date <- floor_date(Sys.Date(), unit = "week") + days(6)

# Initialize WIS_all
WIS_all <- NULL

WIS <- function(df_hhs, model, date, forecast_date) {
  filename <- paste0("model-output/", model, "/", date, "-", model, ".csv")
  
  # Check if the file exists
  if (!file.exists(filename)){
    cat("File path is null for model:", model, "on date:", date, "\n")
    return(NULL)
  }
  
  # Load forecast data
  forecast <- read_csv(filename, show_col_types = FALSE)
  
  if (nrow(forecast) == 0) {
    cat("Forecast file is empty for model:", model, "\n")
    return(NULL)
  }
  
  state_vector <- c("Ontario")  
  quantiles_vector <- c(0.025, 0.1, 0.25)
  
  # Simplified lapply to inspect state processing
  df_WIS <- lapply(state_vector, function(state) {
    single_forecast <- forecast %>%
      filter(target_end_date == forecast_date, location == state, target == 'wk inc covid hosp')
    
    # Print if forecast data is missing for the state
    if (nrow(single_forecast) == 0) {
      cat("No forecast data for state:", state, "on date:", forecast_date, "\n")
      return(NULL)
    }
    
    single_true <- df_hhs %>%
      filter(time == as_date(forecast_date), geo_value == state) %>%
      pull(covid)
  
    # Print if there is no true value for the state
    if (length(single_true) == 0) {
      cat("No true value for state:", state, "on date:", forecast_date, "\n")
      return(NULL)
    }
    
    # Get the median forecast value
    median_forecast <- single_forecast %>%
      filter(output_type_id == 0.5) %>%
      pull(value)
    
    # Print if no median forecast was found
    if (length(median_forecast) == 0) {
      cat("No median forecast for state:", state, "\n")
      return(NULL)
    }
    
    # Calculate error metrics
    AE <- abs(single_true - median_forecast)
    MSE <- (single_true - median_forecast)^2
    WIS <- AE / 2
    
    cat("State:", state, "AE:", AE, "MSE:", MSE, "Initial WIS:", WIS, "\n")
    
    # Simplified quantile loop with prints
    for (quantile in quantiles_vector) {
      lower <- single_forecast %>% filter(output_type_id == quantile) %>% pull(value)
      upper <- single_forecast %>% filter(output_type_id == 1 - quantile) %>% pull(value)
      
      if (length(lower) == 0 || length(upper) == 0) {
        cat("Missing quantile data for state:", state, "quantile:", quantile, "\n")
        return(NULL)
      }
      
      WIS <- WIS + (quantile * (upper - lower) + 
                      (single_true < lower) * (lower - single_true) + 
                      (single_true > upper) * (single_true - upper))
      
      cat("Updated WIS after quantile:", quantile, "for state:", state, ":", WIS, "\n")
    }
    
    # Final WIS calculation
    WIS <- WIS / (length(quantiles_vector) + 0.5)
    
    return(data.frame(location = state, WIS = WIS, AE = AE, MSE = MSE))
  }) %>% bind_rows()
  
  # Check if df_WIS is null
  if (is.null(df_WIS)) {
    cat("No WIS results generated for model:", model, "\n")
    return(NULL)
  }
  
  # Calculate final metrics
  WIS_results <- data.frame(
    forecast_date = date,
    horizon = forecast_date,
    model = model,
    WIS = mean(df_WIS$WIS, na.rm = TRUE),
    MAE = mean(df_WIS$AE, na.rm = TRUE),
    MSE = mean(df_WIS$MSE, na.rm = TRUE)
  )
  
  # Print the final result
  cat("Final WIS:", WIS_results$WIS, "MAE:", WIS_results$MAE, "MSE:", WIS_results$MSE, "\n")
  
  return(WIS_results)
}


# Main Loop for Forecast Calculation
for (j in 0:3) {  # Forecast horizons (0 to 3 weeks)
  target_date <- as.Date(reference_date) + (j * 7)
  for (model_name in model_names) {
    cat("Reference Date:", as.character(reference_date), "| Model Name:", model_name, "| Target Date:", as.character(target_date), "\n")
    WIS_current <- WIS(df_hhs = df_hhs, model = model_name, date = as.character(reference_date), forecast_date = as.character(target_date))
    if (!is.null(WIS_current)) {
      WIS_all <- bind_rows(WIS_all, WIS_current)
    }
  }
}
