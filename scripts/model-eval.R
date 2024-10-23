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
#reference_date <- floor_date(Sys.Date(), unit = "week") + days(6)
reference_date <- as_date("2024-09-21")

# Initialize WIS_all
WIS_all <- NULL

region_vector <- c("Ontario","North East", "West", "East","Central","North West","Toronto")
target_vector <- c('wk inc covid hosp','wk inc flu hosp','wk inc rsv hosp')

WIS <- function(single_forecast, model, date, forecast_date, region, tid) {

  quantiles_vector <- c(0.025, 0.1, 0.25)
  df_WIS <- data.frame()
  
  single_true <- df_hhs %>%
    filter(time == as_date(forecast_date), geo_value == region) %>%
    pull(covid)
  
  if (length(single_true) == 0) {
    cat("No true value for region:", region, "on date:", forecast_date, "\n")
  }
  
  median_forecast <- single_forecast %>%
    filter(output_type_id == 0.5) %>%
    pull(value)

  
  # Calculate error metrics
  AE <- abs(single_true - median_forecast)
  MSE <- (single_true - median_forecast)^2
  WIS <- AE / 2
  
  cat("Region:", region, "AE:", AE, "MSE:", MSE, "Initial WIS:", WIS, "\n")

  for (quantile in quantiles_vector) {
    lower <- single_forecast %>% filter(output_type_id == quantile) %>% pull(value)
    upper <- single_forecast %>% filter(output_type_id == 1 - quantile) %>% pull(value)
    
    if (length(lower) == 0 || length(upper) == 0) {
      cat("Missing quantile data for region:", region, "quantile:", quantile, "\n")
      next  # Move to the next quantile
    }
    
    WIS <- WIS + (quantile * (upper - lower) + 
                    (single_true < lower) * (lower - single_true) + 
                    (single_true > upper) * (single_true - upper))
    
    cat("Updated WIS after quantile:", quantile, "for region:", region, ":", WIS, "\n")
  }
  
  WIS <- WIS / (length(quantiles_vector) + 0.5)
  
  df_WIS <- bind_rows(df_WIS, data.frame(location = region, WIS = WIS, AE = AE, MSE = MSE))
  
  if (nrow(df_WIS) == 0) {
    cat("No WIS results generated for model:", model, "\n")
    return(NULL)
  }
  
  WIS_results <- data.frame(
    reference_date = date,
    target_end_date = forecast_date,
    model = model,
    WIS = mean(df_WIS$WIS, na.rm = TRUE),
    MAE = mean(df_WIS$AE, na.rm = TRUE),
    MSE = mean(df_WIS$MSE, na.rm = TRUE),
    region = region,
    tid = tid
  )
  
  return(WIS_results)
}

# Main Loop for Forecast Calculation
for (model in model_names){
  filename <- paste0("model-output/", model, "/", reference_date, "-", model, ".csv")
  if (!file.exists(filename)){
    #cat("File path is null for model:", model, "on reference_date:", reference_date, "\n")
    next
  }
  forecast <- read_csv(filename, show_col_types = FALSE)
  
  for (region in region_vector){
    single_forecast <- forecast %>%
      filter(location == region)
    
    # Print if forecast data is missing for the region
    if (nrow(single_forecast) == 0) {
      #cat("No forecast data for state:", state,"\n")
      next  # Move to the next region
    }
    
    for (tid in target_vector){
      single_forecast <- forecast %>%
        filter(target == tid)
      
      # Print if forecast data is missing for the region
      if (nrow(single_forecast) == 0) {
        #cat("No forecast data for target:", tid,"\n")
        next  # Move to the next target
      }
      
      for (j in 0:3){
        single_forecast <- forecast %>%
          filter(horizon == j)
        
        # Print if forecast data is missing for the region
        if (nrow(single_forecast) == 0) {
          #cat("No forecast data for target:", tid,"\n")
          next  # Move to the next target
        }
        target_date <- as.Date(reference_date) + (j * 7)
        cat("Ref. Date:", as.character(reference_date), "| Model:", model, "| Target Date:", as.character(target_date), "| Region:", region,"| Target:", tid, "\n")
        WIS_current <- WIS(single_forecast = single_forecast, model = model, date = as.character(reference_date), forecast_date =  as.character(target_date),region = region, tid = tid)
        if (!is.null(WIS_current)) {
          WIS_all <- bind_rows(WIS_all, WIS_current)
        }else{
          print('File is Null')
        }
      }
    }
  }
}
