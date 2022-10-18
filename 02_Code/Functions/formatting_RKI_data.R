# Function to bring data into the submission format for the nowcast hub:
formatting_RKI_data <- function(doa, locations, limit_factor = 0.9,
                                correct = "coverage", save = TRUE,
                                retrospective = FALSE) {

  path <- ifelse(retrospective, 
               "Nowcast_Hosp/03_Results/RKI_results_retrospective/", 
               "Nowcast_Hosp/03_Results/RKI_results/")
  
  # Loop over all locations:
  data_long_all <- lapply(X = locations, FUN = function(location) {
    
    # Read data:
    if (correct == "none") {
      file <- paste0(doa, "/nowcasting_results_", location,
                     "_", doa, ".csv")
    }
    if (correct == "coverage") {
      file <- paste0(doa, "/coverage_correction_nowcasting_results_", location,
                     "_", doa, ".csv")
    }
    if (correct == "pit") {
      file <- paste0(doa, "/pit_correction_nowcasting_results_", location,
                     "_", doa, ".csv")
    }
    
    data <- read_csv2(file = paste0(path, file)) %>% as.data.frame()
    
    # Correct unplausibly low and high quantiles:
    # data <- data %>% adjust_high_quantiles() %>% correct_zeros()
      
    # Restrict data to information needed:
    data <- data %>% data.table() %>%
      dplyr::select(c("date", "age60", "nowcast7_est", 
                      paste0("nowcast7_",
                             c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)))) %>%
      mutate(date = as.Date(date)) %>%
      dplyr::rename(target_end_date = date, age_group = age60)
      
    # Filter nowcasts on state level:
    if (location != "DE") {
      data <- data %>% filter(age_group == "alle")
    }
    
    # Convert to long format:
    data_long <- melt(data = data,
                      id.vars = c("target_end_date", "age_group"),
                      variable.name = "quantile")
    
    data_long <- data_long %>%
      mutate(location = location,
             quantile = as.numeric(
               ifelse(test = quantile == "nowcast7_est", 
                      yes = NA,
                      no = gsub(pattern = "nowcast7_", replacement = "", quantile))),
             age_group = ifelse(test = age_group == "alle", yes = "00+",
                                no = age_group),
             forecast_date = max(target_end_date),
             type = ifelse(test = is.na(quantile),
                           yes = "mean",
                           no = "quantile"),
             pathogen = "COVID-19")
    return(data_long)
  })
  
  # Match all files together:
  data_complete <- bind_rows(data_long_all) %>%
    mutate(target = as.numeric(gsub(pattern = " days", replacement = "", 
                                    x = forecast_date - target_end_date))) %>%
    filter(target %in% 0:28) %>%
    mutate(target = if_else(target == 0,
                            paste0(target, " day ahead inc hosp"),
                            paste0("-", target, " day ahead inc hosp")))
  data_complete <- data_complete %>% 
    dplyr::select(c("location", "age_group", "forecast_date", "target_end_date",
                    "target", "type", "quantile", "value", "pathogen"))
  
  # Remove quantiles for states with non-plausible 97.5% quantiles
  # (preliminary until this behavior is fixed in code):
  # data_complete <- data_complete %>%
  #   filter(!(location %in% c("DE-HB", "DE-SL") & type == "quantile"))
  
  # Save resulting file:
  if (save == TRUE) {
    readr::write_csv(data_complete, 
                     paste0(path, doa, "/",
                            data_complete$forecast_date[[1]], 
                            "-LMU_StaBLab-GAM_nowcast.csv"))
  }
}



# Function to correct unplausibly high values for upper quantiles. The
# correction is based on the distribution of the quantiles of a normal
# distribution:
adjust_high_quantiles <- function(data, limit_factor = 0.9,
                                   quantiles = c(0.75, 0.8, 0.85, 0.9, 0.95,
                                                 0.975)) {
  
  # Data frame with quantiles for seven day nowcasts:
  cols <- paste0("nowcast7_", quantiles)
  data_quantiles <- data %>% as.data.frame() %>% dplyr::select(all_of(cols))
  
  # Vector of quantiles of normal distribution:
  quantiles_normal <- qnorm(quantiles)
  
  # for loop through quantiles (correction starts with second quantile):
  for (i in 2:(length(quantiles))) {
    normal_ratio <- quantiles_normal[i] / quantiles_normal[i - 1]
    data_quantiles$quantile_ratio <- data_quantiles[, i] /
      data_quantiles[, i - 1]
    data_quantiles[, i] <- ifelse(test = data_quantiles$quantile_ratio >
                                    normal_ratio * limit_factor,
                                  yes = data_quantiles[, i - 1] *
                                    normal_ratio * limit_factor,
                                  no = data_quantiles[, i])
  }
  data_quantiles <- data_quantiles %>% dplyr::select(-quantile_ratio) %>%
    mutate_all(round, 1)
  
  # Replace quantiles in original data:
  data[, cols] <- data_quantiles
  return(data)
}


# Function to set quantiles of 0 to reported number of hospitalizations:
correct_zeros <- function(data) {
  
  # Data frame with reported numbers and quantiles for seven day nowcasts:
  cols <- c("reported7", colnames(data)[str_starts(colnames(data), "nowcast7_0")])
  data_check <- data %>% as.data.frame() %>%
    dplyr::select(c("reported7", starts_with("nowcast7_0")))
  data_check <- as.data.frame(apply(X = data_check, MARGIN = 2,
                      FUN = function(quantile) ifelse(test = data_check$reported7 > quantile,
                                                      yes = data_check$reported7,
                                                      no = quantile)))
  
  # Replace quantiles in original data:
  data[, cols] <- data_check
  return(data)
}




