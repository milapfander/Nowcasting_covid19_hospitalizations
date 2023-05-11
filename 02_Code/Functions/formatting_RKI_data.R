#' Function to bring data into the submission format for the nowcast hub
#' 
#' @param doa day of analysis
#' @param locations vector of german federal states or Germany, abbreviated
#' @param correct quantile correction method
#' @param save should the results be saved, default TRUE
#' @param retrospective should a retrospective evaluation be performed, default FALSE
#' 
#' @returns 
formatting_RKI_data <- function(doa, locations,
                                correct = "coverage", save = TRUE,
                                retrospective = FALSE) {

  path <- ifelse(retrospective, 
               "03_Results/RKI_results_retrospective/", 
               "03_Results/RKI_results/")
  
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
