#' function to merge all nowcasts, coverage- and pit-corrections in a given consequent period
#' @param start_date,end_date start and end date of period, must be dates
#' @param locations location vector, defaults to all 16 federal regions and Germany
#' @param retrospective logical indicating retrospective analysis, default TRUE
#' @param days number of days used for evaluation
#' 
#' @return df with columns nowcast and calculation date, age, location, realized7 after 40 days,
#'         nowcast, coverage- and pit-corrected quantiles and estimates
merge_nowcast_corrections <- function(start_date, end_date,
                       locations = c("DE", "DE-BB", "DE-BE", "DE-BW", "DE-BY", "DE-HB", "DE-HE",
                                     "DE-HH", "DE-MV", "DE-NI", "DE-NW", "DE-RP", "DE-SH", "DE-SL",
                                     "DE-SN", "DE-ST", "DE-TH"),
                       retrospective = TRUE, days = 40) {
  
  if (retrospective == TRUE) {
    retro <- "_retrospective/"
  } else {
    retro <- "/"
  }
  
  # construct dates vector
  date_vector <- seq.Date(start_date, end_date, by = 1)
  
  # iterate over all dates
  dat_complete <- lapply(date_vector, function(curr_date) {
    
    print(curr_date)
    
    # all nowcasts of a day
    dat_nowcast <- lapply(locations, function(l){
      read_csv2(paste0("03_Results/RKI_results",
                       retro, curr_date, "/nowcasting_results_",
                       l, "_", curr_date, ".csv")) %>%
        mutate(date_calc = curr_date,
               location = l) %>%
        dplyr::select(date, date_calc, age60, location, starts_with("nowcast7_"))
    }) %>%
      bind_rows() %>%
      filter(date > curr_date - days) %>%
      mutate(method = "bootstrap")
    
    # all coverage corrections of a day
    dat_coverage <- lapply(locations, function(l){
      temp <- read_csv2(paste0("03_Results/RKI_results",
                       retro, curr_date, "/coverage_correction_nowcasting_results_",
                       l, "_", curr_date, ".csv")) %>%
        mutate(date_calc = curr_date,
               location = l) %>%
        dplyr::select(date, date_calc, age60, location, starts_with("nowcast7_"))
      
      #colnames(temp) <- str_replace(colnames(temp), "nowcast7", replacement = "coverage_corr7")
      
      temp
    }) %>%
      bind_rows() %>%
      filter(date > curr_date - days) %>%
      mutate(method = "factor")
    
    # all pit corrections of a day
    dat_pit <- lapply(locations, function(l){
      temp <- read_csv2(paste0("03_Results/RKI_results",
                       retro, curr_date, "/pit_correction_nowcasting_results_",
                       l, "_", curr_date, ".csv")) %>%
        mutate(date_calc = curr_date,
               location = l) %>%
        dplyr::select(date, date_calc, age60, location, starts_with("nowcast7_"))
      
      #colnames(temp) <- str_replace(colnames(temp), "nowcast7", replacement = "pit_corr7")
      
      temp
    }) %>%
      bind_rows() %>%
      filter(date > curr_date - days) %>%
      mutate(method = "pit")
    
    # join three data frames
    dat_one_day <- bind_rows(dat_nowcast, dat_coverage, dat_pit)
    #dat_one_day <- left_join(x = dat_nowcast, y = dat_coverage,
    #                         by = c("date", "date_calc", "age60", "location"))
    #dat_one_day <- left_join(x = dat_one_day, y = dat_pit,
    #                         by = c("date", "date_calc", "age60", "location"))
  }) %>% bind_rows()
  
  # read data realized7 after 40 days
  data_real <- read_csv("01_Data/realized7_after40d.csv")
  
  # add realized7 to nowcast data by age, location and nowcast date (not calculation date)
  dat <- left_join(dat_complete, data_real, by = c("date", "age60" = "age", "location"))
  return(dat)
}


#' function to prepare aggregated evaluation data for different horizons
#' @param dat data frame from merge_corrections()
#' @param quantiles quantiles of interest, as decimal numbers
#' @return data frame with columns horizon, age, location and the share of realized values
#'  below the quantile value
prepare_evaluation_data <- function(dat, intervals = c(0.5, 0.8, 0.95)) {
  
  # Extract quantiles from intervals:
  quantiles <- sort(as.vector(vapply(intervals,
                                     function(x) {c((1 - x) / 2, 1 - (1 - x) / 2)},
                                     numeric(2))))
  
  # filter and select relevant rows/columns
  dat <- dat %>%
    mutate(horizon = date_calc - date, error = abs(nowcast7_est - realized7),
           perc_error = abs((nowcast7_est - realized7) / realized7)) %>%
    dplyr::select(date, date_calc, horizon, age = age60, location, realized7,
                  nowcast7_est, error, perc_error, method,
                  ends_with(paste0("_", quantiles)))
  
  # Prepare criteria per location, age_group, horizon and method:
  # Absolute and percentage error:
  dat_error <- dat %>% group_by(horizon, age, location, method) %>%
    mutate(perc_error = if_else(is.nan(perc_error), 0, perc_error)) %>%
    dplyr::summarise(mae = mean(error, na.rm = TRUE),
                     mape = mean(perc_error, na.rm = TRUE))
  # Coverage of prediction_intervals:
  dat_quantiles <- dat %>%
    # compare realized7 after 40d with the quantiles
    mutate(across(matches("_[[:digit:]].[[:digit:]]"), ~ realized7 <= .x)) %>%
    # group
    group_by(horizon, age, location, method) %>%
    # calculate share of realized values below the quantile value per group
    summarise(across(matches("_[[:digit:]].[[:digit:]]"), ~ sum(.x, na.rm = TRUE) / n()))
   dat_quantiles <- as.data.frame(dat_quantiles)
   dat_coverage <- matrix(data = NA,  nrow = nrow(dat_quantiles),
                          ncol = length(intervals))
   for (interval in seq_along(intervals)) {
     cols <- paste0("nowcast7_", c((1 - intervals[interval]) / 2,
                                   1 - (1 - intervals[interval]) / 2))
     dat_coverage[, interval] <- test <- dat_quantiles[, cols[2]] -
       dat_quantiles[, cols[1]]
   }
  colnames(dat_coverage) <- paste0("coverage_", intervals)
  
  # Return of results:
  dat <- cbind(dat_error, dat_coverage)
  return(dat)
}







