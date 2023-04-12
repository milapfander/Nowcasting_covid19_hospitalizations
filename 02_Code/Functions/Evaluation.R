## Create data sets for evaluation
evalNowcast <- function(doa_last = Sys.Date(), d_max = 40,  
                        location_RKI = "DE", correction = T, plot = T) {

  library(dplyr)
  
  # Looking at corrected intervals?
  cov_corr <- ifelse(correction, "coverage_correction_", "")
  
  # Filename of older evaluation data
  filename <- unlist(list.files("/03_Results",
                                pattern = paste0(cov_corr, "evaluation_data.RDS"),
                                full.names = TRUE))
  
  if (length(filename) != 0) {
    # Read old data if it exists
    old_eval_data <- readRDS(file = filename)
    
    # Set first day to next day in older data
    first_day <- max(old_eval_data$date) + 1
    
    if (first_day > doa_last - d_max) {
      # Error occurs if evaluation data already exists for the given days: doa_last lies in old
      # evalution data
      stop("The starting day is larger than the day of analysis.")
    }
    
  } else {
    # Go back 100 days as first date
    first_day <- doa_last - d_max - 99
  }
  
  # Vector of days to be evaluated
  days <- seq(from = first_day, to = doa_last - d_max, by = 1)
  now_list_recent <- list()

  for (d in seq_along(days)) {
    print(d)
    ## Nowcasted data
    # Read nowcasted data for each day
    now_file <- paste0("03_Results/RKI_results/", days[[d]],
                       "/", cov_corr, "nowcasting_results_", location_RKI, 
                       "_", days[[d]], ".csv")
    if (file.exists(now_file)) {
      now_dat <- read.csv2(now_file)
    }
    
    now_dat_day <- now_dat %>%
      filter(date == days[[d]]) %>%
      select("date", "age60", "nowcast_est", "nowcast_0.025", "nowcast_0.975", 
             "nowcast7_est", "nowcast7_0.025", "nowcast7_0.975")
    
    ## Reported data
    # Read reported data that lies 40 days back for each day
    true_file <- paste0("03_Results/RKI_results/", days[[d]] + 40,
                        "/", cov_corr, "nowcasting_results_", location_RKI, 
                        "_", days[[d]] + 40, ".csv")
    if (file.exists(true_file)) {
      true_dat <- read.csv2(true_file)
    }
    
    true_dat_day <- true_dat %>%
      filter(date == days[[d]]) %>%
      select("date", "age60", "reported", "reported7")
    
    # Merge nowcast and 'true' reported data
    now_list_recent[[d]] <- merge(now_dat_day, true_dat_day, by = c("date", "age60"))
  }
  
  # Data frame for all days of evaluation: From list of daily data frames to one big data frame
  eval_data <- data.frame(do.call(rbind.data.frame, now_list_recent)) %>%
    mutate(date = as.Date(date), age60 = as.factor(age60))
  
  if (length(filename) != 0) {
    # Add old data if it exists 
    eval_data <- rbind(old_eval_data, eval_data)
  } 
  
  # Save new evaluation data for future use
  file <- paste0("03_Results/Evaluation", cov_corr, "evaluation_data.RDS")
  saveRDS(object = eval_data, file = file)
  
  if (plot) {
    # Plot for last 100 days
    plot_data <- eval_data %>% 
      filter(date >= doa_last - d_max - 99)
    
    evalPlot(plot_data, location_RKI = location_RKI, 
             correction = correction)
  }
}


## Create plots for evaluation
evalPlot <- function (data_eval, location_RKI = "DE", correction = T) {
  
  library(ggplot2)
  
  # Looking at corrected intervals?
  cov_corr <- ifelse(correction, "coverage_correction_", "")
  
  ## Plotting
  # Daily data
  plot_daily <- ggplot(data = data_eval) +
    geom_line(aes(date, nowcast_est, col = "Nowcast")) +
    geom_line(aes(date, reported, col = "Meldungen")) +
    geom_ribbon(aes(date, ymin = nowcast_0.025, ymax = nowcast_0.975),
                fill = "cornflowerblue",
                col = NA,
                alpha = .4)
  labs(x = "Date", 
       y = "Daily hospitalizations") +
    facet_wrap(~age60, scale = "free") +
    theme_bw() +
    scale_x_date(date_labels = "%d.%m.%y") +
    scale_color_manual(name = NULL, values = c("Nowcast" = "cornflowerblue", 
                                             "Meldungen" = "black")) +
    theme(axis.text = element_text(size = 14), 
          axis.title = element_text(size = 14),
          legend.position = "top")
  
  # Data in 7 day windows
  plot_7days <- ggplot(data = data_eval) +
    geom_line(aes(date, nowcast7_est, col = "Nowcast")) +
    geom_line(aes(date, reported7, col = "Meldungen")) +
    geom_ribbon(aes(date, ymin = nowcast7_0.025, ymax = nowcast7_0.975), 
                fill = "cornflowerblue", 
                col = NA,
                alpha = .4) +
    labs(x = "Date", 
         y = "Sum pf Hospitalisierungen") +
    facet_wrap(~age60, scale = "free") +
    theme_bw() +
    scale_x_date(date_labels = "%d.%m.%y") +
    scale_color_manual(name = NULL, values = c("Nowcast" = "cornflowerblue", 
                                               "Meldungen" = "black")) +
    theme(axis.text = element_text(size = 14), 
          axis.title = element_text(size = 14),
          legend.position = "top")
  
  
  # Filenames
  #filename_daily <- 
  #  paste0("Nowcast_Hosp/03_Results/RKI_results/", max(data_eval$date), "/eval_plot_",
  #         cov_corr, "daily_", 
  #         location_RKI, "_", max(data_eval$date),
  #         ".png")
  filename_7days <- 
    paste0("03_Results/RKI_results/", max(data_eval$date), "/eval_plot_",
           cov_corr, "7days_", 
           location_RKI, "_", max(data_eval$date),
           ".png")
  
  # Save plots
  #ggsave(filename = filename_daily, plot = plot_daily)
  ggsave(filename = filename_7days, plot = plot_7days)
  
}

