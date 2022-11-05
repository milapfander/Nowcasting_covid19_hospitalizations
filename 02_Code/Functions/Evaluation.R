evalNowcast <- function(doa_first = NULL, doa_last = Sys.Date(), base = "Meldedatum", 
                        location_RKI = "DE", correction = T) {
  
  library(lubridate)
  library(dplyr)
  
  cov_corr <- ifelse(correction, "coverage_correction_", "")
  
  # if (is.null(doa_first)) {
  #   filenames <- unlist(list.files("Nowcast_Hosp/03_Results/Evaluation",
  #                                  pattern = "*.csv",
  #                                  full.names = TRUE))
  #   filename_dates <- as.Date(substr(filenames, nchar(filenames) - 13, nchar(filenames) - 4))
  # 
  #   doa_first <- max(filename_dates) + days(1)
  # 
  #   old_data <- read.csv2(filenames[[which.max(filename_dates)]])
  #   old_data$X <- NULL
  #   old_data$date <- as.Date(old_data$date)
  # }
  
  # Beobachtung
  days <- seq(from = doa_first, to = doa_last - d_max, by = 1)
  now_list_recent <- list()
  
  # Evaluation & Datenaufbereitung: Nowcasting für alle 8-wöchigen Zeitabschnitte
  for (d in seq_along(days)) {
    now_dat <- read.csv2(paste0("Nowcast_Hosp/03_Results/RKI_results/", days[[d]],
                                "/", cov_corr, "nowcasting_results_", location_RKI, 
                                "_", days[[d]], ".csv"))
    
    now_list_recent[[d]] <- now_dat %>%
      filter(date == days[[d]]) %>%
      select(c("date", "age60", "nowcast_est", "nowcast_0.025", "nowcast_0.975", 
               "nowcast7_est", "nowcast7_0.025", "nowcast7_0.975"))
  }
  
  # Datensätze erstellen
  eval_data <- data.frame(do.call(rbind.data.frame, now_list_recent))
  
  # if (exists("old_data")) {
  #   eval_data <- rbind(old_data, eval_data)
  # }
  
  # new data
  data_report <- read.csv2(paste0("Nowcast_Hosp/03_Results/RKI_results/", doa_last,
                                  "/", cov_corr, "nowcasting_results_", location_RKI, 
                                  "_", doa_last, ".csv")) %>%
    select(c("date", "age60", "reported", "reported7"))
  
  # merge data
  eval_data <- merge(eval_data, data_report, by = c("date", "age60"))
  
  eval_data <- eval_data %>%
    mutate(date = as.Date(date), age60 = as.factor(age60))
  
  filename <- paste0("Nowcast_Hosp/03_Results/Evaluation/",  cov_corr, "eval_data_", location_RKI, 
                     min(eval_data$date) + days(1), "_to_", doa_last - d_max, ".csv")
  
  write.csv2(x = eval_data, file = filename)
  return(eval_data)
}


evalPlot <- function (data_eval, daily = F, base = "Meldedatum", age = "alle", 
                      location_RKI = "DE", correction = T) {
  
  library(ggplot2)
  library(dplyr)
  
  cov_corr <- ifelse(correction, "coverage_correction_", "")
  base <- ifelse(base == "Hospdatum", "Hospitalisierungsdatum", base)
  
  data <- data_eval %>% filter(age60 == age)
  
  if (daily) {
    # täglicher Vergleich
    plot <- ggplot(data = data) +
      geom_line(aes(date, nowcast_est, col = "Nowcast")) +
      geom_line(aes(date, reported, col = "Meldungen")) +
      geom_ribbon(aes(date, ymin = nowcast_0.025, ymax = nowcast_0.975),
                  fill = "cornflowerblue",
                  col = NA,
                  alpha = .4) +
      theme_bw() +
      labs(x = base, 
           y ="Hospitalisierungen") +
      scale_x_date(date_labels = "%d.%m.%y") +
      theme(axis.text = element_text(size = 14), 
            axis.title = element_text(size = 14),
            legend.position = "top") +
      scale_color_manual(name = NULL, values = c("Nowcast" = "cornflowerblue",
                                                 "Meldungen" = "black"))
    
    filename <- paste0("Nowcast_Hosp/03_Results/Evaluation/Plots/eval_plot_daily", 
                       age60,
                       "_",
                       tolower(substr(base, 1, 4)),
                       ".png")
    
  } else {
    # 7-tägiger Vergleich
    plot <- ggplot(data = data) +
      geom_line(aes(date, nowcast7_est, col = "Nowcast")) +
      geom_line(aes(date, reported7, col = "Meldungen")) +
      geom_ribbon(aes(date, ymin = nowcast7_0.025, ymax = nowcast7_0.975), 
                  fill = "cornflowerblue", 
                  col = NA,
                  alpha = .4) +
      theme_bw() +
      labs(x = base, 
           y ="7-tägige Hospitalisierungen") +
      scale_x_date(date_labels = "%d.%m.%y", date_breaks = "15 days") +
      theme(axis.text = element_text(size = 14), 
            axis.title = element_text(size = 14), 
            legend.position = "top") +
      scale_color_manual(name = NULL, values = c("Nowcast" = "cornflowerblue", 
                                                 "Meldungen" = "black"))
    
    filename <- paste0("Nowcast_Hosp/03_Results/Evaluation/Plots/eval_plot_7daily", 
                       age60,
                       "_",
                       tolower(substr(base, 1, 4)),
                       ".png")
  }
  
  ggsave(filename = filename, plot = plot)
  return(plot)
}




