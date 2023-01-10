evalNowcast <- function(doa_last = Sys.Date(), d_max = 40,  
                        location_RKI = "DE", correction = T, plot = T) {
  
  library(lubridate)
  library(dplyr)
  
  
  cov_corr <- ifelse(correction, "coverage_correction_", "")
  
  filename <- unlist(list.files("Nowcast_Hosp/03_Results",
                                pattern = paste0(cov_corr, "evaluation_data.RDS"),
                                full.names = TRUE))
  
  first_day <- doa_last - d_max - 99
  
  if (length(filename) != 0) {
    old_eval_data <- readRDS(file = filename)
    first_day <- max(old_eval_data$date) + 1
    
    if (first_day > doa_last - d_max) {
      stop("The starting day is larger than the day of analysis.")
    }
  }
  
  days <- seq(from = first_day, to = doa_last - d_max, by = 1)
  now_list_recent <- list()
  browser()
  # Evaluation & Datenaufbereitung: Nowcasting für alle 8-wöchigen Zeitabschnitte
  for (d in seq_along(days)) {
    # nowcasted data
    now_dat <- read.csv2(paste0("Nowcast_Hosp/03_Results/RKI_results/", days[[d]],
                                "/", cov_corr, "nowcasting_results_", location_RKI, 
                                "_", days[[d]], ".csv"))
    
    now_dat_day <- now_dat %>%
      filter(date == days[[d]]) %>%
      select("date", "age60", "nowcast_est", "nowcast_0.025", "nowcast_0.975", 
             "nowcast7_est", "nowcast7_0.025", "nowcast7_0.975")
    
    # true data
    true_dat <- read.csv2(paste0("Nowcast_Hosp/03_Results/RKI_results/", days[[d]] + 40,
                                 "/", cov_corr, "nowcasting_results_", location_RKI, 
                                 "_", days[[d]] + 40, ".csv"))
    
    true_dat_day <- true_dat %>%
      filter(date == days[[d]]) %>%
      select("date", "age60", "reported", "reported7")
    
    now_list_recent[[d]] <- merge(now_dat_day, true_dat_day, by = c("date", "age60"))
  }
  
  # Datensätze erstellen
  eval_data <- data.frame(do.call(rbind.data.frame, now_list_recent)) %>%
    mutate(date = as.Date(date), age60 = as.factor(age60))
  
  if (length(filename) != 0) {
    eval_data <- rbind(old_eval_data, eval_data)
  } 
  
  file <- paste0("Nowcast_Hosp/03_Results/", cov_corr, "evaluation_data.RDS")
  saveRDS(object = eval_data, file = file)
  
  if (plot) {
    plot_data <- eval_data %>% 
      filter(date >= doa_last - d_max - 99)
    
    evalPlot(plot_data, location_RKI = location_RKI, 
             correction = correction)
  }
}


evalPlot <- function (data_eval, location_RKI = "DE", correction = T) {
  
  library(ggplot2)
  library(dplyr)
  
  cov_corr <- ifelse(correction, "coverage_correction_", "")
  
  # täglicher Vergleich
  plot_daily <- ggplot(data = data_eval) +
    geom_line(aes(date, nowcast_est, col = "Nowcast")) +
    geom_line(aes(date, reported, col = "Meldungen")) +
    geom_ribbon(aes(date, ymin = nowcast_0.025, ymax = nowcast_0.975),
                fill = "cornflowerblue",
                col = NA,
                alpha = .4)
  labs(x = "Datum", 
       y = "Hospitalisierungen") +
    facet_wrap(~age60, scale = "free") +
    theme_bw() +
    scale_x_date(date_labels = "%d.%m.%y") +
    scale_color_manual(name = NULL, values = c("Nowcast" = "cornflowerblue", 
                                               "Meldungen" = "black")) +
    theme(axis.text = element_text(size = 14), 
          axis.title = element_text(size = 14),
          legend.position = "top")
  
  
  # 7-tägiger Vergleich
  plot_7days <- ggplot(data = data_eval) +
    geom_line(aes(date, nowcast7_est, col = "Nowcast")) +
    geom_line(aes(date, reported7, col = "Meldungen")) +
    geom_ribbon(aes(date, ymin = nowcast7_0.025, ymax = nowcast7_0.975), 
                fill = "cornflowerblue", 
                col = NA,
                alpha = .4) +
    labs(x = "Datum", 
         y = "7-tägige Hospitalisierungen") +
    facet_wrap(~age60, scale = "free") +
    theme_bw() +
    scale_x_date(date_labels = "%d.%m.%y") +
    scale_color_manual(name = NULL, values = c("Nowcast" = "cornflowerblue", 
                                               "Meldungen" = "black")) +
    theme(axis.text = element_text(size = 14), 
          axis.title = element_text(size = 14),
          legend.position = "top")
  
  
  filename_daily <- 
    paste0("Nowcast_Hosp/03_Results/RKI_results/", max(data_eval$date), "/eval_plot_",
           cov_corr, "daily_", 
           location_RKI, "_", max(data_eval$date),
           ".png")
  
  filename_7days <- 
    paste0("Nowcast_Hosp/03_Results/RKI_results/", max(data_eval$date), "/eval_plot_",
           cov_corr, "7days_", 
           location_RKI, "_", max(data_eval$date),
           ".png")
  
  ggsave(filename = filename_daily, plot = plot_daily)
  ggsave(filename = filename_7days, plot = plot_7days)
  
}

