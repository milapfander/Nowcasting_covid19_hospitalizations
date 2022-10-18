evalNowcast <- function(doa_first = NULL, doa_last = Sys.Date(), d_max = 40, base = "Meldedatum", 
                        n = 1000, save = T) {
  library(tidyverse)
  library(tidyr)
  library(magrittr)
  library(lubridate)
  library(mgcv)
  library(readxl)
  library(rgdal)
  library(checkmate)
  library(ggplot2)
  library(readr)
  library(dplyr)
  
  source("Nowcast_Hosp/02_Code/Functions/Preprocessing.R")
  source("Nowcast_Hosp/02_Code/Functions/Fitting.R")

  if (is.null(doa_first)) {
    filenames <- unlist(list.files("Nowcast_Hosp/03_Results/Evaluation", 
                                   pattern = "*.csv", 
                                   full.names = TRUE))
    filename_dates <- as.Date(substr(filenames, nchar(filenames) - 13, nchar(filenames) - 4))
    
    doa_first <- max(filename_dates) + days(1)
    
    old_data <- read.csv2(filenames[[which.max(filename_dates)]])
    old_data$X <- NULL
    old_data$date <- as.Date(old_data$date)
  }
  
  # Beobachtung
  days <- seq(from = doa_first, to = doa_last - d_max, by = 1)
  now_list <- list()
  
  # Evaluation & Datenaufbereitung: Nowcasting für alle 8-wöchigen Zeitabschnitte
  for (d in seq_along(days)) {
    now_list[[d]] <- nowcasting(T_0 = days[[d]] - days(56), doa = days[[d]], d_max = d_max, 
                                base = base, save = save, n = n)
  }

  # letzten Tag des Nowcasts
  now_list_recent <- lapply(X = now_list, function(dat) {
    dat[1:3, ]
  })
  
  # Datensätze erstellen
  eval_data <- data.frame(do.call(rbind.data.frame, now_list_recent))

  if (exists("old_data")) {
    eval_data <- rbind(old_data, eval_data)
  }
  eval_data <- eval_data %>%
    mutate(date = as.Date(date), age60 = as.factor(age60))

  filename <- paste0("Nowcast_Hosp/03_Results/Evaluation/eval_data_", 
                     base, "_",
                     min(eval_data$date) + days(1), "_bis_", doa_last - d_max,
                     ".csv")
  
  write.csv2(x = eval_data, file = filename)
  return(eval_data)
}


evalPlot <- function (data_eval, daily = F, age60 = "alle", base = "Meldedatum", doa = Sys.Date()) {
  library(ggplot2)

  if (age60 == "alle") {
    age <- NULL
  } else {
    age <- ifelse(age60 == "ja", "60plus", "u60")
  }
  
  if (base == "Hospdatum") {
    base <- "Hospitalisierungsdatum"
  }
  
  data_report <- read_csv2(paste0("Nowcast_Hosp/03_Results/nowcasting_results_", 
                                   base, "_", doa, ".csv"))
  
  if(daily) {
    # täglicher Vergleich
    plot <- ggplot() +
      geom_line(aes(date, nowcast_est, col = "Nowcast"),
                data = data_eval[data_eval$age60 == age60, ]) +
      geom_line(aes(date, reported, col = "Meldungen"),
                data = data_report[data_report$age60 == age60 & 
                                     data_report$date %in% data_eval$date, ]) +
      geom_ribbon(aes(date, ymin = nowcast_lwr, ymax = nowcast_upr),
                  data = data_eval[data_eval$age60 == age60, ],
                  fill = "cornflowerblue",
                  col = NA,
                  alpha = .4) +
      theme_bw() +
      labs(x = base, 
           y ="Hospitalisierungen") +
      scale_x_date(date_labels = "%d.%m.%y") +
      theme(axis.text=element_text(size=14), axis.title = element_text(size=14),
            legend.position = "top") +
      scale_color_manual(name = NULL, values = c("Nowcast" = "cornflowerblue",
                                                 "Meldungen" = "black"))
    
    filename <- paste0("Nowcast_Hosp/03_Results/Evaluation/Plots/eval_plot_daily", 
                       age,
                       "_",
                       tolower(substr(base, 1, 4)),
                       ".png")
    
  } else {
    # 7-tägiger Vergleich
    plot <- ggplot() +
      geom_line(aes(date, nowcast7_est, col = "Nowcast"), 
                data = data_eval[data_eval$age60 == age60, ]) +
      geom_line(aes(date, reported7, col = "Meldungen"),
                data = data_report[data_report$age60 == age60 & 
                                     data_report$date %in% data_eval$date, ]) +
      geom_ribbon(aes(date, ymin = nowcast7_lwr, ymax = nowcast7_upr), 
                  data = data_eval[data_eval$age60 == age60, ], 
                  fill = "cornflowerblue", 
                  col = NA,
                  alpha = .4) +
      theme_bw() +
      labs(x = base, 
           y ="7-tägige Hospitalisierungen") +
      scale_x_date(date_labels = "%d.%m.%y", date_breaks = "15 days") +
      theme(axis.text=element_text(size=14), axis.title = element_text(size=14), 
            legend.position = "top") +
      scale_color_manual(name = NULL, values = c("Nowcast" = "cornflowerblue", 
                                                 "Meldungen" = "black"))
    
    filename <- paste0("Nowcast_Hosp/03_Results/Evaluation/Plots/eval_plot_7daily", 
                       age,
                       "_",
                       tolower(substr(base, 1, 4)),
                       ".png")
  }
  ggsave(filename = filename, plot = plot)
  return(plot)
}




