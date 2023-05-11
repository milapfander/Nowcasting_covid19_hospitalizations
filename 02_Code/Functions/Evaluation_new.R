## Create data sets for evaluation

#' function to evaluate the nowcast results
#' @param doa_last last date of data, default today
#' @param d_max maximal lag/horizon in days, default 40 days
#' @param location_RKI default "DE"
#' @param correction should coverage correction method be used for confidence
#' intervals, default TRUE
#' @param plot should a plot using `plotEval` be generated, default TRUE
#' 
#' @returns only saves the evaluation results to "03_Results/Evaluation/
#' coverage_correction_evaluation_data.RDS"
evalNowcast <- function(doa_last = Sys.Date(), d_max = 40,  
                        location_RKI = "DE", correction = T, plot = T) {
  
  library(dplyr)

  # Looking at corrected intervals?
  cov_corr <- ifelse(correction, "coverage_correction_", "")
  
  ## Nowcasted data
  # Read nowcasted data for 40 days ago
  now_file <- paste0("03_Results/RKI_results/", doa_last - d_max,
                     "/", cov_corr, "nowcasting_results_", location_RKI, 
                     "_", doa_last - d_max, ".csv")
  
  if (file.exists(now_file)) {
    now_dat <- read.csv2(now_file) %>%
      select("date", "age60", "nowcast7_est", "nowcast7_0.025", "nowcast7_0.975")
  }
  
  # Read realized reports after 40 days
  real_dat <- read.csv("03_Results/Evaluation/realized7_after40d.csv") %>%
    filter(location == location_RKI) %>%
    rename("age60" = "age")
  
  # Join nowcasted and real hospitalizations
  joined_dat <- merge(x = now_dat, y = real_dat, 
                      by = c("date", "age60"), 
                      all.x = T, all.y = F)
  
  # Vector of days to be evaluated
  days <- seq(from = doa_last - 2 * d_max, to = doa_last - d_max, by = 1)
  now_recent <- data.frame()
  
  for (d in seq_along(days)) {
    print(d)
    # Read nowcasted data for each day
    now_file <- paste0("03_Results/RKI_results/", days[[d]],
                       "/", cov_corr, "nowcasting_results_", location_RKI, 
                       "_", days[[d]], ".csv")
    
    if (file.exists(now_file)) {
      now_dat <- read.csv2(now_file)
    }
    
    # Extract reported numbers for each day
    now_dat_day <- now_dat %>%
      filter(date == days[[d]]) %>%
      select("date", "age60", "reported7")
    
    # Join to previous reported numbers
    now_recent <- rbind(now_recent, now_dat_day)
  }
  
  # Data frame of nowcasted, reported and reported after 40 days
  eval_data <- merge(x = now_recent, y = joined_dat, 
                     by = c("date", "age60"), 
                     all.x = T, all.y = F)
  
  eval_data <- eval_data %>%
    mutate(date = as.Date(date), age60 = as.factor(age60))
  
  # Save new evaluation data for future use
  file <- paste0("03_Results/Evaluation/", cov_corr, "evaluation_data.RDS")
  saveRDS(object = eval_data, file = file)
  
  # Plot
  if (plot) evalPlot(plot_data, location_RKI = location_RKI, correction = correction)
}


## Create plots for evaluation
#' function to generate a plot for the evaluation results from function 
#' `evalNowcast`
#' @param data_eval evaluation data, results from function `evalNowcast`
#' @param location_RKI default "DE"
#' @param correction should coverage correction method be used for confidence
#' intervals, default TRUE
#' 
#' @import ggplot2
#' 
#' @returns only saves the generated plot to "03_Results/RKI_results/"
evalPlot <- function (data_eval, location_RKI = "DE", correction = T) {
  
  library(ggplot2)
  
  # Looking at corrected intervals?
  cov_corr <- ifelse(correction, "coverage_correction_", "")

  ## Plotting
  # Data in 7 day windows
  plot_7days <- ggplot(data = data_eval) +
    geom_line(aes(date, nowcast7_est, col = "Nowcast")) +
    geom_ribbon(aes(date, ymin = nowcast7_0.025, ymax = nowcast7_0.975), 
                fill = "cornflowerblue", 
                col = NA,
                alpha = .4) +
    geom_line(aes(date, reported7, col = "Reports")) +
    geom_line(aes(date, realized7, col = "Reports after 40 days")) +
    labs(x = "Date", 
         y = "Sum of Hospitalisierungen") +
    facet_wrap(~age60, scale = "free") +
    theme_bw() +
    scale_x_date(date_labels = "%d.%m.%y") +
    scale_color_manual(name = NULL, values = c("Nowcast" = "cornflowerblue", 
                                               "Reports" = "black",
                                               "Reports after 40 days" = "orange")) +
    theme(axis.text = element_text(size = 14), 
          axis.title = element_text(size = 14),
          legend.position = "top")
  
  
  # Filename
  filename_7days <- 
    paste0("03_Results/RKI_results/", max(data_eval$date), "/eval_plot_",
           cov_corr, "7days_", 
           location_RKI, "_", max(data_eval$date),
           ".png")
  
  # Save plots
  ggsave(filename = filename_7days, plot = plot_7days, width = 10, height = 7)
  
}

