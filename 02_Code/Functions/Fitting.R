# This function performs the nowcasting described in Section 4 of the paper

#' function to perform the nowcast calculations
#' @param doa day of analysis
#' @param T.0 first registration date to be considered in the nowcast, corresponds to t = 0
#' @param d.max maximum duration time that is assumed, see Section 5
#' @param base (two options: Meldedatum or Hospdatum)
#' @param n passed to `predict_nowcast`
#' @param path data path
#' @param save should daily results be saved, default TRUE
#' @param location_RKI location, e.g. "DE" for Germany or federal states
#' @param quantiles to be extracted
#' @param age_groups (options: split60, RKI)
#' @param adjust_quantiles Should quantiles be adjusted due to realization
#'   uncertainty
#' @param save_model Should the GAM model be saved?
#' @param save_bootstrap should bootstrap evaluations be saved, default FALSE
#' @param retrospective retrospective evaluation?, default FALSE
#' 
#' @returns a data frame that contains the estimated distribution function
#' F_t(T-t) as well as the corresponding 2.5% and 97.5% quantiles

nowcasting <- function(doa, T_0, d_max, base = "Meldedatum", n = 100,
                       path = "01_Data", save = TRUE,
                       location_RKI = "DE",
                       quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975),
                       age_groups = "split60", adjust_quantiles = FALSE,
                       save_model = FALSE, save_bootstrap = FALSE,
                       retrospective = FALSE) {
  
  # Loading of packages (necessary for parallel computing):
  library(tidyverse)
  library(magrittr)
  library(lubridate)
  library(mgcv)
  library(readxl)
  library(checkmate)
  library(data.table)
  library(parallel)
  library(dplyr)
  
  doa <- doa + days(1)
  T_max <- interval(T_0, doa) %/% days(1)
  dates <- seq(from = as.Date(T_0), to = as.Date(T_0) + days(T_max) - 1,
               by = "days")
  
  # Use english locale for weekday levels:
  if (Sys.info()['sysname'] != "Windows") {
    locale_weekday <- "en_US.UTF-8"
  }
  else {
    locale_weekday <- "English"
  }
  
  # RKI data:
  # Read data:
  doa1 <- doa - days(1)
  data <- data.table(read.csv(paste0(path, "/Data_RKI/COVID-19_hospitalizations_preprocessed_", doa1, ".csv")))
  data <- data[data$age_group != "00+"]
  data$age_group <- factor(data$age_group)
  colnames(data)[[1]] <- "date"
  
  # Reformat variables:
  dat <- melt(data = data, id.vars = c("date", "location", "age_group"),
              variable.name = "d")
  dat$d <- as.numeric(gsub(pattern = "value_(\\.)?|d", x = dat$d,
                           replacement = ""))
  dat <- dat %>% filter(location == location_RKI) %>% 
    mutate(date = as.Date(date), Meldedatum_Hosp = date + d) %>%
    dplyr::rename(base = date, Altersgruppe = age_group, N_t_d = value)
  
  # Only two age groups when age_group = "split60" is specified:
  if (age_groups == "split60") {
    dat <- dat %>%
      mutate(Altersgruppe = factor(x = ifelse(test = Altersgruppe %in% c("60-79",
                                                                   "80+"),
                                           yes = "60+", no = "0-60")))
  }
  else {
    dat <- dat %>% mutate(Altersgruppe = as.factor(Altersgruppe))
  }
  
  # Replace negative values:
  dat$N_t_d[dat$N_t_d < 0 | is.na(dat$N_t_d)] <- 0
  
  # Prepare nowcast data:
  data_nowcast <- dat %>% 
    mutate(Wochentag = lubridate::wday(x = base, label = TRUE,
                                       abbr = FALSE, locale = locale_weekday),
           d = d + 1) %>%
    group_by(base, Altersgruppe, d, Meldedatum_Hosp, Wochentag) %>% 
    dplyr::summarise(N_t_d = sum(N_t_d)) %>%
    arrange(base, Meldedatum_Hosp)
  
  data_old <- data_nowcast %>% filter(base < T_0)
  data_nowcast <- data_nowcast %>% filter(d %in% 1:(d_max + 1),
                                          base %in% dates)
  
  # Add cumulative numbers:
  data_nowcast <- data_nowcast %>% group_by(base, Altersgruppe) %>%
    mutate(C_t_d = cumsum(N_t_d),
           t = as.numeric(base) - as.numeric(dates[1]) + 1)

  # some further data preparations:
  data_nowcast <- data_nowcast %>%
    mutate(Wochentag_hosp = lubridate::wday(x = Meldedatum_Hosp, label = TRUE,
                                            abbr = FALSE, locale = locale_weekday),
           t1 = t,  t2 = max(t1 - 28, 0), # break point four weeks before analysis date
           pi = NA)
  class(data_nowcast$Wochentag) <- "factor"
  class(data_nowcast$Wochentag_hosp) <- "factor"
  data_nowcast$Wochentag <- relevel(data_nowcast$Wochentag, ref = "Wednesday")
  data_nowcast$Wochentag_hosp <- relevel(data_nowcast$Wochentag_hosp, ref = "Wednesday")
  data_nowcast_model <- data_nowcast %>% filter(t + d <= T_max + 1)
  data_nowcast_new <- data_nowcast %>% filter(t + d > T_max + 1)
  
  # Fit the nowcast model and add fitted values to the data frame
  model <- gam(formula = cbind(N_t_d, C_t_d - N_t_d) ~
                 s(d, k = 5, bs = "ps", by = Altersgruppe) + t1 + t2  +
                 Wochentag + Wochentag_hosp,
               family = quasibinomial,
               data = data_nowcast_model[data_nowcast_model$d > 1, ])
  
  # Refit the model with Pearson estimates for scale parameter if scale
  # parameters seems to be too low (model does not converge):
  if (model$scale < 0.001) {
    model <- gam(formula = cbind(N_t_d, C_t_d - N_t_d) ~
                   s(d, k = 5, bs = "ps", by = Altersgruppe) + t1 + t2  +
                   Wochentag + Wochentag_hosp,
                 family = quasibinomial, control = gam.control(scale.est = "pearson"),
                 data = data_nowcast_model[data_nowcast_model$d > 1, ])
  }
  
  # Save model if requested:
  doa <- doa - days(1)
  if (save_model == TRUE) {
    if (retrospective == FALSE) {
      if (!dir.exists(paste0(path, "/../03_Results/RKI_results/", doa, "/"))) {
        dir.create(paste0(path, "/../03_Results/RKI_results/", doa, "/"))
      }
      saveRDS(object = model,
              file = paste0(path, "/../03_Results/RKI_results/", doa,
                            "/nowcasting_model_", location_RKI, "_", doa,
                            ".rds"))
                            
    }
    if (retrospective == TRUE) {
      if (!dir.exists(paste0(path, "/../03_Results/RKI_results_retrospective/", doa, "/"))) {
        dir.create(paste0(path, "/../03_Results/RKI_results_retrospective/", doa, "/"))
      }
      saveRDS(object = model,
              file = paste0(path, "/../03_Results/RKI_results_retrospective/", doa,
                            "/nowcasting_model_", location_RKI, "_", doa,
                            ".rds"))
                            
    }
  }
  
  data_nowcast_model$pi[data_nowcast_model$d > 1] <- predict.gam(object = model,
                                                                 type = "response")
  data_nowcast_model$pi <- ifelse(data_nowcast_model$pi > 1 - 1e-02,
                                  yes = 0, no = data_nowcast_model$pi)
  data_nowcast <- rbind(data_nowcast_model, data_nowcast_new)
  
  # Compute nowcasting:
  data_nowcast <- predict_nowcast(model = model, data = data_nowcast, doa = doa,
                                  T_max = T_max, quantiles = quantiles, n = n,
                                  adjust_quantiles = adjust_quantiles,
                                  path = path,
                                  retrospective = retrospective,
                                  save_bootstrap = save_bootstrap,
                                  location_RKI = location_RKI)
  
  
  #########################################
  # Report nowcasting results:
  data_report <- data_nowcast %>% group_by(base, Altersgruppe) %>%
    slice(tail(row_number(), 1)) %>%
    dplyr::select(base, Altersgruppe, C_t_d, C_estimate,
                  starts_with("C_quantile_"),
                  C7_estimate,starts_with("C7_quantile_"), F_estimate,
                  starts_with("F_quantile_")) %>%
    arrange(desc(base))
  
  data_report <- add_older_data(data_report, data_old, quantiles)
  
  # Add 7-day sum of reported values:
  data_report$Altersgruppe <- as.factor(data_report$Altersgruppe)
  data_report$reported7 <- NA
  for(i in 1:nrow(data_report)) {
    if (is.na(data_report$reported7[i])) {
      data_report$reported7[i] <-
        sum(data_report$C_t_d[i + seq(from = 0,
                                      to = length(levels(data_report$Altersgruppe)) * 6,
                                      by = length(levels(data_report$Altersgruppe)))])
    }
  }
  
  ###################################
  # Prepare output:
  data_report <- data_report %>%
    mutate_at(4:(length(quantiles) * 2 + 5), round, 1) %>%
    dplyr::select(base, Altersgruppe, C_t_d, C_estimate, starts_with("C_quantile_"),
                  reported7, C7_estimate,starts_with("C7_quantile_"),
                  F_estimate, starts_with("F_quantile_")) %>%
    filter(base >= "2020-01-28")
  colnames(data_report) <- c("date", "age60", "reported", "nowcast_est",
                             paste0("nowcast_",rep(quantiles, each=1)),
                             "reported7", "nowcast7_est", paste0("nowcast7_",rep(quantiles, each=1)),
                             "F_est", paste0("F_",rep(quantiles, each=1)))
  
  # Correct unplausibly low and high quantiles:
  data_report <- data_report %>% adjust_high_quantiles() %>% correct_zeros()

  # Save daily results:
  if (save == TRUE) {
    if (retrospective == FALSE) {
      if (!dir.exists(paste0(path, "/../03_Results/RKI_results/", doa, "/"))) {
        dir.create(paste0(path, "/../03_Results/RKI_results/", doa, "/"))
      }
      readr::write_csv2(data_report,
                        paste0(path, "/../03_Results/RKI_results/", doa,
                               "/nowcasting_results_", location_RKI, "_", doa, ".csv"))
    }
    if (retrospective == TRUE) {
      if (!dir.exists(paste0(path, "/../03_Results/RKI_results_retrospective/", doa, "/"))) {
        dir.create(paste0(path, "/../03_Results/RKI_results_retrospective/", doa, "/"))
      }
      readr::write_csv2(data_report,
                        paste0(path, "/../03_Results/RKI_results_retrospective/", doa,
                               "/nowcasting_results_", location_RKI, "_", doa, ".csv"))
    }
  }
  
  # Return nowcast results:
  return(data_report)
}

#' This function performs the actual nowcast (i.e. calculates the nowcasted
#' hospitalizations) and calculates the required quantiles via a bootstrap.
#' It is only called inside the function nowcasting(...)
#' 
#' @param model the fitted nowcast model
#' @param data a data frame that contains all the data fitted in the nowcasting
#'   model; a column that contains the fitted probabilities pi() is also included
#' @param doa day of analysis
#' @param T_max the number of considered registration dates
#' @param quantiles inherited from `nowcasting`
#' @param n number of bootstrap samples (default to n = 10 000)
#' @param adjust_quantiles Should quantiles be adjusted due to realization
#'   uncertainty
#' @param retrospective inherited from `nowcasting`
#' @param save_bootstrap inherited from `nowcasting`
#' @param location_RKI inherited from `nowcasting`
#' @param path prefix for data to save results to
#' 
#' @returns a data frame that binds the rows of data and newdata including new 
#' columns for the nowcast estimate as well as the alpha/2 and 1-alpha/2
#' quantiles

predict_nowcast <- function(model, data, doa, T_max, quantiles, n = 1000,
                            adjust_quantiles = FALSE,
                            retrospective = FALSE,
                            save_bootstrap = FALSE, location_RKI = "DE",
                            path = "01_Data") {
  
  # Extract newdata:
  newdata <- data %>% filter(t + d > T_max + 1)
  newdata$pi <- predict.gam(object = model, type = "response",
                            newdata = newdata)
  
  # Replace estimated probabilities of approx. 1:
  newdata$pi <- ifelse(newdata$pi > 1 - 1e-02, yes = 0, no = newdata$pi)
  
  # Prediction matrix:
  model_matrix <- predict.gam(object = model, type = "lpmatrix",
                              newdata = newdata)
  
  # Mean and covariance matrix of estimated model parameters:
  theta <- model$coefficients
  V <- vcov.gam(model)
  
  # Simulate from model parameters and determine pi:
  set.seed(1)
  X <- rmvn(n, theta, V)
  lp <- X %*% t(model_matrix)
  pi <- exp(lp) / (1 + exp(lp))
  pi[pi == "NaN"] <- 0 # set minimum pi to 0
  
  # Delete bootstrap iterations with probabilities of approx. 1:
  pi <- pi[apply(X = pi, MARGIN = 1, FUN = max) < 1 - 1e-01, ]
  n <- nrow(pi)
  
  # Matrix of predicted F and C
  F_sim <- C_sim <- C7_sim <- matrix(0, n, ncol(pi))
  C_sim_both <- C7_sim_both <- matrix(0, n, ncol(pi) /
                                        (length(unique(newdata$Altersgruppe))))
  
  # Add prediction bounds to the datasets
  data$C_estimate <- data$C_t_d
  newdata$C_estimate <- NA
  
  # Add predictions for 7-day numbers to the dataset:
  data$C7_estimate <- newdata$C7_estimate <- NA
  
  # Add F to the datasets:
  data$F_estimate <- newdata$F_estimate <- 1
  
  # Create datasets for sum of both age groups:
  # TODO: Spalten fehlen
  data_both <- data %>% group_by(base, d) %>%
    dplyr::summarize(N_t_d = sum(N_t_d), C_t_d = sum(C_t_d),
                     C_estimate = sum(C_estimate),
                     C7_estimate = sum(C7_estimate),
                     t = mean(t), d = mean(d)) %>%
    mutate(Altersgruppe = "alle")
  data <- bind_rows(data, data_both)
  data$Altersgruppe <- as.factor(data$Altersgruppe)
  newdata_both <- newdata %>% group_by(base, d) %>%
    dplyr::summarize(N_t_d = sum(N_t_d), C_t_d = sum(C_t_d),
                     C_estimate = sum(C_estimate),
                     C7_estimate = sum(C7_estimate),
                     t = mean(t)) %>%
    mutate(Altersgruppe = "alle")
  newdata <- bind_rows(newdata, newdata_both)
  newdata$Altersgruppe <- as.factor(newdata$Altersgruppe)
  
  # Add estimates for different dates:
  for (tt in unique(newdata$t)) {
    print(tt)
    C_estimate_sum <- 0
    for(age in levels(newdata$Altersgruppe)) {

      if(age != "alle"){
        ind_t <- which(newdata$t == tt & newdata$Altersgruppe == age)
        C_t_d <- data$C_t_d[which(data$t == tt &
                                        data$d == max(data$d[which(data$t == tt)],
                                                      na.rm = TRUE) &
                                        data$Altersgruppe == age)]
        newdata$F_estimate[ind_t] <- cumprod(1 - newdata$pi[ind_t]) 
        newdata$C_estimate[ind_t] <- C_t_d / newdata$F_estimate[ind_t]
        
        #start summing up for the all variable
        C_estimate_sum <- C_estimate_sum + newdata$C_estimate[ind_t]

        # Compute predicted C over 7 days:
        for (d in unique(newdata$d[newdata$t == tt & newdata$Altersgruppe == age])) {
          ind_d <- which(newdata$t <= tt & newdata$t >= tt - 6 &
                               newdata$d == d & newdata$Altersgruppe == age)
          newdata$C7_estimate[tail(ind_d, 1)] <- sum(newdata$C_estimate[ind_d])
          
          # Calculate sum for days without all entries in newdata:
          data_known <- data %>% filter(t + d <= T_max + 1)
          if (length(ind_d) < 7) {
            len <- 7 - length(ind_d)
            C_t_d_add <- sum(data_known$C_t_d[which(data_known$t <= tt & data_known$t >= tt - 6 &
                                                          data_known$d == d & data_known$Altersgruppe == age)])
            newdata$C7_estimate[tail(ind_d, 1)] <- newdata$C7_estimate[tail(ind_d, 1)] + C_t_d_add
          } 
        }
        
        # Compute simulated C:
        for (j in 1:n) {
          F_sim[j, ind_t] <- cumprod(1 - pi[j, ind_t])
          C_sim[j, ind_t] <- C_t_d / F_sim[j, ind_t]
          #C_sim[j, ind_t] <- ifelse(C_sim[j, ind_t] > 10000, yes = NA,
          #                             no = C_sim[j, ind_t])
          
          # Optionally draw from Poisson distribution for difference of
          # reported and estimated cases (last available row for time t):
          if (adjust_quantiles == TRUE) {
            C_sim[j, tail(ind_t, 1)] <- C_t_d +
              rpois(n = 1, lambda = C_sim[j, tail(ind_t, 1)] - C_t_d)
          }
          
          # Compute simulated C7:
          for (d in unique(newdata$d[newdata$t == tt & newdata$Altersgruppe == age])) {
            ind_d <- which(newdata$t <= tt & newdata$t >= tt - 6 &
                                 newdata$d == d & newdata$Altersgruppe == age)
            C7_sim[j, tail(ind_d, 1)] <- sum(C_sim[j, ind_d])
          }
          
          # Calculate sum for days without all entries:
          if (length(ind_d) < 7) {
            len <- 7 - length(ind_d)
            C_t_d_add <- sum(data_known$C_t_d[which(data_known$t <= tt & data_known$t >= tt - 6 &
                                                          data_known$d == d & data_known$Altersgruppe == age)])
            C7_sim[j, tail(ind_d, 1)] <- C7_sim[j, tail(ind_d, 1)] + C_t_d_add
          } 
        }
        
      }
      # alle Altersgruppen gesamt
      else {
        ind_t <- which(newdata$t == tt & newdata$Altersgruppe == age)
        newdata$C_estimate[ind_t] <- C_estimate_sum
      }
    }
  }
  newdata <- newdata %>% group_by(base,d) %>% 
    mutate(C7_estimate = ifelse(Altersgruppe=="alle", sum(C7_estimate,na.rm = TRUE),
                                C7_estimate))
  
  # Compute estimates for all age groups:
  mult_factor <- length(levels(newdata$Altersgruppe)) - 1
  for (j in 1:ncol(C_sim_both)) {
    C_sim_both[, j] <- rowSums(cbind(C_sim[, mult_factor * j - (mult_factor - 1:(mult_factor - 1))],
                                     C_sim[, mult_factor * j]), na.rm = TRUE)
    C7_sim_both[, j] <- rowSums(cbind(C7_sim[, mult_factor * j - (mult_factor - 1:(mult_factor - 1))],
                                      C7_sim[, mult_factor * j]), na.rm = TRUE)
  }

  
  # Save bootstrap results:
  bootstrap_results <- bind_rows(as.data.frame(t(C7_sim)),
                                 as.data.frame(t(C7_sim_both)))
  colnames(bootstrap_results) <- paste0("Iteration_", 1:ncol(bootstrap_results))
  bootstrap_results <- bind_cols(newdata[, c("base", "Altersgruppe")],
                                 bootstrap_results) %>%
    group_by(base, Altersgruppe) %>% slice(tail(row_number(), 1)) %>%
    arrange(desc(base), Altersgruppe)
  
  if (save_bootstrap == TRUE) {
    if (retrospective == FALSE) {
      if (!dir.exists(paste0(path, "/../03_Results/RKI_results/", doa, "/"))) {
        dir.create(paste0(path, "/../03_Results/RKI_results/", doa, "/"))
      }
      readr::write_csv2(bootstrap_results,
                        paste0(path, "/../03_Results/RKI_results/", doa,
                               "/bootstrap_results_", location_RKI, "_", doa, ".csv"))
    }
    if (retrospective == TRUE) {
      if (!dir.exists(paste0(path, "/../03_Results/RKI_results_retrospective/", doa, "/"))) {
        dir.create(paste0(path, "/../03_Results/RKI_results_retrospective/", doa, "/"))
      }
      readr::write_csv2(bootstrap_results,
                        paste0(path, "/../03_Results/RKI_results_retrospective/", doa,
                               "/bootstrap_results_", location_RKI, "_", doa, ".csv"))
    }
  }
  
  
  # Add quantiles:
  quantiles_newdata <- matrix(nrow = nrow(newdata), ncol = 3 * length(quantiles))
  colnames(quantiles_newdata) <- paste0(c("F_quantile_", "C_quantile_",
                                          "C7_quantile_"),
                                        rep(quantiles, each = 3))
  
  # Compute quantiles:
  # Individual age groups:
  for(k in 1:ncol(pi)) {
    quantiles_newdata[k, seq(from = 1, to = ncol(quantiles_newdata), by = 3)] <-
      quantile(F_sim[, k], 1 - quantiles, type = 7, na.rm = TRUE)
    quantiles_newdata[k, seq(from = 2, to = ncol(quantiles_newdata), by = 3)] <-
      quantile(C_sim[, k], quantiles, type = 7, na.rm = TRUE)
    quantiles_newdata[k, seq(from = 3, to = ncol(quantiles_newdata), by = 3)] <-
      quantile(C7_sim[, k], quantiles, type = 7, na.rm = TRUE)
  }
  # Sum of age groups:
  for (k in 1:ncol(C_sim_both)) {
    quantiles_newdata[k + ncol(pi), seq(from = 2, to = ncol(quantiles_newdata), by = 3)] <-
      quantile(C_sim_both[, k], quantiles, type = 7, na.rm = TRUE)
    quantiles_newdata[k + ncol(pi), seq(from = 3, to = ncol(quantiles_newdata), by = 3)] <-
      quantile(C7_sim_both[, k], quantiles, type = 7, na.rm = TRUE)
  }
  
  # Add to data:
  quantiles_newdata <- as.data.frame(quantiles_newdata)
  newdata <- bind_cols(newdata, quantiles_newdata)
  
  # Prepare known data:
  data <- data %>% filter(t + d <= T_max) %>%
    mutate(F_estimate = 1)
  
  # Add quantiles
  quantiles_data <- matrix(NA, nrow = nrow(data), ncol = 3 *length(quantiles))
  colnames(quantiles_data) <- paste0(c("F_quantile_", "C_quantile_",
                                       "C7_quantile_"),
                                        rep(quantiles, each = 3))
  
  # Add to data:
  quantiles_data <- as.data.frame(quantiles_data)
  data <- bind_cols(data, quantiles_data)
  data <- data %>% mutate_at(vars(starts_with("F")), ~1)

  # Prepare final data:
  data_final <- rbind(data, newdata) %>% arrange(desc(base), Meldedatum_Hosp)
  return(data_final)
}


#' This function adds older time points to the output data of the nowcasting
#' function. Used in the function `nowcasting`
#' 
#' @param data_report output data of nowcasting model
#' @param data_old data containing data with baseline dates older than T_0
#' @param quantiles inherited from `nowcasting`
#' 
#' @returns a data frame with additional (older) observations
add_older_data <- function(data_report, data_old, quantiles) {
  data_old <- data_old %>% mutate(Altersgruppe = as.factor(Altersgruppe))
  
  # Preparation of older data:
  data_old <- data_old %>%
    group_by(Altersgruppe, base) %>%
    dplyr::summarize(C_t_d = sum(N_t_d)) %>% ungroup %>%
    complete(base = seq.Date(min(base), max(base), by = "day"), Altersgruppe,
             fill = list(C_t_d = 0)) %>% distinct()
  
  data_old_both <- data_old %>% group_by(base) %>%
    dplyr::summarize(C_t_d = sum(C_t_d)) %>% mutate(Altersgruppe = "alle")
  
  # Add information about F to old data:
  data_old$F_estimate <- ifelse(test = data_old$Altersgruppe == "alle",
                                   yes = NA, no = 1)
  
  for(q in quantiles){
    data_old[,ncol(data_old) + 1] <- ifelse(test = data_old$Altersgruppe == "alle",
                                          yes = NA, no = 1)
    colnames(data_old)[ncol(data_old)] <- paste0("F_quantile_", q)
  }
  
  # Match both data sources:
  data_report <- bind_rows(data_report, data_old, data_old_both) %>%
    arrange(desc(base), Altersgruppe) %>%
    mutate(Altersgruppe = as.factor(Altersgruppe))
  
  # Insert missing values:
  data_report$C_estimate <- ifelse(test = is.na(data_report$C_estimate),
                                   yes = data_report$C_t_d,
                                   no = data_report$C_estimate)
  
  # Add seven day-calculations for older time points:
  for(i in 1:nrow(data_report)) {
    if (is.na(data_report$C7_estimate[i])) {
      data_report$C7_estimate[i] <-
        sum(data_report$C_estimate[i + seq(from = 0, to = length(levels(data_report$Altersgruppe)) * 6,
                                           by = length(levels(data_report$Altersgruppe)))])
    }
  }
  
  # Add missing values:
  for(q in quantiles){
    data_report[is.na(data_report[paste0("C_quantile_", q)]),paste0("C_quantile_", q)] <-
      data_report$C_t_d[is.na(data_report[paste0("C_quantile_", q)])]
    data_report[is.na(data_report[paste0("C7_quantile_", q)]),paste0("C7_quantile_", q)] <-
      data_report$C7_estimate[is.na(data_report[paste0("C7_quantile_", q)])]
  }

  for(q in quantiles){
    data_report[is.na(data_report[paste0("C7_quantile_", q)]), paste0("C7_quantile_", q)] <-
      data_report$C7_estimate[is.na(data_report[paste0("C7_quantile_", q)])]
  }

  # Return result data:
  return(data_report)
}


#' Function to correct unplausibly high values for upper quantiles. The
#' correction is based on the distribution of the quantiles of a normal
#' distribution.
#' 
#' @param data data frame containing nowcast results and quantiles
#' @param limit_factor constant value used in calculation, default 0.9
#' @param quantiles vector of quantiles to be corrected
#' 
#' @returns data frame with corrected quantiles
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
    data_quantiles[, i] <- ifelse(data_quantiles$quantile_ratio >
                                    normal_ratio * limit_factor,
                                  data_quantiles[, i - 1] *
                                    normal_ratio * limit_factor,
                                  data_quantiles[, i])
  }
  data_quantiles <- data_quantiles %>% dplyr::select(-quantile_ratio) %>%
    mutate_all(round, 1)
  
  # Replace quantiles in original data:
  data[, cols] <- data_quantiles
  return(data)
}


#' Function to set quantiles of 0 to reported number of hospitalizations
#' 
#' @param data data frame containing nowcast results and quantiles
#' 
#' @returns data frame with corrected quantiles
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







