library(dplyr)

### Interval correction with expansion factor:
interval_correction <- function(doa, location, interval_vec = c(0.8, 0.95),
                                retrospective = FALSE) {
  
  print(location)
  date1 <- doa - 99
  date2 <- doa - 40
  dates <- as.list(seq(date1, date2, by = 1))
  
  if (retrospective == TRUE) {
    retro <- "_retrospective/"
  }
  else {
    retro <- "/"
  }
  
  # today's data
  data_today <- read_csv2(paste0("03_Results/RKI_results", 
                                 retro, doa, "/nowcasting_results_",
                                 location, "_", doa, ".csv")) %>%
    mutate(horizon = as.numeric(doa - date), age60 = as.factor(age60))
  
  # Add realized data:
  location_RKI <- location
  data_real <- read_csv(paste0("01_Data/Data_RKI/COVID-19_hospitalizations_preprocessed_",
                               doa, ".csv")) %>%
    mutate(age_group = if_else(condition = age_group == "00+", true = "alle",
                               false = age_group)) %>% 
    dplyr::filter(location == location_RKI)
  if (length(unique(data_today$age60)) == 3) {
    data_real <- data_real %>%
      mutate(age_group = case_when(age_group %in% c("60-79", "80+") ~ "60+",
                                   age_group %in% c("00-04", "05-14", "15-34",
                                                    "35-59") ~ "0-60",
                                   TRUE ~ age_group))
  }
  data_real <- data_real %>% mutate(age_group = as.factor(age_group))
  data_real <- melt(data = data_real, id.vars = c("date", "age_group", "location"),
                    variable.name = "d") %>%
    filter(d != "value_>80d")
  data_real$d <- as.numeric(gsub(pattern = "value_(\\.)?|d", x = data_real$d,
                                 replacement = ""))
  data_real <- data_real %>% mutate(d = d + 1) %>% filter(d <= 40) %>%
    group_by(date, age_group) %>% summarise(realized = sum(value)) %>%
    arrange(desc(date))
  
  # Prepare sum over seven days:
  data_real$realized7 <- NA
  for(i in 1:nrow(data_real)) {
    data_real$realized7[i] <-
      sum(data_real$realized[i + seq(from = 0,
                                     to = length(levels(data_real$age_group)) * 6,
                                     by = length(levels(data_real$age_group)))])
  }
  colnames(data_real)[2] <- "age60"
  
  # Match today's data with realized values after 40 days:
  data_today <- left_join(data_today, data_real)
  base_dat <- data_today
  
  # read data from 3 to 1.5 months ago
  data_list <- lapply(X = dates, 
                      FUN = function(x) {
                        read_csv2(paste0("03_Results/RKI_results", 
                                         retro, x, "/nowcasting_results_",
                                         location, "_", x, ".csv")) %>%
                          mutate(horizon = as.numeric(x - date)) %>%
                          filter(horizon < 40)
                        })
  
  for (interval in interval_vec) {
    quantile_colnames <- paste0("nowcast7_", 
                                c((1 - interval) / 2, 1 - (1 - interval) / 2))
    # create dataset
    data <- bind_rows(data_list) %>%
      dplyr::select(date, horizon, age60, nowcast7_est, all_of(quantile_colnames))
    
    colnames(data) <- c("date", "horizon", "age60", "nowcast7_est", "nowcast7_lwr", "nowcast7_upr")
    data <- merge(data, base_dat %>% dplyr::select(date, age60, reported7, realized7), 
                  by = c("date", "age60"), all.y = FALSE) %>%
      arrange(horizon)
    
    # get width and factor for coverage (for quantiles we should use factor = reported7 / nowcast7_x.xxx)
    data_width <- data %>% 
      rowwise() %>% 
      mutate(width_upr = nowcast7_upr - nowcast7_est,
             width_lwr = nowcast7_est - nowcast7_lwr,
             width_cover = realized7 - nowcast7_est,
             factor = if_else(condition = width_cover < 0,
                              true = abs(width_cover / width_lwr),
                              false = width_cover / width_upr)) %>%
      mutate(factor = if_else(condition = is.nan(factor), true = 0,
                              false = factor)) %>%
      filter(factor < Inf)
    
    # factor for coverage
    data_factor <- aggregate(factor ~ age60 + horizon, 
                             FUN = function(x) quantile(x, probs = interval), 
                             data = data_width)
    
    data_today <- merge(data_today, data_factor, by = c("age60", "horizon")) %>%
      arrange(horizon)
    
    colnames(data_today)[colnames(data_today) %in% quantile_colnames] <- 
      c("nowcast7_lwr", "nowcast7_upr")
    
    data_today <- data_today %>% rowwise() %>%
      mutate(nowcast7_upr_new = nowcast7_est + abs(nowcast7_est - nowcast7_upr) *
               factor,
             nowcast7_lwr_new = nowcast7_est - abs(nowcast7_est - nowcast7_lwr) *
               factor) %>%
      dplyr::select(-factor)
    
    # Correct for too large changes (partly due to integer raw values):
    data_today <- data_today %>%
      mutate(nowcast7_lwr_new = if_else(reported7 > nowcast7_lwr_new,
                                        reported7, nowcast7_lwr_new)) %>%
      mutate(nowcast7_upr = if_else(abs(nowcast7_upr - nowcast7_upr_new) >
                                    2 * abs(nowcast7_lwr - nowcast7_lwr_new),
                                    nowcast7_est + 2 * abs(nowcast7_upr -
                                                             nowcast7_est),
                                    nowcast7_upr_new),
             nowcast7_lwr = nowcast7_lwr_new) %>%
      dplyr::select(-nowcast7_upr_new, -nowcast7_lwr_new)
    
    colnames(data_today)[colnames(data_today) %in% c("nowcast7_lwr", "nowcast7_upr")] <- 
      quantile_colnames
  }
  
  data_today <- data_today %>%
    dplyr::select(-horizon) %>%
    arrange(desc(date), age60) %>%
    relocate(date)
  base_dat <- base_dat %>% filter(date < min(data_today$date)) %>%
    dplyr::select(-horizon)
  data_today <- rbind(data_today, base_dat)
  
  # Correct unplausibly low and high quantiles:
  data_today <- data_today %>% correct_zeros()
    
  write.csv2(x = data_today, 
             file = paste0("03_Results/RKI_results", 
                           retro, doa, 
                           "/coverage_correction_nowcasting_results_", location, "_", doa, ".csv"),
             row.names = FALSE)
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














