# Rscript to automatically run computation of RKI analysis script

library(tidyverse)
library(lubridate)
library(git2r)
library(data.table)

# Check current time when starting endless R script:
current_time <- now("CET")
print(current_time)
hour <- as.numeric(strftime(x = current_time, format = "%H"))
day <- as_date(now("CET"))
day <- if_else(file.exists(paste0("03_Results/RKI_results/", day,
                                  "/", day, "-LMU_StaBLab-GAM_nowcast.csv")),
               day + days(1), day)

# Endless while loop:
while(TRUE) {
  
  # Current time after execution of script:
  current_time <- now("CET")
  print(current_time)
  exec_time <- if_else(day == as_date(now()), now(),
                       ymd_hm(paste0(day, " 06:00"), tz = "CET"))
  print(exec_time)
  
  # Time duration until next execution:
  num_seconds <- time_length(x = as.duration(interval(start = current_time,
                                                      end = exec_time)),
                             unit = "second")
  print(num_seconds)
  Sys.sleep(abs(num_seconds))
  
  # Run nowcasting only when the new prepared dataset is already available:
  new_data <- FALSE
  while (new_data == FALSE) {
    download.file(url = "https://raw.githubusercontent.com/KITmetricslab/hospitalization-nowcast-hub/main/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv",
                  destfile = paste0("01_Data/Data_RKI/COVID-19_hospitalizations_preprocessed_",
                                    day, ".csv"))
    data_new <- data.table(read.csv(paste0("01_Data/Data_RKI/COVID-19_hospitalizations_preprocessed_",
                                           day, ".csv")))
    file_old <- list.files("01_Data/Data_RKI")[length(list.files("01_Data/Data_RKI")) - 1]
    data_old <- data.table(read.csv(paste0("01_Data/Data_RKI/", file_old)))
    if (data_new$date[nrow(data_new)] == data_old$date[nrow(data_old)]) {
      print("no new data available")
      # Sleep for half an hour and try again:
      Sys.sleep(1800)
    }
    else {
      new_data <- TRUE
    }
  }
  
  # Run analysis script:
  source("02_Code/Analysis_RKI_states.R")
  paste("Update finished at", now("CET"))
  
  # Automize Git process:
  # Commit results to local repository:
  cred_github <- cred_token(token = "GITHUB_PAT")
  git2r::pull(repo = getwd(), credentials = cred_github)
  git2r::add(repo = getwd(), path = file.path(getwd(), "/*"))
  git2r::commit(repo = getwd(),
         message = paste0("nowcasting results LMU_StaBLab_", as_date(now())))
  git2r::push(object = getwd(), credentials = cred_github)
  
  # Copy file to forked GitHub repository:
  file.copy(from = paste0("03_Results/RKI_results/",
                          as_date(now()), "/", as_date(now()),
                          "-LMU_StaBLab-GAM_nowcast.csv"),
            to = paste0("../hospitalization-nowcast-hub/data-processed/LMU_StaBLab-GAM_nowcast/",
                        as_date(now()), "-LMU_StaBLab-GAM_nowcast.csv"))
  
  # Commit results to forked GitHub repository:
  nowcast_repo <- paste0("../hospitalization-nowcast-hub")
  git2r::pull(repo = nowcast_repo, credentials = cred_github)
  #git2r::fetch(repo = nowcast_repo, name = "upstream") # commented out due to possible merge conflicts with automatic pull request
  #git2r::merge(x = nowcast_repo, y = "upstream/main")
  git2r::add(repo = nowcast_repo,
             path = file.path("/home/ubuntu/hospitalization-nowcast-hub/*"))
  git2r::commit(repo = nowcast_repo,
         message = paste0("nowcasting results LMU_StaBLab_", as_date(now())))
  git2r::push(object = nowcast_repo, credentials = cred_github)
  

  # Incrase day for execution time by one unit:
  if (now("CET") > exec_time) {
    day <- as_date(now("CET")) + days(1)
  }
}






