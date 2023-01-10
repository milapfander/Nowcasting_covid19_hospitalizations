### Code to perform nowcasting of hospitalizations on federal state level based
### on Schneble et al. (2020):

# Loading packages and functions:
library(tidyverse)
library(magrittr)
library(lubridate)
library(mgcv)
library(readxl)
library(rgdal)
library(checkmate)
library(data.table)
library(parallel)
library(stringr)
source("Nowcast_Hosp/02_Code/Functions/Fitting.R")
source("Nowcast_Hosp/02_Code/Functions/formatting_RKI_data.R")
source("Nowcast_Hosp/02_Code/Functions/Intervals.R")
source("Nowcast_Hosp/02_Code/Functions/Evaluation.R")

# Perform RKI nowcast
doa <- Sys.Date()
#doa <- as.Date("2022-09-26")
T_0 <- doa - days(56) # data of eight weeks
d_max <- 40
locations <- c("DE", "DE-BB", "DE-BE", "DE-BW", "DE-BY", "DE-HE", # "DE-HB",
               "DE-HH", "DE-MV", "DE-NI", "DE-NW", "DE-RP", "DE-SH", #"DE-SL",
               "DE-SN", "DE-ST", "DE-TH")

# Download RKI data:
download.file(url = "https://raw.githubusercontent.com/KITmetricslab/hospitalization-nowcast-hub/main/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv",
              destfile = paste0("Nowcast_Hosp/01_Data/Data_RKI/COVID-19_hospitalizations_preprocessed_",
                                doa, ".csv"))


# Function for printing log messages:
print_logMessage <- function(diag_message, verbose_toLogFile = FALSE,
                             reset_logFile = FALSE) {
  log_file <- "Nowcast_Hosp/nowcast_progress.log"
  if (reset_logFile && file.exists(log_file)) { # make sure no old log file exists
    file.remove(log_file)
  }
  if (!file.exists(log_file)) { # create a new log file, if necessary
    file.create(log_file)
  }
  if (!verbose_toLogFile) { # print to console
    message(diag_message)
  } else { # print to log file
    write(diag_message, file = log_file, append = TRUE)
  }
}


# Perform nowcasting:

# Number of cores to use:
cores <- detectCores() - 1

# Single core (no parallelization):
if (cores == 1) {
  lapply(X = locations, FUN = function(location) {
    print(location)
    age_groups <- if_else(location == "DE", "RKI", "split60")
    nowcasting(T_0 = T_0, doa = doa, d_max = d_max, base = "Meldedatum",
               n = 100, LGL_data = FALSE, location_RKI = location,
               adjust_quantiles = TRUE, age_groups = age_groups,
               save_model = TRUE, save_bootstrap = FALSE)
  })
} else {
  
  # Multiple cores:
  print_logMessage(paste0("Run nowcast models on ", cores, " cores."),
                   verbose_toLogFile = TRUE, reset_logFile = TRUE)
  
  # Windows:
  if (Sys.info()['sysname'] == "Windows") {
    local_cluster <- makePSOCKcluster(rep("localhost", cores))
    clusterExport(cl = local_cluster,
                  varlist = c("nowcasting", "predict_nowcast",
                              "add_older_data", "doa", "days", "T_0", "d_max"),
                  envir = environment(nowcasting))
    parLapply(cl = local_cluster, X = locations, fun = function(location) {
      print(location)
      age_groups <- if_else(location == "DE", "RKI", "split60")
      nowcasting(T_0 = T_0, doa = doa, d_max = d_max, base = "Meldedatum",
                 n = 10000, LGL_data = FALSE, location_RKI = location,
                 adjust_quantiles = TRUE, age_groups = age_groups,
                 save_model = TRUE, save_bootstrap = TRUE)
    })
    stopCluster(local_cluster)
  } else {
    # Other operating systems:
    mclapply(X = locations, FUN = function(location) {
      print(location)
      age_groups <- if_else(location == "DE", "RKI", "split60")
      nowcasting(T_0 = T_0, doa = doa, d_max = d_max, base = "Meldedatum",
                 n = 10000, LGL_data = FALSE, location_RKI = location,
                 adjust_quantiles = TRUE, age_groups = age_groups,
                 save_model = TRUE, save_bootstrap = TRUE)
    },
    mc.cores = cores)
  }
}

# Enlarge intervals to hold 95% and 50% prediction intervals:
lapply(X = locations, FUN = function(location) {
  interval_correction(doa = doa, location = location,
                      interval_vec = c(0.5, 0.8, 0.95))
})
#lapply(X = locations, FUN = function(location) {
#  interval_correction_pit(doa = doa, location = location,
#                      interval_vec = c(0.5, 0.8, 0.95))
#})

# Convert results to the format of the Nowcast Hub:
formatting_RKI_data(doa, locations = locations, limit_factor = 0.9)


# Evaluation
if (cores == 1) {
  lapply(X = locations, FUN = function(location) {
    print(location)
    evalNowcast(doa_last = doa, d_max = d_max, location_RKI = locations, correction = T, plot = T)
    })
  
} else {
  
  # Multiple cores:
  print_logMessage(paste0("Run nowcast models on ", cores, " cores."),
                   verbose_toLogFile = TRUE, reset_logFile = TRUE)
  
  # Windows:
  if (Sys.info()['sysname'] == "Windows") {
    local_cluster <- makePSOCKcluster(rep("localhost", cores))
    clusterExport(cl = local_cluster,
                  varlist = c("nowcasting", "predict_nowcast",
                              "add_older_data", "doa", "days", "T_0", "d_max"),
                  envir = environment(nowcasting))
    parLapply(cl = local_cluster, X = locations, fun = function(location) {
      print(location)
      evalNowcast(doa_last = doa, d_max = d_max, location_RKI = locations, correction = T, plot = T)
    })
    stopCluster(local_cluster)
  } else {
    # Other operating systems:
    mclapply(X = locations, FUN = function(location) {
      print(location)
      evalNowcast(doa_last = doa, d_max = d_max, location_RKI = locations, correction = T, plot = T)
    },
    mc.cores = cores)
  }
}