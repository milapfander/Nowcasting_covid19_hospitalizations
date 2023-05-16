#' function to construct old versions of RKI data
#' 
#' @param data_RKI RKI data containing
#' @param date_retro maximum date for data to be restricted
#' @param path path to save newly created data to
#' @param save should the data be saved, default TRUE
#' 
#' @returns data frame or only writes the data to disk
retroEval <- function(data_RKI, date_retro,
                      path = paste0("01_Data/Data_RKI/COVID-19_hospitalizations_preprocessed_",
                                    date_retro, ".csv"),
                      save = T) {
  library(dplyr)
  
  data_RKI <- data_RKI %>%
    dplyr::filter(date <= date_retro) %>%
    group_by(location, age_group) %>%
    do(revert(.))
  
  if (save) {
    write.csv(x = data_RKI, file = path, row.names = FALSE)
  } else {
    data_RKI
  }
}

#' function to revert data to an older version by replacing newer observations
#' with missing values
#' 
#' @param data input data
#' 
#' @returns data frame with less observations
revert <- function (data) {
  if (nrow(data) < 81) {
    tri <- nrow(data)
  }
  else {
    tri <- 81
  }
  d <- lower.tri(diag(tri), diag = T)[, tri:1]
  d[d == 1] <- NA
  
  if (nrow(data) > 81) {
    d <- rbind(matrix(0, ncol = 81, nrow = nrow(data) - 81), d)
  } else {
    d <- d[(tri - nrow(data)):tri, ]
  }
  
  data[5:(tri + 4)] <- data[5:(tri + 4)] + d
  
  if (nrow(data) < 81) {
    data[(tri + 4 + 1):85] <- NA
  }
  return(data)
}
