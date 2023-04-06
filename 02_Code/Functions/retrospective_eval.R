retroEval <- function(data_RKI, date_retro,
                      path = paste0("03_Results/Evaluation/retrospective/retrospective_data_RKI_",
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


