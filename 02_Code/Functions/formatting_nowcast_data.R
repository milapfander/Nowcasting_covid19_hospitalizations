library(dplyr)

format_nowcast_data <- function(url_nowcast = paste0("https://raw.githubusercontent.com/KITmetricslab/hospitalization-nowcast-hub/main/data-processed/"),
                                url_rki =  "https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Hospitalisierungen_in_Deutschland/master/Aktuell_Deutschland_COVID-19-Hospitalisierungen.csv",
                                nowcast_type = "NowcastHub-MeanEnsemble",
                                url_population = "https://raw.githubusercontent.com/KITmetricslab/hospitalization-nowcast-hub/main/other_data/population_sizes.csv",
                                target_file = NULL) {
  
  checkmate::assert_character(url_nowcast)
  checkmate::assert_character(url_rki) 
  checkmate::assert_character(url_population) 
  
  # Url nowcast for specified nowcast:
  url_nowcast_full <- paste0(url_nowcast, nowcast_type, "/", Sys.Date(), "-",
                        nowcast_type, ".csv")
  
  if(tryCatch(read.csv(url(url_nowcast_full)),error = function(e){return("fail")}) != "fail"){
    data_nowcast <- read.csv(url(url_nowcast_full))
  }
  else {
    url_nowcast_full <- paste0(url_nowcast, nowcast_type, "/", Sys.Date() - 1, "-",
                          nowcast_type, ".csv")
    data_nowcast <- read.csv(url(url_nowcast_full))
  }
  
  population_dat <- read.csv(url(url_population))
  population_dat <- population_dat %>%
    filter(age_group=="00+")
  
  #Prepare Nowcast Data for join
  data_nowcast <- data_nowcast %>%
    filter(age_group=="00+", quantile == 0.5) %>%
    left_join(population_dat[c("location","population")], by = "location") %>%
    #mutate(hosp_per_100tsd = value / population * 100000) %>%
    mutate(hosp = value) %>%
    mutate(Bundesland = case_when(location == "DE-BW" ~ "Baden-Württemberg", 
                             location == "DE-BY" ~ "Bayern",
                             location == "DE-BE" ~ "Berlin",
                             location == "DE-BB" ~ "Brandenburg",
                             location == "DE-HB" ~ "Bremen",
                             location == "DE-HH" ~ "Hamburg",
                             location == "DE-HE" ~ "Hessen",
                             location == "DE-MV" ~ "Mecklenburg-Vorpommern",
                             location == "DE-NI" ~ "Niedersachsen",
                             location == "DE-NW" ~ "Nordrhein-Westfalen",
                             location == "DE-RP" ~ "Rheinland-Pfalz",
                             location == "DE-SL" ~ "Saarland",
                             location == "DE-ST" ~ "Sachsen-Anhalt",
                             location == "DE-SN" ~ "Sachsen",
                             location == "DE-SH" ~ "Schleswig-Holstein",
                             location == "DE-TH" ~ "Thüringen",
                             location == "DE" ~ "Deutschland"))
  
  #last forecast to unstable -> remove last date
  data_nowcast$target_end_date<- as.Date(data_nowcast$target_end_date)
  max_date <- max(data_nowcast$target_end_date)-1         
  data_nowcast <- data_nowcast[data_nowcast$target_end_date<=max_date,]
  #first forecast date
  min_date <- min(data_nowcast$target_end_date)
  
  #rename date column
  names(data_nowcast)[names(data_nowcast) == 'target_end_date'] <- 'Datum'
  

  
  ############ Prepare RKI Data for join #############
  data_rki = read.csv(url(url_rki))
  data_rki$Datum <- as.Date(data_rki$Datum)
  data_rki$Bundesland[data_rki$Bundesland_Id ==8] <- "Baden-Württemberg"
  data_rki$Bundesland[data_rki$Bundesland_Id ==16] <- "Thüringen"
  data_rki$Bundesland[data_rki$Bundesland=="Bundesgebiet"] <- "Deutschland"
  data_rki <- data_rki[data_rki$Altersgruppe == "00+",]
  
  #remove rki data up to forecast
  data_rki <- data_rki[data_rki$Datum < min_date,]
  #rename value column
  names(data_rki)[names(data_rki) == 'X7T_Hospitalisierung_Faelle'] <- 'hosp'
  #names(data_rki)[names(data_rki) == 'X7T_Hospitalisierung_Inzidenz'] <- 'hosp_per_100tsd'
 
  #cols = c("Datum", "Bundesland", "hosp_per_100tsd")
  cols = c("Datum", "Bundesland", "hosp")
  data <- rbind(data_nowcast[cols],data_rki[cols]) %>%
    arrange(Bundesland, desc(Datum))

  if (is.null(target_file)) {
   return(data)
  } else {
   saveRDS(data, file = target_file)
   invisible(data)
  }
}

  
format_nowcast_data(nowcast_type = "NowcastHub-MeanEnsemble",
                    target_file = "Nowcast_Hosp/03_Results/RKI_results/bruchpunkt_nowcast_abs.rds")





