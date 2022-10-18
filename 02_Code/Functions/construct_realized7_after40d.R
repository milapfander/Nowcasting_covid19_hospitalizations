# Script to generate dataset with realized values of corona infections after 40 days
# Germany has 7 age groups, the federal states only 3
# returns a df with columns date, age, location, realized7

# (code based on the functions in Intervals.R)

library(dplyr)
library(readr)
library(stringr)
library(reshape2)
setwd("Nowcast_Hosp")

# date of preprocessed data
doa <- Sys.Date()- days(1)

## construct column realized7 after 40 days
# load preprocessed data
data_real <- read_csv(paste0("01_Data/Data_RKI/COVID-19_hospitalizations_preprocessed_",
                             doa, ".csv")) %>%
  # mutate age groups of federal states to 3 groups
  mutate(age_group = case_when(age_group == "00+" ~ "alle",
                               age_group %in% c("60-79", "80+") & location != "DE" ~ "60+",
                               age_group %in% c("00-04", "05-14", "15-34", "35-59")
                                 & location != "DE" ~ "0-60",
                               TRUE ~ age_group)) %>%
  mutate(age_group = as.factor(age_group))
data_real <- reshape2::melt(data = data_real, id.vars = c("date", "age_group", "location"),
                            variable.name = "d") %>%
  filter(d != "value_>80d")
data_real$d <- as.numeric(str_replace_all(pattern = "value_(\\.)?|d", string = data_real$d,
                               replacement = ""))
data_real <- data_real %>%
  mutate(d = d + 1) %>%
  filter(d <= 40) %>%
  group_by(date, age_group, location) %>%
  dplyr::summarise(realized = sum(value)) %>%
  arrange(desc(date))

# separate german and federal state data
data_ger <- data_real %>% filter(location == "DE") %>%
  mutate(age_group = factor(age_group), location = factor(location))
data_fed <- data_real %>% filter(location != "DE") %>%
  mutate(age_group = factor(age_group), location = factor(location))

# Prepare sum over seven days, separately:
data_ger$realized7 <- numeric(nrow(data_ger))
for(i in 1:nrow(data_ger)) {
  data_ger$realized7[i] <-
    sum(data_ger$realized[i + seq(from = 0,
                                   to = length(levels(data_ger$age_group)) *
                                    length(levels(data_ger$location)) * 6,
                                   by = length(levels(data_ger$age_group)) *
                                    length(levels(data_ger$location)))])
}
data_fed$realized7 <- numeric(nrow(data_fed))
for(i in 1:nrow(data_fed)) {
  data_fed$realized7[i] <-
    sum(data_fed$realized[i + seq(from = 0,
                                  to = length(levels(data_fed$age_group)) *
                                    length(levels(data_fed$location)) * 6,
                                  by = length(levels(data_fed$age_group)) *
                                    length(levels(data_fed$location)))])
}

# join german and federal state data again
data_real <- bind_rows(data_ger, data_fed) %>%
  select(!"realized") %>%
  ungroup()
colnames(data_real)[colnames(data_real) == "age_group"] <- "age"

# save data
write_csv(data_real, file = "01_Data/realized7_after40d.csv")






