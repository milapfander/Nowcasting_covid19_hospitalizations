library(tidyverse)
library(lubridate)

setwd("Nowcast_Hosp")
source("02_Code/Functions/merge_corrections.R")

# Create evaluation data:
evaluation_data <- merge_nowcast_corrections(start_date = as.Date("2021-10-01"),
                                  end_date = as.Date("2022-04-30"))
head(evaluation_data)
saveRDS(evaluation_data, file = "03_Results/Evaluation/evaluation_rki.rds")

# Prepare data for aggregated evaluation:
dat <- readRDS("03_Results/Evaluation/evaluation_rki.rds")
eval_dat <- prepare_evaluation_data(dat = dat, interval = c(0.5, 0.8, 0.95))
eval_dat

# Plots for Germany:
dat_germany <- eval_dat %>%
  filter(location == "DE", age == "alle", horizon <= 28)
# MAE:
ggplot(data = dat_germany %>% filter(method == "bootstrap"),
       mapping = aes(x = desc(horizon), y = mae)) + geom_line()
# MAPE:
ggplot(data = dat_germany %>% filter(method == "bootstrap"),
       mapping = aes(x = desc(horizon), y = mape)) + geom_line()
# Coverage:
ggplot(data = dat_germany,
       mapping = aes(x = desc(horizon), y = coverage_0.95, col = method)) +
  geom_line()
ggplot(data = dat_germany,
       mapping = aes(x = desc(horizon), y = coverage_0.5, col = method)) +
  geom_line()


# Plots for age groups:
dat_age <- eval_dat %>%
  filter(location == "DE", age != "alle", horizon <= 28)
# MAE:
ggplot(data = dat_age %>% filter(method == "bootstrap"),
       mapping = aes(x = desc(horizon), y = mae, col = age)) +
  geom_line()
# MAPE:
ggplot(data = dat_age %>% filter(method == "bootstrap"),
       mapping = aes(x = desc(horizon), y = mape, col = age)) +
  geom_line()
# Coverage:
ggplot(data = dat_age %>% filter(method == "bootstrap"),
       mapping = aes(x = desc(horizon), y = coverage_0.95, col = age)) +
  geom_line()
ggplot(data = dat_age %>% filter(method == "factor"),
       mapping = aes(x = desc(horizon), y = coverage_0.95, col = age)) +
  geom_line()
ggplot(data = dat_age %>% filter(method == "pit"),
       mapping = aes(x = desc(horizon), y = coverage_0.95, col = age)) +
  geom_line()
ggplot(data = dat_age %>% filter(method == "bootstrap"),
       mapping = aes(x = desc(horizon), y = coverage_0.5, col = age)) +
  geom_line()
ggplot(data = dat_age %>% filter(method == "factor"),
       mapping = aes(x = desc(horizon), y = coverage_0.5, col = age)) +
  geom_line()
ggplot(data = dat_age %>% filter(method == "pit"),
       mapping = aes(x = desc(horizon), y = coverage_0.5, col = age)) +
  geom_line()


# Plots for states:
dat_states <- eval_dat %>%
  filter(location != "DE", age == "alle", horizon <= 28)
# MAE:
ggplot(data = dat_states %>% filter(method == "bootstrap"),
       mapping = aes(x = desc(horizon), y = mae, col = location)) +
  geom_line()
# MAPE:
ggplot(data = dat_states %>% filter(method == "bootstrap"),
       mapping = aes(x = desc(horizon), y = mape, col = location)) +
  geom_line()
# Coverage:
ggplot(data = dat_states %>% filter(method == "bootstrap"),
       mapping = aes(x = desc(horizon), y = coverage_0.95, col = location)) +
  geom_line()
ggplot(data = dat_states %>% filter(method == "factor"),
       mapping = aes(x = desc(horizon), y = coverage_0.95, col = location)) +
  geom_line()
ggplot(data = dat_states %>% filter(method == "pit"),
       mapping = aes(x = desc(horizon), y = coverage_0.95, col = location)) +
  geom_line()
ggplot(data = dat_states %>% filter(method == "bootstrap"),
       mapping = aes(x = desc(horizon), y = coverage_0.5, col = location)) +
  geom_line()
ggplot(data = dat_states %>% filter(method == "factor"),
       mapping = aes(x = desc(horizon), y = coverage_0.5, col = location)) +
  geom_line()
ggplot(data = dat_states %>% filter(method == "pit"),
       mapping = aes(x = desc(horizon), y = coverage_0.5, col = location)) +
  geom_line()


################################################################################

# Plots for Nowcast Hub:

# Prepare data for aggregated evaluation:
dat <- readRDS("03_Results/Evaluation/evaluation_rki.rds") %>%
  filter(date_calc >= as.Date("2021-11-20"))
eval_dat <- prepare_evaluation_data(dat = dat,
                                    interval = c(0.5, 0.8, 0.95)) %>%
  filter(method != "pit")
eval_dat

# Plots for Germany:
dat_germany <- eval_dat %>%
  filter(location == "DE", age == "alle", horizon <= 28)
# MAE:
ggplot(data = dat_germany %>% filter(method == "bootstrap"),
       mapping = aes(x = desc(horizon), y = mae)) + geom_line()
# MAPE:
ggplot(data = dat_germany %>% filter(method == "bootstrap"),
       mapping = aes(x = desc(horizon), y = mape)) + geom_line()
# Coverage:
ggplot(data = dat_germany,
       mapping = aes(x = desc(horizon), y = coverage_0.95, col = method)) +
  geom_line()
ggplot(data = dat_germany,
       mapping = aes(x = desc(horizon), y = coverage_0.5, col = method)) +
  geom_line()


# Plots for age groups:
dat_age <- eval_dat %>%
  filter(location == "DE", age != "alle", horizon <= 28)
# MAE:
ggplot(data = dat_age %>% filter(method == "bootstrap"),
       mapping = aes(x = desc(horizon), y = mae, col = age)) +
  geom_line()
# MAPE:
ggplot(data = dat_age %>% filter(method == "bootstrap"),
       mapping = aes(x = desc(horizon), y = mape, col = age)) +
  geom_line()
# Coverage:
ggplot(data = dat_age %>% filter(method == "bootstrap"),
       mapping = aes(x = desc(horizon), y = coverage_0.95, col = age)) +
  geom_line()
ggplot(data = dat_age %>% filter(method == "factor"),
       mapping = aes(x = desc(horizon), y = coverage_0.95, col = age)) +
  geom_line()
ggplot(data = dat_age %>% filter(method == "pit"),
       mapping = aes(x = desc(horizon), y = coverage_0.95, col = age)) +
  geom_line()
ggplot(data = dat_age %>% filter(method == "bootstrap"),
       mapping = aes(x = desc(horizon), y = coverage_0.5, col = age)) +
  geom_line()
ggplot(data = dat_age %>% filter(method == "factor"),
       mapping = aes(x = desc(horizon), y = coverage_0.5, col = age)) +
  geom_line()
ggplot(data = dat_age %>% filter(method == "pit"),
       mapping = aes(x = desc(horizon), y = coverage_0.5, col = age)) +
  geom_line()


# Plots for states:
dat_states <- eval_dat %>%
  filter(location != "DE", age == "alle", horizon <= 28)
# MAE:
ggplot(data = dat_states %>% filter(method == "bootstrap"),
       mapping = aes(x = desc(horizon), y = mae, col = location)) +
  geom_line()
# MAPE:
ggplot(data = dat_states %>% filter(method == "bootstrap"),
       mapping = aes(x = desc(horizon), y = mape, col = location)) +
  geom_line()
# Coverage:
ggplot(data = dat_states %>% filter(method == "bootstrap"),
       mapping = aes(x = desc(horizon), y = coverage_0.95, col = location)) +
  geom_line()
ggplot(data = dat_states %>% filter(method == "factor"),
       mapping = aes(x = desc(horizon), y = coverage_0.95, col = location)) +
  geom_line()
ggplot(data = dat_states %>% filter(method == "pit"),
       mapping = aes(x = desc(horizon), y = coverage_0.95, col = location)) +
  geom_line()
ggplot(data = dat_states %>% filter(method == "bootstrap"),
       mapping = aes(x = desc(horizon), y = coverage_0.5, col = location)) +
  geom_line()
ggplot(data = dat_states %>% filter(method == "factor"),
       mapping = aes(x = desc(horizon), y = coverage_0.5, col = location)) +
  geom_line()
ggplot(data = dat_states %>% filter(method == "pit"),
       mapping = aes(x = desc(horizon), y = coverage_0.5, col = location)) +
  geom_line()







