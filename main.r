library(tidyverse)
covid <- read_csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_doses_admin_global.csv")

covid2 <- covid %>% filter(Province_State == "") %>% select(-c(Province_State)) %>% view()
