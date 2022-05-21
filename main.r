# Make sure tidyverse library is running and working directory is set where the csv files are
# library(tidyverse)
# setwd(<File location>)

# initial table setup
covid <- read.csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_doses_admin_global.csv")
covid2 <- covid %>% filter(Province_State == "") %>% select(-c(1:7, 9:11))
covid2 <- rename(covid2, Country = Country_Region)
covid2 <- covid2 %>% pivot_longer(cols = starts_with("X202"), names_to = "date", values_to = "shots", values_drop_na = TRUE)
covid2 <- covid2 %>% filter(shots > 0)
covid2 <- covid2 %>% mutate(Country = replace(Country, Country == "Korea, South", "South Korea"))
covid2 <- covid2 %>% group_by(Country) %>% mutate(daysSinceStart = row_number())
bed <- read.csv("data.csv", check.names=TRUE)
bed2 <- rename(bed, beds = Hospital.beds..per.10.000.population.)
bed2 <- bed2 %>% group_by(Country) %>% slice(which.max(Year)) %>% select(-c(Year))
bed2 <- bed2 %>% mutate(Country = replace(Country, Country == "Republic of Korea", "South Korea"))
bed2 <- bed2 %>% mutate(Country = replace(Country, Country == "Iran (Islamic Republic of)", "Iran"))
bed2 <- bed2 %>% mutate(Country = replace(Country, Country == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom"))
demo <- read.csv("demographics.csv", check.names=TRUE)
demo2 <- demo %>% select (-c(Country.Code, Series.Name)) %>% pivot_wider(names_from = Series.Code, values_from = YR2015) %>% group_by(Country.Name) %>% mutate(SP.POP.80UP.TOTL = as.integer(SP.POP.80UP.FE) + as.integer(SP.POP.80UP.MA), SP.POP.65UP.TOTL.IN = as.integer(SP.POP.65UP.MA.IN) + as.integer(SP.POP.65UP.FE.IN), SP.POP.1564.TOTL.IN = as.integer(SP.POP.1564.MA.IN) + as.integer(SP.POP.1564.FE.IN), SP.POP.0014.TOTL.IN = as.integer(SP.POP.0014.MA.IN) + as.integer(SP.POP.0014.FE.IN)) %>% select(Country.Name, SP.POP.80UP.TOTL, SP.POP.65UP.TOTL.IN, SP.POP.1564.TOTL.IN, SP.POP.0014.TOTL.IN)
demo2 <- rename(demo2, Country = Country.Name)
demo2 <- demo2 %>% mutate(Country = replace(Country, Country == "Republic of Korea", "South Korea"))
demo2 <- demo2 %>% mutate(Country = replace(Country, Country == "Iran, Islamic Rep.", "Iran"))
table <- covid2 %>% left_join(bed2, by = "Country")
table <- table %>% mutate(vacRate = shots/Population)
table <- table %>% left_join(demo2, by = "Country")
view(table)

# models
model1 <- lm(table$vacRate ~ table$daysSinceStart + table$Population + table$beds)
summary(model1)
model2 <- lm(table$vacRate ~ table$daysSinceStart + table$SP.POP.80UP.TOTL + table$SP.POP.65UP.TOTL.IN + table$SP.POP.1564.TOTL.IN + table$SP.POP.0014.TOTL.IN)
summary(model2)
model3 <- lm(table$vacRate ~ table$daysSinceStart + table$beds + table$SP.POP.80UP.TOTL + table$SP.POP.65UP.TOTL.IN + table$SP.POP.1564.TOTL.IN + table$SP.POP.0014.TOTL.IN)
summary(model3)
model4 <- lm(table$vacRate ~ table$daysSinceStart + table$Population + table$SP.POP.80UP.TOTL + table$SP.POP.65UP.TOTL.IN + table$SP.POP.1564.TOTL.IN + table$SP.POP.0014.TOTL.IN)
summary(model4)
model5 <- lm(table$vacRate ~ table$daysSinceStart + table$beds + table$Population + table$SP.POP.80UP.TOTL + table$SP.POP.65UP.TOTL.IN + table$SP.POP.1564.TOTL.IN + table$SP.POP.0014.TOTL.IN)
summary(model5)

# scatter plot
table2 <- table %>% select(Country, vacRate, daysSinceStart) %>% group_by(Country) %>% filter(daysSinceStart == max(daysSinceStart)) %>% na.omit() %>% view()
ggplot(table2, aes(x = daysSinceStart, y = vacRate)) + geom_point()

# bar graph
lmdf <- data.frame(Model = c('Model 1', 'Model 2', 'Model 3', 'Model 4', 'Model 5'), R2 = c(summary(model1)$r.squared, summary(model2)$r.squared, summary(model3)$r.squared, summary(model4)$r.squared, summary(model5)$r.squared))
ggplot(data = lmdf) + geom_bar(mapping = aes(x = Model, y = R2), stat = "identity")
