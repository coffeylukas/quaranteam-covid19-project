library(tidyverse)
library(usmap)

setwd('~/Documents/School/VT/SocialDataComp/quaranteam-covid19-project')

# Importing the giant social factors dataset
social <- read_csv('data/social_factors_hex.csv')
counties <- read_csv('data/social_factors_county.csv')

# First I'll analyze the counties and average each county's social factors.
# ------------------------------------------------------------------------------------

social$county <- as.factor(social$county)

avg.county.risk <- social %>% 
  group_by(county) %>%
  summarise(
    avg_economic_risk = mean(economic_risk),
    avg_food_risk = mean(food_risk),
    avg_housing_risk = mean(housing_risk),
    avg_healthlit_risk = mean(healthlit_risk),
    avg_covid_19_ssi_risk = mean(covid_19_ssi)
  )

county.avgs <- social %>% 
  group_by(county) %>%
  summarise(
    avg_pop_density = mean(pop_density),
    avg_age = mean(pop_25_and_over),
    avg_exposure_score = mean(exposure_score)
  )

counties <- counties %>% 
  merge(avg.county.risk, by = "county")

ggplot(counties, aes(x = avg_covid_19_ssi_risk)) +
  # geom_point(aes(y = avg_economic_risk, color = "Economic Risk")) +
  geom_point(mapping = aes(y = county.avgs$avg_pop_density, color = "COVID-19 Risk")) +
  # geom_point(aes(y = avg_food_risk, color = "Food Risk")) +
  # geom_point(aes(y = avg_housing_risk, color = "Housing Risk")) +
  # geom_point(aes(y = avg_healthlit_risk, color = "Health Risk")) +
  labs(title = "Risk Levels vs. County Population",
       y = "Risk Level (1-5)",
       x = "Population",
       color = "Social Factor")








