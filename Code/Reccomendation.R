library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
library(fmsb)

towns = read_csv('Cleaned Data/Towns.csv')
house = read_csv('Cleaned Data/cleanHousePrices.csv')
crime = read_csv('Cleaned Data/cleanCrimeRate.csv')
band = read_csv('Cleaned Data/cleanBand.csv')


avg_band <- band %>% 
  group_by(District) %>% 
  summarise(avg_bandwidth = mean(Avg_download, na.rm = TRUE))

avg_crime_rate <- crime %>%
  mutate(District = toupper(District)) %>% 
  group_by(District) %>% 
  summarise(avg_crime = n()) %>% 
  filter(avg_crime > 1000) %>% 
  filter(!is.na(District))

avg_house_prices <- house %>%
  filter(Year == "2024") %>% 
  group_by(District) %>% 
  summarise(avg_price = mean(Price, na.rm=TRUE))

avg_population <- towns %>% 
  group_by(District) %>% 
  summarise(
    avg_2024 = mean(Population2024, na.rm = TRUE)
  )

combined_data <- avg_band %>% 
  inner_join(avg_crime_rate, by="District") %>% 
  inner_join(avg_house_prices, by="District") %>% 
  inner_join(avg_population, by="District")

combined_data <- combined_data %>%
  mutate(across(c(avg_price, avg_crime, avg_2024, avg_bandwidth), as.numeric))


normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

combined_data <- combined_data %>%
  mutate(
    norm_bandwidth = normalize(avg_bandwidth),         # Higher is better
    norm_price = 1 - normalize(avg_price), # Lower is better
    norm_rate = 1 - normalize(avg_crime),   # Lower is better
    norm_pop = 1 - normalize(avg_2024), # Lower is better
  )

combined_data <- combined_data %>%
  mutate(
    score = 0.3 * norm_bandwidth +
      0.3 * norm_rate +
      0.3 * norm_price +
      0.1 * norm_pop
  )


combined_data <- combined_data %>%
  mutate(
    rating_score_scaled = 1 + 9 * (score - min(score)) / (max(score) - min(score)),
    rating_1_to_10 = round(rating_score_scaled, 0),
    recommendation = case_when(
      rating_1_to_10 <= 5 ~ "Not recommended",
      rating_1_to_10 > 5 & rating_1_to_10 <= 8 ~ "Recommended",
      rating_1_to_10 >= 9 ~ "Highly recommended"
    )
  ) %>% 
  arrange(desc(rating_1_to_10))

write_csv(combined_data, "Recommended System/Recommend.csv")







