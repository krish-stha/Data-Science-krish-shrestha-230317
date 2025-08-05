library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
library(fmsb)

towns = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Cleaned Data/Cleaned_Towns_and_postcodes.csv")
house = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Cleaned Data/Cleaned_House_sales.csv")
crime = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Cleaned Data/cleaned_crime_rate.csv")
band = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Cleaned Data/Cleaned_BroadBand_Speed.csv")


ds_band <- band %>%
  inner_join(towns %>% select(shortPostcode, District), by = "shortPostcode")

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
    norm_bandwidth = normalize(avg_bandwidth),
    norm_price = 1 - normalize(avg_price), 
    norm_rate = 1 - normalize(avg_crime),  
    norm_pop = 1 - normalize(avg_2024), 
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

write_csv(combined_data, "C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Recommended System/Recommendation_System.csv")







