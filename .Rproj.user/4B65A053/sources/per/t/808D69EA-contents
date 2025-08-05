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
school = read_csv('Cleaned Data/cleanschool.csv')

house %>%
  filter(County == "WEST YORKSHIRE") %>%
  distinct(District)


#Bandwidth vs County

lm_band <- band %>%
  select(Avg_download, Postcode) %>%
  inner_join(house %>% select(Postcode, Price, County),
             by = "Postcode") %>%
  distinct(Postcode, .keep_all = TRUE)


ggplot(lm_band, aes(x = Avg_download, y = Price)) +
  
  geom_point(aes(color = County), alpha = 0.6) +
  scale_color_manual(values = c("SOUTH YORKSHIRE" = "red", "WEST YORKSHIRE" = "green")) +
  
  geom_smooth(data = subset(lm_band, County == "SOUTH YORKSHIRE"), method = "lm", se = FALSE, color = "blue", size = 1.2) +
  geom_smooth(data = subset(lm_band, County == "WEST YORKSHIRE"), method = "lm", se = FALSE, color = "orange", size = 1.2) +
  
  scale_y_log10() +
  scale_x_log10() +
  
  coord_cartesian(xlim = c(5, 150)) +  # adjust these limits based on your data
  
  theme_minimal() +
  labs(title = "House Price vs Download Speed by County (log scale)",
       x = "Download Speed (Mbit/s)",
       y = "House Price (£)",
       color = "County")

lm_full <- lm(Price ~ Avg_download, data = lm_band)
summary(lm_full)

cor_full <- cor(lm_band$Price, lm_band$Avg_download, use = "complete.obs")
cat("Correlation between House Price and Download Speed:", round(cor_full, 3), "\n")


#Attainment 8 vs House Prices
lm_ha <- school %>% 
  inner_join(house %>% select(Price,shortPostcode), by="shortPostcode") %>% 
  mutate(
    ATT8SCR = as.numeric(ATT8SCR),
    Price = as.numeric(Price)
  ) %>%
  filter(!is.na(ATT8SCR) & !is.na(Price)) %>% 
  distinct(URN, .keep_all = TRUE)

ggplot(lm_ha, aes(x = Price, y = ATT8SCR, color = County)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, aes(fill = County), alpha = 0.2) +
  labs(
    title = "Attainment 8 Score vs House Price (2024)",
    x = "Average House Price (GBP)",
    y = "Attainment 8 Score",
    color = "County",
    fill = "County"
  ) +
  theme_minimal()

correlation <- cor(lm_ha$ATT8SCR, lm_ha$Price, use = "complete.obs")
print(paste("Correlation between Attainment 8 score and House Price:", round(correlation, 3)))

model <- lm(ATT8SCR ~ Price, data = lm_ha)
summary(model)

#attainment 8 vs crime

pop_district <- towns %>% 
  group_by(District, County) %>% 
  select(Population2024, District, County) %>% 
  summarise(avg_pop2024 = mean(Population2024))

drug_crime <- crime %>% 
  filter(`Crime type` == "Drugs") %>%
  mutate(District = toupper(District)) %>% 
  group_by(District) %>% 
  summarise(drug_crime_district = n()) %>% 
  filter(drug_crime_district > 100) %>% 
  drop_na(District)

drug_crime_rate <- drug_crime %>% 
  inner_join(pop_district, by = "District") %>% 
  mutate(drug_district = (drug_crime_district/avg_pop2024) * 10000)

avg_8 <- school %>% 
  select(District, ATT8SCR) %>% 
  group_by(District) %>% 
  summarise(avg_scr = mean(ATT8SCR))

lm_crime_avg <- drug_crime_rate %>%
  inner_join(avg_8, by = "District")


ggplot(lm_crime_avg, aes(x = drug_district, y = avg_scr, color = County)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, aes(fill = County), alpha = 0.2) +
  labs(
    title = "Average Attainment 8 Score vs Drug rate per 10000",
    x = "Drug rate per 10,000",
    y = "Average Attainment 8 Score",
    color = "County",
    fill = "County"
  ) +
  theme_minimal()


correlation <- cor(lm_crime_avg$avg_scr, lm_crime_avg$drug_district, use = "complete.obs")
print(paste("Correlation between Attainment 8 score and House Price:", round(correlation, 3)))

model <- lm(avg_scr ~ drug_district, data = lm_crime_avg)
summary(model)


#Download vs drug

pop_district <- towns %>% 
  group_by(District, County) %>% 
  select(Population2024, District, County) %>% 
  summarise(avg_pop2024 = mean(Population2024))

drug_crime <- crime %>% 
  filter(`Crime type` == "Drugs") %>%
  mutate(District = toupper(District)) %>% 
  group_by(District) %>% 
  summarise(drug_crime_district = n()) %>% 
  filter(drug_crime_district > 100) %>% 
  drop_na(District)

drug_crime_rate <- drug_crime %>% 
  inner_join(pop_district, by = "District") %>% 
  mutate(drug_district = (drug_crime_district/avg_pop2024) * 10000)

avg_download <- band %>% 
  group_by(District) %>% 
  summarise(avg_band = mean(Avg_download, na.rm = TRUE))

lm_download <- drug_crime_rate %>% 
  inner_join(avg_download, by="District")

ggplot(lm_download, aes(x = drug_district, y = avg_band, color = County)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, aes(fill = County), alpha = 0.2) +
  labs(
    title = "Average Attainment 8 Score vs Drug rate per 10000",
    x = "Drug rate per 10,000",
    y = "Average Download Speed",
    color = "County",
    fill = "County"
  ) +
  theme_minimal()


correlation <- cor(lm_download$avg_band, lm_download$drug_district, use = "complete.obs")
print(paste("Correlation between Attainment 8 score and House Price:", round(correlation, 3)))

model <- lm(avg_band ~ drug_district, data = lm_download)
summary(model)

#Average download speed vs attainment 8

avg_download <- band %>% 
  group_by(District, County) %>% 
  summarise(avg_band = mean(Avg_download, na.rm = TRUE))

avg_8 <- school %>% 
  select(District, ATT8SCR) %>% 
  group_by(District) %>% 
  summarise(avg_scr = mean(ATT8SCR))

lm_band_school <- avg_download %>% 
  inner_join(avg_8, by = "District")

ggplot(lm_band_school, aes(x = avg_scr, y = avg_band, color = County)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, aes(fill = County), alpha = 0.2) +
  labs(
    title = "Average Attainment 8 Score vs Drug rate per 10000",
    x = "Avera attainment 8",
    y = "Average Download Speed",
    color = "County",
    fill = "County"
  ) +
  theme_minimal()


correlation <- cor(lm_band_school$avg_band, lm_band_school$avg_scr, use = "complete.obs")
print(paste("Correlation between Attainment 8 score and House Price:", round(correlation, 3)))

model <- lm(avg_band ~ avg_scr, data = lm_band_school)
summary(model)






