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
school = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Cleaned Data/Cleaned_school_Data.csv")

house %>%
  filter(County == "WEST YORKSHIRE") %>%
  distinct(District)


# House Price vs Download Speed for both Counties

lm_band <- band %>%
  select(Avg_download, Postcode) %>%
  inner_join(house %>% select(Postcode, Price, County),
             by = "Postcode") %>%
  distinct(Postcode, .keep_all = TRUE)


ggplot(lm_band, aes(x = Avg_download, y = Price, color = County)) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.6, size = 3, shape = 16) +
  scale_color_manual(values = c("SOUTH YORKSHIRE" = "#D62828", "WEST YORKSHIRE" = "#007F5F")) +
  
  geom_smooth(method = "lm", se = TRUE, size = 1.5) +
  
  scale_y_log10(labels = scales::dollar_format(prefix = "£")) +
  scale_x_log10(labels = scales::label_number(accuracy = 1)) +
  
  coord_cartesian(xlim = c(5, 150)) +
  
  theme_minimal(base_size = 15) +
  labs(
    title = "House Price vs Download Speed by County (Log Scale)",
    x = "Download Speed (Mbit/s)",
    y = "House Price (£)",
    color = "County"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 13)
  )


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
ggplot(lm_ha, aes(x = Price, y = ATT8SCR, color = County, fill = County)) +
  geom_point(alpha = 0.7, size = 3, shape = 16) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.25, size = 1.3) +
  scale_color_manual(values = c("SOUTH YORKSHIRE" = "#D62828", "WEST YORKSHIRE" = "#007F5F")) +
  scale_fill_manual(values = c("SOUTH YORKSHIRE" = "#D62828", "WEST YORKSHIRE" = "#007F5F")) +
  scale_x_continuous(labels = scales::dollar_format(prefix = "£")) +
  labs(
    title = "Attainment 8 Score vs Average House Price (2024)",
    x = "Average House Price (GBP)",
    y = "Attainment 8 Score",
    color = "County",
    fill = "County"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 12)),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 13)
  )


correlation <- cor(lm_ha$ATT8SCR, lm_ha$Price, use = "complete.obs")
print(paste("Correlation between Attainment 8 score and House Price:", round(correlation, 3)))

model <- lm(ATT8SCR ~ Price, data = lm_ha)
summary(model)

#attainment 8 vs Drug Offense rates

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
  geom_point(alpha = 0.7, size = 4, shape = 16) +
  geom_smooth(method = "lm", se = TRUE, aes(fill = County), alpha = 0.25, size = 1.3) +
  scale_color_manual(values = c("SOUTH YORKSHIRE" = "#E76F51", "WEST YORKSHIRE" = "#2A9D8F")) +
  scale_fill_manual(values = c("SOUTH YORKSHIRE" = "#E76F51", "WEST YORKSHIRE" = "#2A9D8F")) +
  labs(
    title = "Impact of Drug Offense Rate on Average Attainment 8 Score by District",
    x = "Drug Offense Rate per 10,000 People",
    y = "Average Attainment 8 Score",
    color = "County",
    fill = "County"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 15)),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 13),
    legend.text = element_text(size = 12)
  )



correlation <- cor(lm_crime_avg$avg_scr, lm_crime_avg$drug_district, use = "complete.obs")
print(paste("Correlation between Attainment 8 score and Offense rates:", round(correlation, 3)))

model <- lm(avg_scr ~ drug_district, data = lm_crime_avg)
summary(model)


# Average Download speed vs Drug Offense Rate per 10000 people
# 
# pop_district <- towns %>% 
#   group_by(District, County) %>% 
#   select(Population2024, District, County) %>% 
#   summarise(avg_pop2024 = mean(Population2024))
# 
# drug_crime <- crime %>% 
#   filter(`Crime type` == "Drugs") %>%
#   mutate(District = toupper(District)) %>% 
#   group_by(District) %>% 
#   summarise(drug_crime_district = n()) %>% 
#   filter(drug_crime_district > 100) %>% 
#   drop_na(District)
# 
# drug_crime_rate <- drug_crime %>% 
#   inner_join(pop_district, by = "District") %>% 
#   mutate(drug_district = (drug_crime_district/avg_pop2024) * 10000)
# 
# avg_download <- band %>% 
#   group_by(District) %>% 
#   summarise(avg_band = mean(Avg_download, na.rm = TRUE))
# 
# lm_download <- drug_crime_rate %>% 
#   inner_join(avg_download, by="District")
# 
# ggplot(lm_download, aes(x = drug_district, y = avg_band, color = County)) +
#   geom_point(alpha = 0.7, size = 3.5, shape = 16) +
#   geom_smooth(method = "lm", se = TRUE, aes(fill = County), alpha = 0.25, size = 1.3) +
#   scale_color_manual(values = c("SOUTH YORKSHIRE" = "#457B9D", "WEST YORKSHIRE" = "#F4A261")) +
#   scale_fill_manual(values = c("SOUTH YORKSHIRE" = "#457B9D", "WEST YORKSHIRE" = "#F4A261")) +
#   labs(
#     title = "Average Download Speed vs Drug Offense Rate per 10,000 People",
#     x = "Drug Offense Rate per 10,000 People",
#     y = "Average Download Speed (Mbps)",
#     color = "County",
#     fill = "County"
#   ) +
#   theme_minimal(base_size = 15) +
#   theme(
#     plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 15)),
#     axis.title = element_text(face = "bold", size = 14),
#     axis.text = element_text(size = 12),
#     legend.position = "top",
#     legend.title = element_text(face = "bold", size = 13),
#     legend.text = element_text(size = 12)
#   )
# 
# correlation <- cor(lm_download$avg_band, lm_download$drug_district, use = "complete.obs")
# print(paste("Correlation between Attainment 8 score and House Price:", round(correlation, 3)))
# 
# model <- lm(avg_band ~ drug_district, data = lm_download)
# summary(model)

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
  geom_point(alpha = 0.7, size = 3.5, shape = 16) +
  geom_smooth(method = "lm", se = TRUE, aes(fill = County), alpha = 0.25, size = 1.3) +
  scale_color_manual(values = c("SOUTH YORKSHIRE" = "#264653", "WEST YORKSHIRE" = "#E9C46A")) +
  scale_fill_manual(values = c("SOUTH YORKSHIRE" = "#264653", "WEST YORKSHIRE" = "#E9C46A")) +
  labs(
    title = "Average Attainment 8 Score vs Average Download Speed",
    x = "Average Attainment 8 Score",
    y = "Average Download Speed (Mbps)",
    color = "County",
    fill = "County"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 15)),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 13),
    legend.text = element_text(size = 12)
  )


correlation <- cor(lm_band_school$avg_band, lm_band_school$avg_scr, use = "complete.obs")
print(paste("Correlation between Attainment 8 score and Average Download Speed", round(correlation, 3)))

model <- lm(avg_band ~ avg_scr, data = lm_band_school)
summary(model)






