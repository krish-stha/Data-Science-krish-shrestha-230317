library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
library(fmsb)

house <- read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Cleaned Data/Cleaned_House_sales.csv")

# Data aggregation
average_price <- house %>% 
  group_by(County, Year) %>% 
  summarise(Avg_Price = mean(Price), .groups = "drop")

house_price_south <- house %>% 
  filter(County == "SOUTH YORKSHIRE") %>% 
  group_by(District)

house_price_west <- house %>% 
  filter(County == "WEST YORKSHIRE") %>% 
  group_by(District)

avg_p_2024 <- house %>%
  filter(Year == "2024") %>% 
  group_by(District) %>% 
  summarise(Avg_Price_2024 = mean(Price), .groups = "drop")

# Line plot for average prices over years
ggplot(average_price, aes(x = Year, y = Avg_Price, color = County, group = County)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = c("SOUTH YORKSHIRE" = "#1f77b4", "WEST YORKSHIRE" = "#ff7f0e")) + # better contrasting colors
  scale_y_continuous(
    breaks = seq(0, max(average_price$Avg_Price), by = 10000),
    labels = comma
  ) +
  labs(
    title = "Yearly Average House Prices in Yorkshire Counties (2020–2024)",
    x = "Year",
    y = "Average Price (£)",
    color = "County"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 10)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "bottom"
  )




# Boxplot for South Yorkshire
ggplot(house_price_south, aes(x = District, y = Price, fill = District)) +
  geom_boxplot(outlier.size = 1.2, alpha = 0.75) +
  labs(
    title = "Price Distribution by District – South Yorkshire (2021–2025)",
    x = "District",
    y = "House Price (£)"
  ) +
  scale_y_continuous(
    breaks = seq(0, max(house_price_south$Price, na.rm = TRUE), by = 50000),
    labels = comma
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 10)),
    legend.position = "none"
  ) +
  coord_cartesian(
    ylim = c(
      quantile(house_price_south$Price, 0.05, na.rm = TRUE),
      quantile(house_price_south$Price, 0.95, na.rm = TRUE)
    )
  )


# Boxplot for West Yorkshire
ggplot(house_price_west, aes(x = District, y = Price, fill = District)) +
  geom_boxplot(outlier.size = 1.2, alpha = 0.75) +
  labs(
    title = "Price Distribution by District – West Yorkshire (2021–2025)",
    x = "District",
    y = "House Price (£)"
  ) +
  scale_y_continuous(
    breaks = seq(0, max(house_price_south$Price, na.rm = TRUE), by = 50000),  # same y-axis scale for comparison
    labels = comma
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 10)),
    legend.position = "none"
  ) +
  coord_cartesian(
    ylim = c(
      quantile(house_price_south$Price, 0.05, na.rm = TRUE),
      quantile(house_price_south$Price, 0.95, na.rm = TRUE)
    )
  )
