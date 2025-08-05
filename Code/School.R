library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)

school = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Cleaned Data/Cleaned_school_Data.csv")

south_yorkshire <- school %>%
  filter(County == "SOUTH YORKSHIRE") %>% 
  group_by(District)

west_yorkshire <- school %>%
  filter(County == "WEST YORKSHIRE") %>% 
  group_by(District)

district_scores <- school %>%
  group_by(District) %>%
  summarise(avg_score = mean(ATT8SCR, na.rm = TRUE)) 

ggplot(south_yorkshire, aes(x = reorder(District, ATT8SCR, FUN = median), y = ATT8SCR)) +
  geom_boxplot(fill = "#87CEFA", color = "#1E90FF", outlier.color = "red", outlier.shape = 16) +
  labs(
    title = "Distribution of Attainment 8 Scores – South Yorkshire",
    x = "District",
    y = "Attainment 8 Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


ggplot(west_yorkshire, aes(x = reorder(District, ATT8SCR, FUN = median), y = ATT8SCR)) +
  geom_boxplot(fill = "#9370DB", color = "#FF69B4", outlier.color = "red", outlier.shape = 17) +
  labs(
    title = "Distribution of Attainment 8 Scores – West Yorkshire",
    x = "District",
    y = "Attainment 8 Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



ggplot(district_scores, aes(x = reorder(District, avg_score), y = avg_score)) +
  geom_segment(aes(xend = District, y = 0, yend = avg_score), color = "gray70", linewidth = 1.2) +
  geom_point(color = "#2E8B57", size = 4) +
  labs(
    title = "Average Attainment 8 Score by District (2024)",
    x = "District",
    y = "Average Attainment 8 Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

