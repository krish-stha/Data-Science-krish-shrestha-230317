library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
library(fmsb)



band = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Cleaned Data/Cleaned_BroadBand_Speed.csv")


bandwidth_south = band %>%
  filter(County=="SOUTH YORKSHIRE") %>% 
  group_by(District)

bandwidth_south <- bandwidth_south %>%
  mutate(`Avg_download` = as.numeric(`Avg_download`))


town_bandwidth_south <- bandwidth_south %>% 
  drop_na(`Avg_download`) %>% 
  group_by(Town) %>%
  summarise(`Average_download` = mean(`Avg_download`, na.rm = TRUE))


bandwidth_west = band %>% 
  filter(County=="WEST YORKSHIRE") %>% 
  group_by(District)

bandwidth_west <- bandwidth_west %>%
  mutate(`Avg_download` = as.numeric(`Avg_download`))


town_bandwidth_west <- bandwidth_west %>% 
  drop_na(`Avg_download`) %>% 
  group_by(Town) %>%
  summarise(`Average_download` = mean(`Avg_download`, na.rm = TRUE))

# Boxplot: South Yorkshire 
ggplot(bandwidth_south, aes(x = District, y = `Avg_download`, fill = District)) +
  geom_boxplot(outlier.size = 1, color = "darkblue") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribution of Download Speeds in South Yorkshire Districts",
    x = "District",
    y = "Download Speed (Mbps)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    legend.position = "none",
    plot.margin = margin(15, 20, 15, 20)
  )


# Boxplot: West Yorkshire
ggplot(bandwidth_west, aes(x = District, y = `Avg_download`, fill = District)) +
  geom_boxplot(outlier.size = 1, color = "darkgreen") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Download Speed Trends Across West Yorkshire Districts",
    x = "District",
    y = "Download Speed (Mbps)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    legend.position = "none",
    plot.margin = margin(15, 20, 15, 20)
  )


# Bar Chart: South Yorkshire Towns
ggplot(town_bandwidth_south, aes(x = reorder(Town, -`Average_download`), y = `Average_download`, fill = Town)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  scale_fill_manual(values = colorRampPalette(c("#FF7F0E", "#2CA02C", "#1F77B4"))(nrow(town_bandwidth_south))) +
  labs(
    title = "Average Download Speeds by Town in South Yorkshire",
    x = "Town",
    y = "Average Speed (Mbps)"
  ) +
  theme_light(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none",
    plot.margin = margin(15, 20, 15, 20)
  )


# Bar Chart: West Yorkshire Towns 
ggplot(town_bandwidth_west, aes(x = reorder(Town, -`Average_download`), y = `Average_download`, fill = Town)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  scale_fill_manual(values = colorRampPalette(c("#D62728", "#9467BD", "#8C564B"))(nrow(town_bandwidth_west))) +
  labs(
    title = "West Yorkshire Town-Wise Internet Performance",
    x = "Town",
    y = "Average Speed (Mbps)"
  ) +
  theme_light(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none",
    plot.margin = margin(15, 20, 15, 20)
  )
