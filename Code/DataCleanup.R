library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)




price_paid_headers <- c(
  "TransactionID",     # Unique ID
  "Price",             # Price paid
  "DateOfTransfer",    # Date of sale
  "Postcode",          # Full postcode
  "PropertyType",      # D, S, T, F, O
  "OldNew",            # Y or N
  "Duration",          # F or L
  "PAON",              # Primary address object (e.g., house number)
  "SAON",              # Secondary address (e.g., flat number)
  "Street",            # Street name
  "Locality",          # Locality name
  "Town",              # Town/city
  "District",          # District (e.g., council)
  "County",            # County
  "PPDCategoryType",   # A or B
  "RecordStatus"       # A, C, D
)

# Read and assign headers for each dataset
HousePrices2020 <- read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/pp-2020.csv", col_names = FALSE)
colnames(HousePrices2020) <- price_paid_headers

HousePrices2021 <- read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/pp-2021.csv", col_names = FALSE)
colnames(HousePrices2021) <- price_paid_headers

HousePrices2022 <- read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/pp-2022.csv", col_names = FALSE)
colnames(HousePrices2022) <- price_paid_headers

HousePrices2023 <- read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/pp-2023.csv", col_names = FALSE)
colnames(HousePrices2023) <- price_paid_headers

HousePrices2024 <- read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/pp-2024.csv", col_names = FALSE)
colnames(HousePrices2024) <- price_paid_headers

PopulationData = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/Population2011.csv"
)


PopulationData = PopulationData %>%  
  mutate(Postcode = str_replace_all(Postcode, " ", "")) %>% 
  mutate(shortPostcode = str_trim(substring(Postcode, 1,4))) %>% 
  group_by(shortPostcode) %>%
  summarise_at(vars(Population), list(Population2011 = sum)) %>% 
  mutate(Population2012 = 1.00695353132322269 * Population2011) %>%
  mutate(Population2013 = 1.00669740535540783 * Population2012) %>%
  mutate(Population2014 = 1.00736463978721671 * Population2013) %>%
  mutate(Population2015 = 1.00792367505802859 * Population2014) %>%
  mutate(Population2016 = 1.00757874492811929 * Population2015) %>%
  mutate(Population2017 = 1.00679374473924223 * Population2016) %>%
  mutate(Population2018 = 1.00605929132212552 * Population2017) %>%
  mutate(Population2019 = 1.00561255390388033 * Population2018) %>%
  mutate(Population2020 = 1.00561255390388033 * Population2019) %>%
  mutate(Population2021 = 1.005425 * Population2020) %>%
  mutate(Population2022 = 1.004920 * Population2021) %>%
  mutate(Population2023 = 1.004510 * Population2022) %>%
  mutate(Population2024 = 1.004220 * Population2023) %>% 
  select(shortPostcode,Population2020,Population2021,Population2022,Population2023,Population2024)


HousePrices = HousePrices2020 %>% 
  add_row(HousePrices2021) %>% 
  add_row(HousePrices2022) %>% 
  add_row(HousePrices2023) %>% 
  add_row(HousePrices2024)

HousePrices <- bind_rows(
  HousePrices2020,
  HousePrices2021,
  HousePrices2022,
  HousePrices2023,
  HousePrices2024
)


Towns = HousePrices %>%
  filter(County=="SOUTH YORKSHIRE"|County=="WEST YORKSHIRE") %>%
  mutate(Postcode = str_replace_all(Postcode, " ", "")) %>%
  mutate(shortPostcode = str_trim(substring(Postcode, 1,4))) %>% 
  left_join(PopulationData,by = "shortPostcode") %>% 
  select(shortPostcode,Town,District,County,Population2020,Population2021,Population2022,Population2023,Population2024) %>% 
  group_by(shortPostcode) %>% 
  filter(row_number()==1) %>%
  drop_na(Population2020, Population2021, Population2022, Population2023, Population2024) %>% 
  arrange(County)

cleanHousePrices = HousePrices %>%
  filter(County=="SOUTH YORKSHIRE"|County=="WEST YORKSHIRE") %>%
  mutate(Postcode = str_replace_all(Postcode, " ", "")) %>% 
  mutate(shortPostcode = str_trim(substring(Postcode, 1,4))) %>% 
  mutate(Year = year(DateOfTransfer)) %>% 
  arrange(County) %>% 
  select(Postcode,shortPostcode,Price,Year,PropertyType, District, Town, County)

write.csv(Towns, "C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Cleaned Data/Cleaned_Towns_and_postcodes.csv", row.names = FALSE)
write.csv(cleanHousePrices,"C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Cleaned Data/Cleaned_House_sales.csv", row.names = FALSE)



crimeRate04s = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2024-04-south-yorkshire-street.csv")
crimeRate04n = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2024-04-west-yorkshire-street.csv")

crimeRate03s = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2025-03-south-yorkshire-street.csv")
crimeRate03n = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2025-03-west-yorkshire-street.csv")

crimeRate14s = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2025-04-south-yorkshire-street.csv")
crimeRate14n = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2025-04-west-yorkshire-street.csv")

crimeRate05s = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2024-05-south-yorkshire-street.csv")
crimeRate05n = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2024-05-west-yorkshire-street.csv")

crimeRate06s = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2024-06-south-yorkshire-street.csv")
crimeRate06n = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2024-06-west-yorkshire-street.csv")

crimeRate07s = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2024-07-south-yorkshire-street.csv")
crimeRate07n = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2024-07-west-yorkshire-street.csv")

crimeRate08s = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2024-08-south-yorkshire-street.csv")
crimeRate08n = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2024-08-west-yorkshire-street.csv")

crimeRate09s = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2024-09-south-yorkshire-street.csv")
crimeRate09n = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2024-09-west-yorkshire-street.csv")

crimeRate10s = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2024-10-south-yorkshire-street.csv")
crimeRate10n = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2024-10-west-yorkshire-street.csv")

crimeRate11s = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2024-11-south-yorkshire-street.csv")
crimeRate11n = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2024-11-west-yorkshire-street.csv")

crimeRate12s = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2024-12-south-yorkshire-street.csv")
crimeRate12n = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2024-12-west-yorkshire-street.csv")

crimeRate01s = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2025-01-south-yorkshire-street.csv")
crimeRate01n = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2025-01-west-yorkshire-street.csv")

crimeRate02s = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2025-02-south-yorkshire-street.csv")
crimeRate02n = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/2025-02-west-yorkshire-street.csv")

p2l = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/P2L.csv")



TotalCrime <- bind_rows(crimeRate02n,crimeRate02s, crimeRate01n, crimeRate01s, crimeRate12n, 
                        crimeRate12s, crimeRate03n, crimeRate03s, crimeRate04n, crimeRate04s,
                        crimeRate14n, crimeRate14s, crimeRate11n, crimeRate11s, crimeRate05n,
                        crimeRate05s, crimeRate06n, crimeRate07s, crimeRate08n, crimeRate08s,
                        crimeRate09n, crimeRate09s, crimeRate10n, crimeRate10s)
lsoa <- p2l %>% 
  select(lsoa11cd, ladnm, pcds)

CleanCrimeData <- TotalCrime %>%
  mutate(
    County = toupper(trimws(gsub("Police", "", `Falls within`)))
  ) %>%
  select(
    -Longitude, -Latitude, -Location,-`Falls within`, -`Last outcome category`, -Context, -`Reported by`, -`LSOA name`
  ) %>%
  drop_na(`Crime ID`, `LSOA code`)


CleanCrimeData = CleanCrimeData %>% 
  left_join(lsoa, by=c('LSOA code' = 'lsoa11cd')) %>% 
  distinct(`Crime ID`, .keep_all = TRUE)


CleanCrimeData = CleanCrimeData %>%
  rename(Postcode = "pcds") %>% 
  rename(District = "ladnm") %>%
  mutate(Postcode = str_replace_all(Postcode, " ", "")) %>%
  mutate(shortPostcode = str_trim(substring(Postcode, 1,4)))

write.csv(CleanCrimeData, "C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Cleaned Data/cleaned_crime_rate.csv", row.names = FALSE)
  





school = read_csv("C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/schooldata.csv")

cleanSchool = school %>% 
  select(URN, SCHNAME, TOWN, PCODE, ATT8SCR, NUMBOYS, NUMGIRLS, P8MEA) %>% 
  mutate(PCODE = str_replace_all(PCODE," ", "")) %>% 
  rename(Postcode="PCODE") %>% 
  mutate(shortPostcode = str_trim(substring(Postcode, 1,4))) %>%
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>% 
  filter(!is.na(ATT8SCR)) %>% 
  mutate(P8MEA = as.numeric(P8MEA)) %>% 
  filter(!is.na(P8MEA)) %>% 
  drop_na()

cleanSchool = cleanSchool %>% 
  inner_join(cleanHousePrices %>% select(District, County, shortPostcode), by = "shortPostcode" ) %>% 
  distinct(URN, .keep_all = TRUE)

write.csv(cleanSchool, "C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Cleaned Data/Cleaned_school_Data.csv", row.names = FALSE)





band = read_csv('C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Obtained Data/bandwidth.csv')

cleanBand = band %>%
  mutate(Postcode = str_replace_all(postcode, " ", "")) %>%
  mutate(shortPostcode = str_trim(substring(Postcode, 1,4))) %>%
  rename(Avg_download = `Average download speed (Mbit/s)`) %>% 
  select(Postcode, shortPostcode, Avg_download)

cleanBand = cleanBand %>% 
  inner_join(cleanHousePrices %>% select(Postcode, District, Town, County), by = "Postcode") %>% 
  distinct(Postcode, .keep_all = TRUE)


write.csv(cleanBand, "C:/Users/97798/OneDrive/Documents/Krish_Shrestha_230317/Cleaned Data/cleaned_BroadBand_Speeds.csv", row.names = FALSE)

