rm(list = ls()) # Clear global environment

# Load Packages ----
library(tidyverse)
library(janitor)
library(here)

# Load Data ----
wb_indicators <- read_csv(here("data", "wb_indicators.csv"), na = c("..", ""))
wb_metadata <- read_csv(here("data", "wb_indicators_metadata.csv"))

# Pivot Longer ----
wb_indicators_long <- wb_indicators %>%
  pivot_longer(cols = '2001 [YR2001]':'2020 [YR2020]', # Which columns to squish
               names_to = "year", # The original column names are squished into this column
               values_to = "indicator_value") # The values are correctly aligned in this column

# Check it out (always):
# View(wb_indicators_long) # Why do I comment this out or run it in the Console?

wb_data_clean <- wb_indicators_long %>%
  tidyr::separate(col = year, into = c("year", "year_chr"), sep = " ") %>%
  dplyr::select(-year_chr, -'Country Code', -'Series Code') # This drops a few redundant columns (caution here...best to leave things if you're not sure)

head(wb_data_clean)

wb_data_tidy <- wb_data_clean %>%
  tidyr::drop_na('Series Name') %>%
  tidyr::pivot_wider(names_from = 'Series Name', values_from = indicator_value) # Pivot to wide format

head(wb_data_tidy)

names(wb_data_tidy) <- c("country", "year", "access_clean_fuels_pp", "access_electricity_pp", "co2_emissions_kt", "fossil_fuel_cons_pt", "water_stress")

head(wb_data_tidy)

us_wb <- wb_data_tidy %>%
  filter(country == "United States")

head(us_wb)

nicaragua_co2 <- wb_data_tidy %>%
  filter(country == "Nicaragua") %>%
  select(year, co2_emissions_kt)

wb_subset <- wb_data_tidy %>%
  select(-c(water_stress, access_electricity_pp))

# Keep columns country, year, and co2_emissions_kt
wb_data_tidy[['country','year','co2_emissions_kt']]

# Exclude column access_fuels_pp
wb_data_tidy.drop('access_fuels_pp', axis = 1) # axis = 1 here indicates drop COLUMN (0 = rows)

wb_newnames <- wb_data_tidy %>%
  rename(elec = access_electricity_pp, co2 = co2_emissions_kt)

# Check the class of year:
class(wb_data_tidy$year) # Character! Let's change it.

wb_data_tidy <- wb_data_tidy %>%
  mutate(year = as.numeric(year))

# Check again:
class(wb_data_tidy$year)

wb_co2_tons <- wb_data_tidy %>%
  mutate(co2_tons = co2_emissions_kt * 1000)

head(wb_co2_tons)

co2_total <- wb_data_tidy %>%
  group_by(country) %>%
  summarize(total_co2_kt = sum(co2_emissions_kt, na.rm = TRUE))

co2_annual <- wb_data_tidy %>%
  group_by(year) %>%
  summarize(annual_total_co2_kt = sum(co2_emissions_kt, na.rm = TRUE))

print("Here")
# Let's plot this for fun:
ggplot(data = co2_annual, aes(x = year, y = annual_total_co2_kt)) + geom_line()

