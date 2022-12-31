# Load necessary packages for data cleaning and mapping.
library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)

# Preliminary statement used to turn off spherical plotting to prevent an error when using st_intersects().
sf_use_s2(FALSE)

# Population table is read from local files and contains population density data from the 2020 NYC Census. Census table is generated using an API that lets you put in parameters to retrieve data for.
population_table <- read_csv("C:/Users/Sean/Downloads/MTA Population and Demographics Case Study/population_table.csv")
census_table <- get_decennial(
  geography = "tract",
  state = "New York",
  county = c(
    "Bronx",
    "Kings",
    "New York",
    "Queens"),
  year = 2020,
  variables = c(
    white = "P2_005N",
    black = "P2_006N",
    hispanic_latino = "P2_002N",
    asian = "P2_008N",
    native_american = "P2_007N",
    other = "P2_010N",
    two_or_more_races = "P2_011N"),
  key = "c8bf34da6f7ac716f344ddc7d83a7e1b26a49df9",
  summary_var = "P2_001N",
  geometry = TRUE,
  cb = FALSE
)

# Renaming of columns in census_table and converting census_tract column to numeric to allow joins.
census_table <- rename(census_table, census_tract = GEOID, full_name = NAME, race = variable, total_population = value, total_by_race = summary_value, polygons = geometry)
census_table$census_tract <- as.numeric(as.character(census_table$census_tract))

# Population and census tables are joined to create the combined demographics table containing demographic and population density data. Also, census tract polygons are made more clear by removing water from shapes.
demographics_table <- inner_join(census_table, population_table)
demographics_table <- erase_water(demographics_table, area_threshold = 0.75)
demographics_table$race <- as.factor(demographics_table$race)
levels(demographics_table$race) <- list("White" = "white", "Black" = "black", "Hispanic or Latino" = "hispanic_latino", "Asian" = "asian", "Native American" = "native_american", "Other" = "other", "Two or more races" = "two_or_more_races")

# Station data which was previously cleaned is used again here. Table is converted to Simple Features datatype and given the same CRS value as the demographics_table.
mta_station_data <- st_as_sf(mta_station_data, coords = c("longitude", "latitude"))
mta_station_data <- st_set_crs(mta_station_data, st_crs(demographics_table))

# Plot of demo_graphics table used to test its functionality. This plot shows the distribution of whites in New York.
demographics_table %>% 
  filter(race == "white") %>% 
  ggplot(mapping = aes(fill = total_population)) + 
  geom_sf(color = NA, size = 3) + 
  theme_void() +
  scale_fill_viridis_c(
    option = "G",
    na.value = "grey"
  )

# Stations table is tested here. All stations are in green. Larger dots have higher ridership.
mta_station_data %>% 
  ggplot() +
  theme_void() +
  geom_sf(mapping = aes(size = ridership_in_2019), color = "green")

# Combined plot of stations and demographics table. Stations are placed on top of population density.
ggplot() +
  geom_sf(data = demographics_table, mapping = aes(fill = population_density), color = NA) +
  geom_sf(data = mta_station_data, mapping = aes(size = ridership_in_2019), color = "green") +
  theme_void() +
  scale_fill_viridis_c(
    option = "G",
    na.value = "grey"
  )

# A new summary table was created that would be used to show summary statistics. One new column in this table shows how many stations fall within a census tract. A second shows the percentage race each census tract is made up of.
summary_stats_table <- mutate(demographics_table, count = lengths(st_intersects(demographics_table, mta_station_data)), percent = demographics_table$total_population / demographics_table$total_by_race)
summary_stats_table <- subset(summary_stats_table, select = -c(full_name, total_population, total_by_race))

# Last table created by merging MTA station data with highest percentage of race within census tract served by station.
mta_summary_table <- summary_stats_table %>% 
  group_by(census_tract) %>% 
  filter(percent == max(percent)) %>%
  arrange(by_group = "census_tract") %>% 
  st_intersection(mta_station_data)

# Summary table is plotted here. This plot shows which census tracts have stations within them. Tracts with a brighter color have more stations within them.
ggplot(summary_stats_table, mapping = aes(fill = count)) + 
  geom_sf(color = NA, size = 3) + 
  theme_void() +
  scale_fill_viridis_c(
    option = "H",
    na.value = "grey"
  )

# Combined plot of summary, demographics, and stations tables. Population density is at the bottom, followed by census tracts with stations within them, followed by stations themselves shown by size in ridership.
ggplot() +
  geom_sf(data = demographics_table, mapping = aes(fill = population_density), color = NA) +
  geom_sf(data = summary_stats_table[which(summary_stats_table$count > 0),], mapping = aes(fill = count), color = "red") +
  geom_sf(data = mta_station_data, mapping = aes(size = ridership_in_2019), color = "green") +
  theme_void() +
  scale_fill_viridis_c(
    option = "G",
    na.value = "grey"
  )

# Plot combining summary stats table with itself. Bottom layer is an empty map with no fill. Top layer shows percentage of census tracts with at least one subway station that is Asian.
ggplot() + 
  geom_sf(data = summary_stats_table) +
  geom_sf(data = summary_stats_table[which(summary_stats_table$count > 0),], mapping = aes(fill = asian), color = NA) +
  theme_void()

# Writing all completed tables to CSV for easy import later.
write_csv(demographics_table, "C:/Users/Sean/Downloads/MTA Population and Demographics Case Study/demographics_table.csv")
write_csv(summary_stats_table, "C:/Users/Sean/Downloads/MTA Population and Demographics Case Study/summary_stats_table.csv")
write_csv(mta_summary_table, "C:/Users/Sean/Downloads/MTA Population and Demographics Case Study/mta_summary_table.csv")

#Importing completed tables.
demographics_table <- st_as_sf(read_csv("C:/Users/Sean/Downloads/MTA Population and Demographics Case Study/demographics_table.csv"))
summary_stats_table <- st_as_sf(read_csv("C:/Users/Sean/Downloads/MTA Population and Demographics Case Study/summary_stats_table.csv"))
mta_summary_table <- st_as_sf(read_csv("C:/Users/Sean/Downloads/MTA Population and Demographics Case Study/mta_summary_table.csv"))