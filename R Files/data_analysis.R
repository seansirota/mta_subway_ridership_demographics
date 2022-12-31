# Load necessary packages for data cleaning and mapping.
library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)

# Remove scientific notation from plots.
options(scipen = 5)

# These are the topics I consider when creating plots:
# 1. Introduction
#   a. Map of stations with no census tract fill. Size of dots shows ridership. Explain ridership centers.
#   b. Bar graph of top ten highest ridership stations.
#   c. Map of census tracts with total stations serving each census tract.
#   d. Population density map. Explain population centers.
#   e. Bar graph comparing total population of races by borough.
#   f. Map of White, Black, Hispanic, and Asian populations. Explain where most of each demographic resides.

# 2. Analysis
#   a. Map of stations on population density. Explain how well subway serves population centers.
#   b. Scatter plot comparing ridership to population density of each station by race. Bar graph comparing race ridership.
#   c. Map of stations on White, Black, Hispanic, and Asian populations. Explain how well subway serves each demographic.
#   d. Scatter plots comparing ridership of stations to highest percentage race served by subway.
#   e. Map showing stations with race by highest percentage by census tract. Size indicates highest race percentage.
#   f. Bar graph showing races not served by the subway.
# 3. Conclusion
#   a. Explain takeaways about the subway and its relationship with population density and the demographics which ride it.
#   b. Give suggestions on how to improve subway by saying where it can be expanded to next.
#   c. List topics for further investigation.

# 1a.
mta_summary_table %>% 
  ggplot() +
  geom_sf(data = summary_stats_table, fill = "black", color = NA) +
  geom_sf(mapping = aes(size = ridership_in_2019), color = "green") +
  theme_void() +
  labs(title = "MTA Subway Ridership by Station in 2019", size = "Total Ridership")

# 1b.
mta_summary_table %>% 
  arrange(desc(ridership_in_2019)) %>% 
  slice(1:10) %>% 
  ggplot() +
  geom_col(mapping = aes(x = ridership_in_2019, y = reorder(station_name, ridership_in_2019))) +
  labs(title = "Top Ten MTA Subway Stations in Ridership in 2019", x = "Total Ridership", y = "Stations")

# 1c.
summary_stats_table %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = count), color = NA) +
  theme_void() +
  labs(title = "Total MTA Subway Stations by Census Tract", fill = "Stations within tract")

# 1d.
summary_stats_table %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = population_density), color = NA) +
  theme_void() +
  scale_fill_viridis_c(
    option = "E",
    na.value = "grey"
  ) +
  labs(title = "Population Density of NYC by Census Tract in 2020", fill = "Population Density", caption = "Population density is in persons per square mile.")

# 1e.
demographics_table %>% 
  ggplot() +
  geom_col(mapping = aes(x = reorder(race, -total_population), y = total_population)) +
  labs(title = "Races ranked by Total Population in 2020", x = "Race", y = "Total Population")

# 1f. (White)
demographics_table %>% 
  filter(race == "White") %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = total_population), color = NA) +
  theme_void() +
  scale_fill_viridis_c(
    option = "G",
    na.value = "grey"
  ) +
  labs(title = "White Population by Census Tract in 2020", fill = "Total White")

# 1f. (Hispanic)
demographics_table %>% 
  filter(race == "Hispanic or Latino") %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = total_population), color = NA) +
  theme_void() +
  scale_fill_viridis_c(
    option = "G",
    na.value = "grey"
  ) +
  labs(title = "Hispanic and Latino Population by Census Tract in 2020", fill = "Total Hispanic/Latino")

# 1f. (Black)
demographics_table %>% 
  filter(race == "Black") %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = total_population), color = NA) +
  theme_void() +
  scale_fill_viridis_c(
    option = "G",
    na.value = "grey"
  ) +
  labs(title = "Black Population by Census Tract in 2020", fill = "Total Black")

# 1f. (Asian)
demographics_table %>% 
  filter(race == "Asian") %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = total_population), color = NA) +
  theme_void() +
  scale_fill_viridis_c(
    option = "G",
    na.value = "grey"
  ) +
  labs(title = "Asian Population by Census Tract in 2020", fill = "Total Black")

# 2a.
mta_summary_table %>% 
  ggplot() +
  geom_sf(data = summary_stats_table, mapping = aes(fill = population_density), color = NA) +
  geom_sf(color = "green") +
  theme_void() +
  scale_fill_viridis_c(
    option = "E",
    na.value = "grey"
  ) +
  labs(title = "MTA Subway Coverage over Density", fill = "Population Density", caption = "Population density is in persons per square mile.")

# 2b. (Scatter Plot)
mta_summary_table %>% 
  ggplot(mapping = aes(x = population_density, y = ridership_in_2019)) +
  geom_smooth(method = lm, color = "black", se = FALSE) +
  geom_point(mapping = aes(color = race)) +
  ylim(0, 20000000) +
  labs(title = "MTA Subway Ridership Compared to Population Density by Race", x = "Population Density", y = "Total Ridership", color = "Race", caption = "Population density is in persons per square mile.\nMTA subway station ridership is compared to the\ncensus tract population density in which it resides in.\nStations with ridership over 20,000,000 removed from plot to improve clarity.")

# 2b. (Bar Graph)
mta_summary_table %>% 
  group_by(race) %>% 
  summarise(ridership_in_2019 = sum(ridership_in_2019)) %>% 
  arrange(desc(ridership_in_2019)) %>% 
  ggplot() +
  geom_col(mapping = aes(x = reorder(race, -ridership_in_2019), y = ridership_in_2019)) +
  labs(title = "MTA Subway Ridership by Race in 2019", x = "Race", y = "Total Ridership")

# 2c. (White)
demographics_table %>% 
  filter(race == "White") %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = total_population), color = NA) +
  geom_sf(data = mta_summary_table, color = "green") +
  theme_void() +
  scale_fill_viridis_c(
    option = "G",
    na.value = "grey"
  ) +
  labs(title = "MTA Subway Stations Over White Population", fill = "Total White")

# 2c. (Hispanic)
demographics_table %>% 
  filter(race == "Hispanic or Latino") %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = total_population), color = NA) +
  geom_sf(data = mta_summary_table, color = "green") +
  theme_void() +
  scale_fill_viridis_c(
    option = "G",
    na.value = "grey"
  ) +
  labs(title = "MTA Subway Stations Over Hispanic and Latino Population", fill = "Total Hispanic/Latino")

# 2c. (Black)
demographics_table %>% 
  filter(race == "Black") %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = total_population), color = NA) +
  geom_sf(data = mta_summary_table, color = "green") +
  theme_void() +
  scale_fill_viridis_c(
    option = "G",
    na.value = "grey"
  ) +
  labs(title = "MTA Subway Stations Over Black Population", fill = "Total Black")

# 2c. (Asian)
demographics_table %>% 
  filter(race == "Asian") %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = total_population), color = NA) +
  geom_sf(data = mta_summary_table, color = "green") +
  theme_void() +
  scale_fill_viridis_c(
    option = "G",
    na.value = "grey"
  ) +
  labs(title = "MTA Subway Stations Over Asian Population", fill = "Total Asian")

# 2d.
mta_summary_table %>% 
  ggplot(aes(x = percent, y = ridership_in_2019)) +
  geom_smooth(method = lm, color = "black", se = FALSE) +
  geom_point(mapping = aes( color = race)) +
  ylim(0, 20000000) +
  labs(title = "MTA Subway Ridership Compared to Highest\nRace Percentage by Race", x = "Highest Race Percentage", y = "Total Ridership", color = "Race", caption = "Population density is in persons per square mile.\nMTA subway station ridership is compared to the census tract\nhighest ridership percentage race in which it resides in.\nStations with ridership over 20,000,000 removed from plot to improve clarity.")

# 2e.
mta_summary_table %>% 
  ggplot() +
  geom_sf(data = summary_stats_table, mapping = aes(fill = population_density), color = NA) +
  geom_sf(mapping = aes(color = race)) +
  theme_void() +
  scale_fill_viridis_c(
    option = "E",
    na.value = "grey"
  ) +
  labs(title = "MTA Subway Coverage over Density by Highest Race Percentage", fill = "Population Density", color = "Race", caption = "Population density is in persons per square mile.")

# 2f.
demographics_table %>% 
  st_join(mta_summary_table, left = TRUE) %>% 
  filter(is.na(station_name)) %>% 
  ggplot() +
  geom_col(mapping = aes(x = reorder(race.x, -total_population), y = total_population)) +
  labs(title = "NYC Population Not Served by MTA Subway in 2019", x = "Race", y = "Total Population")