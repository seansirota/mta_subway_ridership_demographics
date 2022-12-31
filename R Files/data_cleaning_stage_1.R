# Load tidyverse package.
library("tidyverse")

# Read original MTA ridership table to data frame.
ridership_2019 <- read_csv("C:/Users/Sean/Downloads/MTA Population and Demographics Case Study/2021 MTA Ridership.csv")
head(ridership_2019)
colnames(ridership_2019)
glimpse(ridership_2019)
view(ridership_2019)

# Read original station locations table to data frame.
station_index <- read_csv("C:/Users/Sean/Downloads/MTA Population and Demographics Case Study/DOITT_SUBWAY_STATION_01_13SEPT2010.csv")
head(station_index)
colnames(station_index)
glimpse(station_index)
view(station_index)

# Rename columns of both tables to make easier to clean.
ridership_2019 <- rename(ridership_2019, station_name = 'Station (alphabetical by borough)', unused = '*', borough = 'Boro', ridership_2019 = 'Ridership 2019')
colnames(ridership_2019)
station_index <- rename(station_index, id = OBJECTID, station_name = NAME, coordinates = the_geom, line = LINE)
colnames(station_index)

# Split the coordinates column into two seperate columns by latitude and longitude.
station_index <- cbind(station_index, str_split_fixed(substring(station_index$coordinates, 8, nchar(station_index$coordinates) - 1), " ", 2))
station_index <- rename(station_index, latitude = "1", longitude = "2")
station_index <- subset(station_index, select = -c(coordinates))
station_index$latitude <- as.numeric(station_index$latitude)
station_index$longitude <- as.numeric(station_index$longitude)

# Write partially cleaned tables to CSV files. Did some further cleaning in Excel to make list of stations on both tables the same.
write_csv(station_index, "C:/Users/Sean/Downloads/MTA Population and Demographics Case Study/partially_cleaned_geo_stations.csv")
write_csv(ridership_2019, "C:/Users/Sean/Downloads/MTA Population and Demographics Case Study/partially_cleaned_ridership.csv")

# Function created to attempt filtering both tables by having the same total stations. Ultimately went unused as I did this task in Excel instead.
station_combination <- function(name1, name2 = NULL) {
  if (is_null(name2)) {
    temp_df <- station_index[which(station_index$station_name == name1),]
    combined_lat <- mean(temp_df$latitude)
    combined_lon <- mean(temp_df$longitude)
    replace_vector <- c(0, name1, "", combined_lat, combined_lon)
    station_index <- station_index[-which(station_index$station_name == name1),]
    station_index[nrow(station_index) + 1,] <- replace_vector
    return(station_index)
  }
  else {
    temp_df <- union_all(station_index[which(station_index$station_name == name1),], station_index[which(station_index$station_name == name2),])
    combined_lat <- mean(temp_df$latitude)
    combined_lon <- mean(temp_df$longitude)
    replace_vector <- c(0, paste(name1, name2, sep = "-"), "", combined_lat, combined_lon)
    station_index <- station_index[-c(which(station_index$station_name == name1), which(station_index$station_name == name2)),]
    station_index[nrow(station_index) + 1,] <- replace_vector
    return(station_index)
  }
}

# Brought ridership table back. No changes made.
ridership_2019 <- read_csv("C:/Users/Sean/Downloads/MTA Population and Demographics Case Study/partially_cleaned_ridership.csv")
colnames(ridership_2019)
ridership_2019 <- rename(ridership_2019, station_name = "Station (alphabetical by borough)", ridership_in_2019 = "Ridership 2019")

#Removed all parentheses with line names within them.
ridership_2019$station_name <- str_replace_all(ridership_2019$station_name, "\\s*\\([^\\)]+\\)", "")
ridership_2019 <- ridership_2019[order(ridership_2019$station_name),]

# Brought station locations table after truncating table by merging stations together into station complexes.
station_index <- read_csv("C:/Users/Sean/Downloads/MTA Population and Demographics Case Study/partially_cleaned_geo_stations.csv")
colnames(station_index)
station_index <- station_index[order(station_index$station_name),]

#Function used to check if two stations in both table had the same exact name. Used to clean all station names before merging the tables together.
check_match <- function(dataset1, column1, dataset2, column2) {
  rep_count <- 1:max(nrow(dataset1), nrow(dataset2))
  check_vector <- c()
  dataset1_name <- c()
  dataset2_name <- c()
  check_table <- data.frame(dataset_1 = dataset1_name, dataset_2 = dataset2_name, check_equal = check_vector)
  for (x in rep_count) {
    check_vector <- append(check_vector, column1[x] == column2[x])
    dataset1_name <- append(dataset1_name, column1[x])
    dataset2_name <- append(dataset2_name, column2[x])
  }
  check_table <- rbind(check_table, data.frame(dataset1_name, dataset2_name, check_vector))
  return(check_table)
}

# Rename station statements for the station locations table.
station_index$station_name[station_index$station_name == "104th-102nd Sts"] <- "104th St"
station_index$station_name[station_index$station_name == "33rd St" & station_index$line == "7"] <- "33rd St-Rawson St"
station_index$station_name[station_index$station_name == "40th St"] <- "40th St-Lowery St"
station_index$station_name[station_index$station_name == "46th St" & station_index$line == "7"] <- "46th St-Bliss St"
station_index$station_name[station_index$station_name == "57th St" & station_index$line == "N-Q-R-W"] <- "57th St-7th Ave"
station_index$station_name[station_index$station_name == "75th St-Eldert Ln"] <- "75th St-Elderts Ln"
station_index$station_name[station_index$station_name == "81st St"] <- "81st St-Museum of Natural History"
station_index$station_name[station_index$station_name == "85th St-Forest Pky"] <- "85th St-Forest Pkwy"
station_index$station_name[station_index$station_name == "8th St-NYU"] <- "8th St-New York University"
station_index$station_name[station_index$station_name == "Bay Pky"] <- "Bay Pkwy"
station_index$station_name[station_index$station_name == "Beverly Rd" & station_index$line == "5-Feb"] <- "Beverley Rd"
station_index$station_name[station_index$station_name == "Broadway-Lafayette St/Bleeker St"] <- "Broadway-Lafayette St/Bleecker St"
station_index$station_name[station_index$station_name == "Brooklyn College-Flatbush Ave"] <- "Flatbush Ave-Brooklyn College"
station_index$station_name[station_index$station_name == "Bushwick-Aberdeen"] <- "Bushwick Ave-Aberdeen St"
station_index$station_name[station_index$station_name == "Canal St-Holland Tunnel"] <- "Canal St"
station_index$station_name[station_index$station_name == "Cortlandt St"] <- "WTC Cortlandt"
station_index$station_name[station_index$station_name == "Crown Hts-Utica Ave"] <- "Crown Heights-Utica Ave"
station_index$station_name[station_index$station_name == "Bronx Park East"] <- "Bronx Park E"
station_index$station_name[station_index$station_name == "East Broadway"] <- "E Broadway"
station_index$station_name[station_index$station_name == "Eastern Pkwy-Bklyn Museum"] <- "Eastern Pkwy-Brooklyn Museum"
station_index$station_name[station_index$station_name == "Ft Hamilton Pkwy"] <- "Fort Hamilton Pkwy"
station_index$station_name[station_index$station_name == "Harlem-148 St"] <- "Harlem-148th St"
station_index$station_name[station_index$station_name == "Jamaica Center-Parsons / Archer"] <- "Jamaica Center-Parsons/Archer"
station_index$station_name[station_index$station_name == "Lexington Ave-63rd St"] <- "Lexington Ave/63rd St"
station_index$station_name[station_index$station_name == "Morrison Av-Soundview"] <- "Morrison Ave-Soundview"
station_index$station_name[station_index$station_name == "Nereid Ave-238 St"] <- "Nereid Ave"
station_index$station_name[station_index$station_name == "Newkirk Ave" & station_index$line == "B-Q"] <- "Newkirk Plaza"
station_index$station_name[station_index$station_name == "Queensboro Plz"] <- "Queensboro Plaza"
station_index$station_name[station_index$station_name == "Rockaway Park-Beach 116 St"] <- "Rockaway Park-Beach 116th St"
station_index$station_name[station_index$station_name == "Roosevelt Island-Main St"] <- "Roosevelt Island"
station_index$station_name[station_index$station_name == "Sutter Ave-Rutland Road"] <- "Sutter Ave-Rutland Rd"
station_index$station_name[station_index$station_name == "West 4th St-Washington Sq"] <- "W 4th St-Washington Sq"
station_index$station_name[station_index$station_name == "Woodhaven Blvd-Queens Mall"] <- "Woodhaven Blvd"

# Replace all 'Av" with 'Ave' to achieve formatting harmony with the ridership table.
station_index$station_name <- str_replace_all(station_index$station_name, "Av*$", "Ave$")

# Added a few rows when I thought some stations were missing. Ultimately ended up being useless since the stations existed within the table but with a different name.
station_index[nrow(station_index) + 1,] <- list("2nd Ave", "", -73.99132844431459, 40.72424913183421)
station_index[nrow(station_index) + 1,] <- list("86th St", "", -73.97821718727536, 40.59317962807707)

# After the above stations were added, the below existing stations had to be removed as they were duplicates.
station_index <- station_index[-first(which(station_index$station_name == "Gravesend-86th St")),]
station_index <- station_index[-first(which(station_index$station_name == "Lower East Side-2nd Ave")),]

# Rename station statements for the ridership table.
ridership_2019$station_name[ridership_2019$station_name == "111st St"] <- "111th St"
ridership_2019$station_name[ridership_2019$station_name == "174-175th Sts"] <- "174th-175th Sts"
ridership_2019$station_name[ridership_2019$station_name == "182-183rd Sts"] <- "182nd-183rd Sts"
ridership_2019$station_name[ridership_2019$station_name == "47-50th Sts-Rockefeller Center"] <- "47th-50th Sts-Rockefeller Center"
ridership_2019$station_name[ridership_2019$station_name == "New Utrecht Ave/62nd St"] <- "62nd St/New Utrecht Ave"
ridership_2019$station_name[ridership_2019$station_name == "74-Broadway/Jackson Hts-Roosevelt Ave"] <- "Jackson Heights-Roosevelt Ave/74th St"
ridership_2019$station_name[ridership_2019$station_name == "86 St"] <- "86th St"
ridership_2019$station_name[ridership_2019$station_name == "Astor Place"] <- "Astor Pl"
ridership_2019$station_name[ridership_2019$station_name == "Beach 67th St-Arverne By The Sea"] <- "Beach 67th St"
ridership_2019$station_name[ridership_2019$station_name == "Chambers St/WTC/Park Place/Cortlandt"] <- "World Trade Center"
ridership_2019$station_name[ridership_2019$station_name == "Court Sq"] <- "Court Sq-23rd St"
ridership_2019$station_name[ridership_2019$station_name == "Delancey St-Essex St"] <- "Delancey St/Essex St"
ridership_2019$station_name[which(ridership_2019$ridership_in_2019 == max(ridership_2019$ridership_in_2019[which(ridership_2019$station_name == "Fulton St")]))] <- "Fulton Center"
ridership_2019$station_name[ridership_2019$station_name == "Jamaica Center-Parsons-Archer"] <- "Jamaica Center-Parsons/Archer"
ridership_2019$station_name[ridership_2019$station_name == "Kew Gardens-Union Turnpike"] <- "Kew Gardens-Union Tpke"
ridership_2019$station_name[ridership_2019$station_name == "Lexington Ave-51st St"] <- "Lexington Ave/51st St"
ridership_2019$station_name[ridership_2019$station_name == "Lexington Ave-63rd St"] <- "Lexington Ave/63rd St"
ridership_2019$station_name[ridership_2019$station_name == "Lorimer St/Metropolitan Ave"] <- "Metropolitan Ave/Lorimer St"
ridership_2019$station_name[ridership_2019$station_name == "Times Sq-42nd St/Bryant Pk/5th Ave"] <- "Times Sq-42nd St/Bryant Park/5th Ave"

# Statements used to format certain strings of station names in the ridership table.
ridership_2019$station_name <- str_replace_all(ridership_2019$station_name, "Avenue", "Ave")
ridership_2019$station_name <- str_replace_all(ridership_2019$station_name, "Avs", "Aves")
ridership_2019$station_name <- str_trim(str_replace_all(ridership_2019$station_name, "(East$|East\\s)", "E "), side = "both")
ridership_2019$station_name <- str_replace_all(ridership_2019$station_name, "Ave.", "Ave")
ridership_2019$station_name <- str_replace_all(ridership_2019$station_name, "St.", "St")

# One new station was added to replace two seperate ones.
ridership_2019[nrow(ridership_2019) + 1,] <- list("34th St-Penn Station", 51599040)

# This statement was used twice to remove the two stations replaced by the above station.
ridership_2019 <- ridership_2019[-first(which(ridership_2019$station_name == "34th St-Penn Station")),]

# Function called to check if all station names across both tables were equal.
checking_table <- check_match(ridership_2019, ridership_2019$station_name, station_index, station_index$station_name)

# ID column was added to create a merge between both tables into one combined table.
station_index_id <- 1:422
ridership_id <- 1:422
ridership_2019 <- cbind(station_index_id,ridership_2019)
station_index <- cbind(ridership_id,station_index)
mta_station_data <- cbind(ridership_2019,station_index)
mta_station_data <- subset(mta_station_data, select = -c(4, 5))

# Combined table was exported to CSV to be checked to make sure location and ridership data matched the same stations.
write_csv(mta_station_data, "C:/Users/Sean/Downloads/MTA Population and Demographics Case Study/combined_table.csv")

# After checking, combined table was re-added to a dataframe and is completed.
mta_station_data <- read_csv("C:/Users/Sean/Downloads/MTA Population and Demographics Case Study/combined_table.csv")