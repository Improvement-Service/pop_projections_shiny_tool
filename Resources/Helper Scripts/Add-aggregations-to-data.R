library(magrittr)
library(dplyr)
library(readxl)
library(tidyverse)

# Read in projection data -------------------------------------------------------
projection_data <- read.csv("Data files/All Councils - Detailed Projections - Male & Female.csv")

# transform age variable to numeric
projection_data <- projection_data %>% filter(Age != "All ages")
projection_data$Age[projection_data$Age == "90+"] <- "90"
projection_data$Age <- as.numeric(projection_data$Age)

# Create aggregated values ------------------------------------------------------
# aggregate data to Scotland level data and append to projection_data
projections_data_with_scot <- projection_data %>% 
  filter(Level == "Council") %>%
  group_by(Year, Sex, Age) %>% 
  summarise(Population = sum(Population)) %>%
  mutate(Council.Name = "Scotland") %>%
  mutate(Level = "Scotland") %>%
  mutate(Area.Name = "Scotland") %>%
  select(Council.Name, Level, Area.Name, Year, Sex, Age, Population) %>%
  rbind(., projection_data)

# aggregate sex data to all persons and append to projection_data_with_scot
projections_data_with_scot_and_persons <- projections_data_with_scot %>%
  group_by(Council.Name, Level, Area.Name, Year, Age) %>%
  summarise(Population = sum(Population)) %>%
  mutate(Sex = "Persons") %>%
  select(Council.Name, Level, Area.Name, Year, Sex, Age, Population) %>%
  rbind(., projections_data_with_scot)

# Read in additional data ----------------------------------------------------------------

# Create a dummy file path to comp_Continuity_SNPP file which contains required data 
# (Folder: Research - Population Projections, has been synced to personal drive)
# X replaces the council name
# Y replaces the folder name
dummy_path <- "C:/Users/connachan.cara/IS/Research - Population Projections/X/SCAP2001Y_out/comp_Continuity_SNPP.xls"

# Create list of council names
councils <- unique(projection_data$Council.Name)
# need to change these names to match with the folder names
# will want to change these back once the full dataset is created
councils[councils == "Glasgow City"] <- "Glasgow"
councils[councils == "Perth & Kinross"] <- "Perth and Kinross"

# Create list of folder names
# (these are the council name with the area type, these are not consistent so
# need to be listed out)
folders <- c("Aberdeen CityMMW",
             "AberdeenshireMMW",
             "AngusMMW",
             "ArgyllandButeHMA",
             "City of EdinburghMMW",
             "ClackmannanshireMMW",
             "Dumfries and GallowayMMW",
             "Dundee CityMMW",
             "East AyrshireMMW",
             "East DunbartonshireMMW",
             "East LothianMMW",
             "East RenfrewshireHMA",
             "FalkirkMMW",
             "FifeMMW",
             "GlasgowMMW",
             "HighlandHMA",
             "InverclydeMMW",
             "MidlothianMMW",
             "MorayMMW",
             "Na h-Eileanan SiarMMW",
             "North Ayrshire",
             "North LanarkshireMMW",
             "Orkney IslandsMMW",
             "Perth and KinrossMMW",
             "RenfrewshireMMW",
             "Scottish BordersMMW",
             "Shetland IslandsMMW",
             "South Ayrshire",
             "South LanarkshireCommAreas",
             "StirlingMMW",
             "West DunbartonshireMMW",
             "West LothianMMW"
)

# Function to read in multiple sheets from 1 file
read_sheets <- function(sheets, path, council){
  data <- read_xls(path = path, 
                   sheet = sheets,
                   range = "A5:AE90",
                   na = "*"
                   ) %>%
    mutate(Council.Name = council) %>%
    mutate(Area.Name = sheets) %>%
    filter(`...1` %in% c("Expectation of life: persons", 
                         "Natural change", 
                         "Net migration", 
                         "Total",
                         "0-15 and 65+ / 16-64",
                         "Sex ratio males /100 females"
                         )
           ) %>%
    pivot_longer(cols = 3:31, names_to = "Year", values_to = "Value") %>%
    rename(Measure = ...1) %>%
    select(3,4,5,1,6)
}

# Function for reading each file
# list1 will be used for the council names, list 2 will be used for the folder names
read_files <- function(council_list, folder_list){
  # This replaces the council name and folder name in the dummy path  with 
  # the name from the iteration in the lists
  complete_path <- gsub("X", council_list, dummy_path)
  complete_path <- gsub("Y", folder_list, complete_path)
  sheet_names <- excel_sheets(path = complete_path)
  sheet_names <- sheet_names[! sheet_names == "Notes"]
  
  # Runs the read sheets function and combines the sheets into 1 data frame
  data <- map_df(sheet_names, 
                 read_sheets,
                 path = complete_path,
                 council = council_list
                 )
  }

# The read_files function needs to be run within map2_df to allow it
# to loop through multiple lists - list of councils and list of folder
# This will then combine the results into a single data frame
measures_data <- map2_df(councils, folders, read_files)

# Converts financial years to calendar years
measures_data$Year <- gsub("[0-9]{2}-", "", measures_data$Year)

# Changes names of indicators
measures_data$Measure[measures_data$Measure == "Expectation of life: persons"] <- "Life Expectancy - Persons"
measures_data$Measure[measures_data$Measure == "Natural change"] <- "Natural Change"
measures_data$Measure[measures_data$Measure == "Net migration"] <- "Net Migration"
measures_data$Measure[measures_data$Measure == "Total"] <- "Total Population"
measures_data$Measure[measures_data$Measure == "0-15 and 65+ / 16-64"] <- "Dependency Ratio"
measures_data$Measure[measures_data$Measure == "Sex ratio males /100 females"] <- "Sex Ratio"

# Converts dependency ratio value to % value
measures_data$Value[measures_data$Measure == "Dependency Ratio"] <- measures_data$Value[measures_data$Measure == "Dependency Ratio"] * 100

measures_data$Value <- round(measures_data$Value, 1) 

# Convert councils names back to match with lookup data
measures_data$Council.Name[measures_data$Council.Name == "Glasgow"] <- "Glasgow City"
measures_data$Council.Name[measures_data$Council.Name == "Perth and Kinross"] <- "Perth & Kinross"

# Write the data to a csv ---------------------------
write.csv(projections_data_with_scot_and_persons, "Data files/Population Projections With Aggregations.csv", row.names = FALSE)
write.csv(measures_data, "Data files/Other measures data.csv", row.names = FALSE)
