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

# Create dependency ratio ----------------------------------------------------------
working_age_totals <- projections_data_with_scot_and_persons %>% 
  filter(Level == "Small Area", Sex == "Persons" & Age %in% 16:64) %>%
  group_by(Council.Name, Level, Area.Name, Year, Sex) %>%
  summarise(WA.Population = sum(Population)) %>%
  ungroup()

dependent_age_totals <- projections_data_with_scot_and_persons %>% 
  filter(Level == "Small Area", Sex == "Persons" & Age %in% c(0:15, 65:90)) %>%
  group_by(Council.Name, Level, Area.Name, Year, Sex) %>%
  summarise(Dependent.Population = sum(Population)) %>%
  ungroup()

dependency_ratio_data <- left_join(working_age_totals, dependent_age_totals) %>%
  mutate(Dependency.Ratio = round((Dependent.Population / WA.Population) * 100,1)) %>%
  select(-Level, -Sex, -WA.Population, -Dependent.Population) %>%
  mutate(Measure = "Dependency Ratio") %>%
  rename(Data = "Dependency.Ratio") %>%
  select(Council.Name, Area.Name, Year, Measure, Data)

# Read in additional data ----------------------------------------------------------------

# Create a dummy file path 
# (Folder: Research - Population Projections, has been synced to personal drive)
# X replaces the council name
# Y replaces the folder name
dummy_path <- "C:/Users/connachan.cara/IS/Research - Population Projections/X/SCAP2001Y_out/reports_Continuity_SNPP.xls"

# Create list of council names
councils <- unique(projections_data_with_scot_and_persons$Council.Name)
councils <- councils[councils != "Scotland"]
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

# Create a function for running through each file
# list1 will be used for the council names, list 2 will be used for the folder names
read_files <- function(list1, list2, sheet_name){
  # This replaces the council name and folder name in the dummy path  with the name from the iteration in the lists
  complete_path <- gsub("X", list1, dummy_path)
  complete_path <- gsub("Y", list2, complete_path)
  read_excel(path = complete_path, sheet = sheet_name, range = "A199:AE240") %>%
    pivot_longer(cols = 2:31, names_to = "Year", values_to = "Data") %>%
    # This converts financial years to calendar years
    separate(Year, "Year", "-", fill = "left") %>%
    filter(Year %in% c(2018:2030)) %>%
    mutate(Council.Name = list1) %>%
    mutate(Measure = sheet_name) %>%
    rename(Area.Name = ...1) %>%
    na.omit()
}

# The read_files function needs to be run within map2_df 
# This will look through the list through the function for each file
# and combine the results into a single data frame
# The function will need to be run each time for the different data sets by
# changing the sheet name

all_persons <- map2_df(councils, folders, read_files, sheet = "All Persons")
net_migration <- map2_df(councils, folders, read_files, sheet = "Net Migration")
sex_ratio <- map2_df(councils, folders, read_files, sheet = "Sex Ratio")
mortality_ratio <- map2_df(councils, folders, read_files, sheet = "SMR")
fertility_rate <- map2_df(councils, folders, read_files, sheet = "TFR")

# Combined other measures data
other_measures <- rbind(all_persons, net_migration) 
other_measures <- rbind(other_measures, sex_ratio) 
other_measures <- rbind(other_measures, mortality_ratio) 
other_measures <- rbind(other_measures, fertility_rate)
other_measures <- other_measures %>%
  select(Council.Name, Area.Name, Year, Measure, Data)
other_measures <- rbind(other_measures, dependency_ratio_data)

# Change names of measures
other_measures$Measure[other_measures$Measure == "All Persons"] <- "Total Population"
other_measures$Measure[other_measures$Measure == "SMR"] <- "Standardised Mortality Ratio"
other_measures$Measure[other_measures$Measure == "TFR"] <- "Total Fertility Rate"

# Change council names back to match with lookup data
other_measures$Council.Name[other_measures$Council.Name == "Glasgow"] <- "Glasgow City"
other_measures$Council.Name[other_measures$Council.Name == "Perth and Kinross"] <- "Perth & Kinross"

# Write the data to a csv ---------------------------
write.csv(projections_data_with_scot_and_persons, "Data files/Population Projections With Aggregations.csv", row.names = FALSE)
write.csv(other_measures, "Data files/Other measures data.csv", row.names = FALSE)
