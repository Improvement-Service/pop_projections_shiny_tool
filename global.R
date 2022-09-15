library(shiny)
library(tidyverse)
library(readxl)

projection_data <- read.csv("Data files/All Councils - Detailed Projections - Male & Female.csv")
shape_data <- read_rds("Data files/SCAP_shapefile.rds")

# Add Scotland level data to projection_data
la_data <- projection_data %>% filter(Level == "Council")

scot_data <- la_data %>% 
  group_by(Year, Sex, Age) %>% 
  summarise(Population = sum(Population)) %>%
  mutate(Council.Name = "Scotland") %>%
  mutate(Level = "Scotland") %>%
  mutate(Area.Name = "Scotland") %>%
  select(Council.Name, Level, Area.Name, Year, Sex, Age, Population)

projection_data <- rbind(projection_data, scot_data)
rm(la_data, scot_data)

# Add Persons data to projection_data
persons_data <- projection_data %>%
  group_by(Council.Name, Level, Area.Name, Year, Age) %>%
  summarise(Population = sum(Population)) %>%
  mutate(Sex = "Persons") %>%
  select(Council.Name, Level, Area.Name, Year, Sex, Age, Population)

projection_data <- rbind(projection_data, persons_data)
rm(persons_data)

# Add dependency ratio to projection_data (this relates to all genders)
age_data <- projection_data %>% filter(Age != "All ages") 
age_data$Age[age_data$Age == "90+"] <- "90"
age_data$Age <- as.numeric(age_data$Age)

working_age_data <- age_data %>% 
  filter(Age %in% 16:64) %>%
  group_by(Council.Name, Level, Area.Name, Year, Sex) %>%
  summarise(WA.Population = sum(Population)) 

dependent_age_data <- age_data %>% 
  filter(Age %in% c(0:15, 65:90)) %>%
  group_by(Council.Name, Level, Area.Name, Year, Sex) %>%
  summarise(Dependent.Population = sum(Population))

dependency_ratio_data <- merge(working_age_data, dependent_age_data) %>%
  mutate(Dependency.Ratio = round((Dependent.Population / WA.Population) * 100,1)) %>%
  select(-WA.Population, -Dependent.Population)

projection_data <- merge(projection_data, dependency_ratio_data)
rm(age_data, working_age_data, dependent_age_data, dependency_ratio_data)

# Convert age column to numeric
projection_data <- projection_data %>% filter(Age != "All ages")
projection_data$Age[projection_data$Age == "90+"] <- "90"
projection_data$Age <- as.numeric(projection_data$Age)

# extract drop down list options
councils <- unique(projection_data$Council.Name[projection_data$Council.Name != "Scotland"])
years <- unique(projection_data$Year)
small_areas <- projection_data %>%
  filter(Level == "Small Area") %>%
  select(Area.Name)
unique_small_areas <- unique(small_areas$Area.Name) %>% sort()