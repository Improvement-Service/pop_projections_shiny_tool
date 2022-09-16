library(magrittr)
library(dplyr)

projection_data <- read.csv("Data files/All Councils - Detailed Projections - Male & Female.csv")

#transform age variable to numeric
projection_data <- projection_data %>% filter(Age != "All ages")
projection_data$Age[projection_data$Age == "90+"] <- "90"
projection_data$Age <- as.numeric(projection_data$Age)

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

working_age_totals <- projections_data_with_scot_and_persons %>% 
  filter(Age %in% 16:64) %>%
  group_by(Council.Name, Level, Area.Name, Year, Sex) %>%
  summarise(WA.Population = sum(Population)) 

dependent_age_totals <- projections_data_with_scot_and_persons %>% 
  filter(Age %in% c(0:15, 65:90)) %>%
  group_by(Council.Name, Level, Area.Name, Year, Sex) %>%
  summarise(Dependent.Population = sum(Population))

dependency_ratio_data <- left_join(working_age_totals, dependent_age_totals) %>%
  mutate(Dependency.Ratio = round((Dependent.Population / WA.Population) * 100,1)) %>%
  select(-WA.Population, -Dependent.Population)

projection_data_complete <- left_join(projections_data_with_scot_and_persons, dependency_ratio_data) %>%
  arrange(Council.Name, Year, Age)

write.csv(projection_data_complete, "Data files/Population Projections With Aggregations.csv", row.names = FALSE)
