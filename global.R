library(shiny)
library(tidyverse)
library(readxl)

projection_data <- read.csv("Data files/All Councils - Detailed Projections - Male & Female.csv")

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
