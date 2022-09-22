library(shiny)
library(tidyverse)
library(readxl)
library(leaflet)
library(plotly)

projection_data <- read.csv("Data files/Population Projections With Aggregations.csv")
shape_data <- read_rds("Data files/SCAP_shapefile.rds")
la_shape_data <- read_rds("Data files/LAShps.rds") 

# extract drop down list options
councils <- unique(projection_data$Council.Name[projection_data$Council.Name != "Scotland"])
years <- unique(projection_data$Year)
small_area_lookup <- projection_data %>%
  filter(Level == "Small Area") %>%
  select(Council.Name, Area.Name) %>%
  distinct()
