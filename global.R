library(shiny)
library(tidyverse)
library(readxl)
library(leaflet)
library(plotly)
library(RColorBrewer)

projection_data <- read.csv("Data files/Population Projections With Aggregations.csv")
shape_data <- read_rds("Data files/SCAP_shapefile.rds")
la_shape_data <- read_rds("Data files/LAShps.rds") 

# extract drop down list options
councils <- unique(projection_data$Council.Name[projection_data$Council.Name != "Scotland"])
years <- unique(projection_data$Year)
small_areas <- projection_data %>%
  filter(Level == "Small Area") %>%
  select(Area.Name)
unique_small_areas <- unique(small_areas$Area.Name) %>% sort()