library(shiny)
library(tidyverse)
library(readxl)
library(leaflet)
library(plotly)
library(RColorBrewer)
library(shinycssloaders)
library(sf)
library(vroom)
library(shinyvalidate)

projection_data <- vroom::vroom("Data files/Population Projections With Aggregations.csv", delim = ",", col_names = TRUE)
shape_data <- read_rds("Data files/SCAP_shapefile.rds")
la_shape_data <- read_rds("Data files/LAShps.rds") 

# extract drop down list options
councils <- unique(projection_data$Council.Name[projection_data$Council.Name != "Scotland"])
years <- unique(projection_data$Year)
small_area_lookup <- projection_data %>%
  filter(Level == "Small Area") %>%
  select(Council.Name, Area.Name) %>%
  distinct()

# load shortname lookups
lookup <- vroom::vroom("Data files/ShortNameLookup.csv", delim = ",", col_names = TRUE)
small_area_lookup <- small_area_lookup %>% 
  left_join(lookup, by = c("Area.Name" = "ShortName", "Council.Name" = "Council"))

# split out council and sub-council in shape_data for merging
shape_data <- shape_data %>% 
  separate(`Sub-Council Area Name`, into = c("SubCouncil", "Council"), sep = " - ", remove = TRUE)

# replace "and" with ampersand in shapefiles - this is to allow merging with projection_data
shape_data$Council <- gsub(" and ", " & ", shape_data$Council)