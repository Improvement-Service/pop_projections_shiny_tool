library(shiny)
library(tidyverse)
library(readxl)
library(leaflet)
library(plotly)
library(RColorBrewer)
library(shinycssloaders)
library(sf)

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

#load shortname lookups
lookup <- read_csv("Data files/ShortNameLookup.csv")
lookup$ShortName <- str_trim(lookup$ShortName)
small_area_lookup <- small_area_lookup %>% left_join(lookup, by = c("Area.Name" = "ShortName", "Council.Name" = "Council"))

#split out council and sub-council in shape_data for merging
shape_data <- shape_data %>% separate(`Sub-Council Area Name`, into = c("SubCouncil", "Council"), sep = " - ", remove = FALSE)

#merge projection data with long names for sub-councils
projection_data <- projection_data %>% left_join(lookup, by = c("Area.Name" = "ShortName", "Council.Name" = "Council"))
projection_data[is.na(projection_data$LongName), "LongName"] <- projection_data[is.na(projection_data$LongName), "Area.Name"]
