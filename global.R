library(shiny)
library(tidyverse)
library(readxl)
library(leaflet)
library(plotly)
library(RColorBrewer)

projection_data <- read_csv("Data files/Population Projections With Aggregations.csv")
shape_data <- read_rds("Data files/SCAP_shapefile.rds")
la_shape_data <- read_rds("Data files/LAShps.rds") 

# extract drop down list options
councils <- unique(projection_data$Council.Name[projection_data$Council.Name != "Scotland"])
years <- unique(projection_data$Year)
small_area_lookup <- projection_data %>%
  filter(Level == "Small Area") %>%
  select(Council.Name, Area.Name) %>%
  distinct()

#replace "and" with ampersand in shapefiles - this is to allow merging with projection_data
shape_data$`Sub-Council Area Name` <- gsub(" and ", " & ", shape_data$`Sub-Council Area Name`)

#load shortname lookups
lookup <- read_csv("Data files/ShortNameLookup.csv")

#split out council and sub-council in shape_data for merging
shape_data[shape_data$`Sub-Council Area Name` == "Annbank Mossblown & Tarbolton - the Coalfields - South Ayrshire", "Sub-Council Area Name"] <- "Annbank Mossblown & Tarbolton: the Coalfields - South Ayrshire"
shape_data <- shape_data %>% separate(`Sub-Council Area Name`, into = c("SubCouncil", "Council"), sep = " - ", remove = FALSE)
