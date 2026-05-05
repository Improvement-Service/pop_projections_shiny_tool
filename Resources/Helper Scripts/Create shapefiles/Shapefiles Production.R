# Create shapefiles for sub-council geographies

library(tidyverse)
library(readxl)
library(leaflet)
library(tools)
library(sf)
library(units)
library(rmapshaper)

# Read in lookup file - DZ to sub-council areas
lookup <- read_excel("Resources/Helper Scripts/Create Shapefiles/Geography Lookup Table - dashboard format.xlsx") %>%
  mutate(`Sub-Council Area Name` = str_trim(`Sub-Council Area Name`))

# sub-council area names
scap_areas <- lookup %>%
  distinct(`Sub-Council Area Name`, `Local Authority Name`)

# Create shape files for custome geographies -----------------------------------

# *****All new DZ are 1:1 match so don't need to select best fit however in
# future if we run again with custom areas that aren't a 1:1 match then historic
# code would need to be used - uses area with highest percentage match then removes 
# duplicates

# Store custom area lookup
custom_lookup <- lookup %>% 
  filter(`Type of Geography` == "Custom")

# Save names of local authorities with custom geographies
custom_areas <- custom_lookup %>% 
  distinct(`Local Authority Name`) %>%
  pull(`Local Authority Name`)

# Read data zone shapes
# Available via https://www.data.gov.uk/dataset/ab9f1f20-3b7f-4efa-9bd2-239acf63b540/data-zone-boundaries-2011
shapes <- st_read("Resources/Helper Scripts/Create Shapefiles/SG_DataZoneBdry_2011/SG_DataZone_Bdry_2011.shp")

# Combine with custom geography lookup 
custom_shapes <- left_join(shapes, 
                           custom_lookup, 
                           by = c("DataZone" = "Datazone 2011 Code")) %>%
  select(-"Type of Geography") %>%
  drop_na() %>%
  # Merge geographies by sub-council area
  group_by(`Sub-Council Area Name`, `Local Authority Name`) %>%
  summarise()

# Create shape files for MMW areas ---------------------------------------------

# Using December 2025 MMW shapes sourced directly from ONS https://geoportal.statistics.gov.uk/datasets/ons::wards-december-2025-boundaries-uk-bfc-1/about
mmw_shapes <- st_read("Resources/Helper Scripts/Create Shapefiles/WD_DEC_2025_UK_BFC_6695660590440927520/WD_DEC_2025_UK_BFC.shp") %>%
  # Keep only Scotland shapes
  filter(str_detect(WD25CD, "S"))

# Lookup for 2011 MMW to LA from https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fdata-zone-lookup
# This has the correct MMW names which have been used in the projections so 
# once combined with the MMW shapes the names can be replaced with these
mmw_codes <- read.csv("Resources/Helper Scripts/Create Shapefiles/DataZone2011lookup_2024-12-16.csv",
                      # Need this setting to read apostrophe's correctly
                      fileEncoding = "WINDOWS-1252") %>%
  # Unique MMW to LA lookup
  distinct(MMWard_Code, MMWard_Name, LA_Code, LA_Name) %>%
  mutate(MMWard_Name = str_trim(MMWard_Name))

mmw_shapes <- mmw_shapes %>%
  left_join(mmw_codes, by = c("WD25CD" = "MMWard_Code")) %>%
  # Filter out rows for councils using custom geographies
  filter(!LAD25NM %in% custom_areas) %>%
  # Keep only sub-council area name and geometry, using names from DZ to MMW 
  # lookup as these are what were used in the projecions
  select("Sub-Council Area Name" = MMWard_Name, 
         "Local Authority Name" = LA_Name,
         geometry)

# Combine and simplify shapes --------------------------------------------------

# Combine shapes for MMW and custom areas
all_shapes <- rbind(mmw_shapes, custom_shapes) %>%
  # transform spatial data to raw longitude/latitude coordinates
#all_shapes <- all_shapes %>% 
  sf::st_transform('+proj=longlat +datum=WGS84') %>%
  # Simplify shapes
  rmapshaper::ms_simplify(input = ., keep = 0.02) %>%
#shapes_simp <- rmapshaper::ms_simplify(input = all_shapes, keep = 0.02)
  # Add area size to be used for population density measure
  mutate("detailed_area" = set_units(st_area(geometry), "km^2"))
# Removes units from number
attributes(all_shapes$detailed_area) = NULL

saveRDS(all_shapes,"Data files/SCAP_shapefile_2026.rds")
