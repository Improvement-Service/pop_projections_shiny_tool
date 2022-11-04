##code for reading and simplifying shapefiles for use in map

##read in packages
library(readr)
library(tidyverse)
library(sf)
library(leaflet)
library(rmapshaper)

shapes <- read_rds("Data files/SCAP_shapefile.rds")
old_size <- object.size(shapes)

shapes_simp <- rmapshaper::ms_simplify(input = shapes, keep = 0.02) 
shapes_simp <- shapes_simp %>%rename(`Sub-Council Area Name` = `Sub.Council.Area.Name`)
object.size(shapes_simp)

tst_shps <- shapes_simp[grep("Argyll and Bute", shapes_simp$`Sub-Council Area Name`),]
leaflet(data = tst_shps) %>% addTiles() %>% addPolygons()


system.time(leaflet(data = shapes_simp) %>% addTiles() %>% addPolygons())
system.time(leaflet(data = shapes) %>% addTiles() %>% addPolygons())

shapes_simp[shapes_simp$`Sub-Council Area Name` == "Annbank Mossblown and Tarbolton - the Coalfields - South Ayrshire",1] <- "Annbank Mossblown and Tarbolton: the Coalfields - South Ayrshire"
shapes[shapes$`Sub-Council Area Name` == "Annbank Mossblown and Tarbolton - the Coalfields - South Ayrshire",1] <- "Annbank Mossblown and Tarbolton: the Coalfields - South Ayrshire"


saveRDS(shapes_simp, "Data files/SCAP_shapefile.rds")
saveRDS(shapes, "Data files/SCAP_shapefile_detailed.rds")
