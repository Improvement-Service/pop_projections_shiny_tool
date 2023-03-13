library(shiny, warn.conflicts = FALSE)
library(tidyverse)
library(readxl)
library(leaflet)
library(ggplot2, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(RColorBrewer)
library(shinycssloaders)
library(sf)
library(vroom)
library(shinyvalidate, warn.conflicts = FALSE)
library(stringr)

library(data.table, warn.conflicts = FALSE)
library(shinyanimate, warn.conflicts = FALSE)
library(DT)
library(shinyjs)

# Read raw data ------------
projection_data <- vroom::vroom("Data files/Population Projections With Aggregations.csv", 
                                delim = ",", 
                                col_names = TRUE, 
                                show_col_types = FALSE
                                )
projection_data <- projection_data %>% 
  mutate_at(vars(Population), funs(round(., 0)))
measures_data <- vroom::vroom("Data files/Other measures data.csv", 
                              delim = ",", 
                              col_names = TRUE, 
                              show_col_types = FALSE
                              )
measures_data <- measures_data %>% 
  mutate_at(vars(Value), funs(round(., 0)))

shape_data <- read_rds("Data files/SCAP_shapefile.rds")
la_shape_data <- read_rds("Data files/LAShps.rds") 

# Small-area look ups ---------
small_area_lookup <- vroom::vroom("Data files/ShortNameLookup.csv", 
                                  delim = ",", 
                                  col_names = TRUE, 
                                  show_col_types = FALSE
                                  ) %>%
  rename("Area.Name" = "ShortName", "Council.Name" = "Council")
# Constrain the width of long area names so that the legend can be narrower in plots
# this is done for shape data below as well, these must be consistent
small_area_lookup$LongName <- str_wrap(small_area_lookup$LongName, 13)

# Global variables ----------
councils <- unique(projection_data$Council.Name[projection_data$Council.Name != "Scotland"])
years <- unique(projection_data$Year)

# Manipulate data objects -----------
measures_data <- left_join(measures_data, 
                           small_area_lookup, 
                           by = c("Council.Name", "Area.Name")
                           )
# Fix issue with NA lookup in Highland
measures_data$LongName[measures_data$Area.Name == "NA"] <- "Nairn"
# Fix Long Names for council level (set longname equal to council name at council level)
measures_data$LongName[is.na(measures_data$LongName)] <- measures_data$Council.Name[is.na(measures_data$LongName)]
# Add area level
measures_data$Level <- if_else(measures_data$Council.Name == measures_data$LongName,
                               "Council",
                               "Small Area"
                               )

# Split out council and sub-council in shape_data for merging
shape_data <- shape_data %>% 
  separate(`Sub-Council Area Name`, 
           into = c("SubCouncil", "Council"), 
           sep = " - ", 
           remove = TRUE
           )
# Fix error in annbank name
shape_data[shape_data$SubCouncil == "Annbank Mossblown and Tarbolton: the Coalfields", "SubCouncil"] <- "Annbank Mossblown and Tarbolton - the Coalfields"
shape_data$SubCouncil <- str_wrap(shape_data$SubCouncil, 13)
# Replace "and" with ampersand in shapefiles - this is to allow merging with projection_data
shape_data$Council <- gsub(" and ", " & ", shape_data$Council)

# Functions -------------

# Function to add total population index to data
add_pop_index <- function(raw_data, gender_selection, age_selection, lookup = small_area_lookup) {
  data <- as.data.table(raw_data)
  setkey(data, Council.Name, Area.Name, Year, Sex, Age)
  small_area_lookup <- as.data.table(lookup)
  setkey(small_area_lookup, Area.Name, Council.Name)
  
  total_pop_data <- data[Sex == gender_selection & Age %in% age_selection, 
                         .(Total.Population = sum(Population)), 
                         by = .(Council.Name, Level, Area.Name, Year, Sex)
  ][, Sex:= NULL
  ][, 
    Population.Index := lapply(.SD, function(x) round((x/x[1]) * 100, 1)), 
    by = .(Council.Name, Level, Area.Name), 
    .SDcols = c("Total.Population")
  ][small_area_lookup, on =.(Area.Name = Area.Name), 
    LongName := i.LongName
  ] %>%
    melt(total_pop_data, 
         id.vars = c("Council.Name", 
                     "Level", 
                     "Area.Name", 
                     "LongName", 
                     "Year"
         ),
         measure.vars = c("Total.Population", "Population.Index"),
         variable.name = "Measure",
         value.name = "Value"
    )
  
  total_pop_data$LongName[is.na(total_pop_data$LongName)] <- total_pop_data$Council.Name[is.na(total_pop_data$LongName)]
  
  return(total_pop_data)
}

# Function to create line graphs
create_line_plot <- function(dataset, 
                             council_selection, 
                             small_area_selection, 
                             measure_selection,
                             graph_type
                             ) {
  
  measure_title <- measure_selection
  all_area_names <- unique(dataset$LongName)
  
  # Default aesthetic for within council area plots...
  # Renders the first area (by factor level) blue and the rest grey
  line_colours <- c("orange", rep("grey", 23))
  # Renders the first area (by factor level) as normal and the rest as more opaque
  # this prevents the selected small area line being hidden by subsequently rendered areas
  alpha_settings <- c(1, rep(0.5, 23))
  
  if(graph_type == "Across Areas") {
    line_colours <- c("orange", "grey", "dimgrey")
    alpha_settings <- c(1, 1, 1)
  }
  
  # Create plot object
  plot <- ggplot(data = dataset, aes(x = Year, 
                                     y = Value, 
                                     group = LongName, 
                                     colour = LongName,
                                     alpha = LongName,
                                     # Creates text for hoverover label
                                     text = paste("Area Name:",
                                                  `LongName`, 
                                                  "<br>", 
                                                  "Year:", 
                                                  `Year`,
                                                  "<br>",
                                                  "Measure:", 
                                                  `measure_title`, 
                                                  "<br>", 
                                                  "Value:",
                                                   # Format values with thousand separator
                                                  prettyNum(`Value`, 
                                                            big.mark = ",", 
                                                            scientific = FALSE
                                                            )
                                                  )
                                     )) +
    geom_line(size = 0.7) +
    scale_color_manual(values = line_colours) +
    scale_alpha_manual(values = alpha_settings) +
    labs(title = "", color = "", alpha = "") +
    theme(plot.title = element_text(size = 9), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(vjust = 0.3, angle = 20, size = 6),
          axis.text.y = element_text(size = 7),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
    ) +
    scale_x_continuous(breaks = 2018:2030)
  
  ggplotly(plot, tooltip = c("text")) %>% 
    config(displayModeBar = FALSE) %>% 
    layout(xaxis = list(fixedrange = TRUE)) %>% 
    layout(yaxis = list(fixedrange = TRUE)) %>%
    layout(legend = list(orientation = 'v', title = ""))
}

create_map <- function(map_data, 
                       council, 
                       year, 
                       tab_num, 
                       age_label = "", 
                       gender = "") {
  
  # Set colours for the map
  map_colours <- brewer.pal(8, "Blues")
  # Assign colours to quintiles
  map_colour_quintiles <- colorBin(map_colours, map_data$Value, n = 8)
  
  small_area_options <- small_area_lookup %>%
    filter(Council.Name == council) %>%
    pull(LongName)
  
  default_area <- small_area_options[1]
  
  default_selected_polygon <- shape_data %>% 
    filter(SubCouncil == default_area) %>% 
    pull(geometry)
  
  # Tab 1 content
  hover_content <- ""
  legend_content <- ""
  if (tab_num == 1) {
    hover_content <- sprintf("<strong>%s</strong><br/>Year: %s<br/>Age: %s<br/>Gender: %s<br/>Population: %s",
                             map_data$SubCouncil, 
                             map_data$Year,
                             age_label,
                             gender,
                             # Format values with thousand separator
                             prettyNum(map_data$Value, 
                                       big.mark = ",", 
                                       scientific = FALSE
                                       )
                             )
    legend_content <- "Population"
    } else if (tab_num == 2) {
      
      hover_content <- sprintf("<strong>%s</strong><br/>Year: %s<br/>%s: %s",
                               map_data$SubCouncil, 
                               map_data$Year,
                               map_data$Measure,
                               # Format values with thousand separator
                               prettyNum(map_data$Value, 
                                         big.mark = ",", 
                                         scientific = FALSE
                                         )
                               )
      legend_content <- "Value"
      }
  
  leaflet(data = map_data, 
          #the following two lines remove zoom control and reinstate it on the right
          #of the map so it doesn't obstruct drop down menu options
          options = leafletOptions(zoomControl = FALSE)
          ) %>%
          htmlwidgets::onRender("function(el, x) {
          L.control.zoom({ position: 'topright' }).addTo(this)}"
                                ) %>%
    # Create background map - OpenStreetMap by default
    addTiles() %>%
    # Add polygons for small area
    addPolygons(smoothFactor = 1,
                weight = 1.5, 
                fillOpacity = 0.8,
                layerId = ~SubCouncil,
                color = "black", 
                # colour of polygons should map to population quintiles
                fillColor = ~map_colour_quintiles(Value),
                # Use HTML to create popover labels with all the selected info
                label = hover_content %>% lapply(htmltools::HTML),
                # Creates a white border on the polygon where the mouse hovers
                highlightOptions = highlightOptions(color = "white", 
                                                    weight = 5, 
                                                    bringToFront = FALSE
                )
    ) %>%
    addLegend("bottomleft", 
              colors = map_colours,
              labels = c(paste0("Smallest ", legend_content), 
                         "",
                         "",
                         "",
                         "",
                         "",
                         "",
                         paste0("Largest ", legend_content)
              ),
              title = paste0("Population, ", year),
              opacity = 1
    ) %>%
    addPolylines(stroke = TRUE,
                 weight = 3,
                 color = "orange",
                 opacity = 0.7,
                 data = default_selected_polygon, 
                 group ="highlighted_polygon"
    )
}

# Function to update the orange highlighted polygon on LA maps.
update_highlighted_polygon <- function(proxy, small_area) {
  selected_polygon <- shape_data %>% 
    filter(SubCouncil == small_area) %>% 
    pull(geometry)
  
  # Remove any previously highlighted polygon
  proxy %>% clearGroup("highlighted_polygon") %>% 
    addPolylines(stroke = TRUE,
                 weight = 3,
                 color = "orange",
                 opacity = 0.7,
                 data = selected_polygon,
                 group = "highlighted_polygon"
    )
}