library(shiny, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(readxl)
library(leaflet)
library(ggplot2, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(RColorBrewer)
library(shinycssloaders)
library(sf)
library(vroom, warn.conflicts = FALSE)
library(stringr)
library(data.table, warn.conflicts = FALSE)
library(shinyanimate, warn.conflicts = FALSE)
library(DT)
library(shinyjs)
library(shinyWidgets)
library(rintrojs)

intro_df <- read.csv("Data files/intro_guide.csv")

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
#la_shape_data <- read_rds("Data files/LAShps.rds") 

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
 years_labels <- years
# years_labels[1] <- "2018 - base year"
 names(years) <- years_labels
 
 ages <- unique(projection_data$Age)
 age_names <- ages
 age_names[91] <- "90+"
 names(ages) <- age_names

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
add_pop_index <- function(raw_data, 
                          gender_selection, 
                          age_selection, 
                          lookup = small_area_lookup
                          ) {
  # Make data table copies of raw_data and lookup dataframes and 
  # assign keys (columns to index by for speed)
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
                             small_area_selection, 
                             graph_type,
                             tab,
                             yrange = NULL) {
  
  measure_title <- "Population Index"
  all_area_names <- sort(unique(dataset$LongName))
  small_area_index <- match(small_area_selection, all_area_names)
  trace_aes <- get_plot_trace_aesthetics(small_area_index)

  # Default aesthetic for within council area plots...
  # Renders the first area (by factor level) blue and the rest grey
  line_colours <- trace_aes$colours
  # Renders the first area (by factor level) as normal and the rest as more opaque
  # this prevents the selected small area line being hidden by subsequently rendered areas
  alpha_settings <- trace_aes$opacity
  source_name <- "within_areas_1"
  if(graph_type == "Across Areas") {
    line_colours <- c("grey", "dimgrey", "orange")
    alpha_settings <- c(1, 0.7, 0.7)
    source_name <- "across_areas"
  }
  if(tab == 2) {
    source_name <- "within_areas_2"
    measure_title <- unique(dataset$Measure)
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
                                                  # "<br>",
                                                  # "Measure:", 
                                                  # `measure_title`, 
                                                  "<br>", 
                                                  `measure_title`,": ",
                                                  # Format values with thousand separator
                                                  prettyNum(`Value`, 
                                                            big.mark = ",", 
                                                            scientific = FALSE
                                                  )
                                     )
  )) +
    #geom_vline(xintercept = 2018, linetype="dashed", 
    #             color = "lightgrey", size=0.7) +
    geom_line(size = 0.7#, position=position_dodge(width=0.5)
    ) +
    scale_color_manual(values = line_colours) + #rep("grey", 25)) +
    scale_alpha_manual(values = alpha_settings) + #rep(0.8, 25)) +
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
  
  if(graph_type == "Across Areas") {
    plot <- plot + ylim(yrange[1], yrange[2])
  }
  
  ggplotly(plot, tooltip = c("text"), source = source_name) %>% 
    config(displayModeBar = FALSE) %>% 
    layout(xaxis = list(fixedrange = TRUE)) %>% 
    layout(yaxis = list(fixedrange = TRUE)) %>%
    layout(legend = list(orientation = 'v', title = "", itemclick = "toggleothers"))
}

create_map <- function(map_data, 
                       tab_num,
                       default_area,
                       initial_polygon_click,
                       age_label = "", 
                       gender = "") {
  #data passed to this function should already be filtered by council and year
  council <- map_data$Council.Name[1]
  year <- map_data$Year[1]
  
  # Set colours for the map
  map_colours <- brewer.pal(8, "Blues")
  # Assign colours to quintiles
  map_colour_quintiles <- colorBin(map_colours, map_data$Value, n = 8)
  
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
    # Set colours for the map
    map_colours <- brewer.pal(8, "Purples")
    # Assign colours to quintiles
    map_colour_quintiles <- colorBin(map_colours, map_data$Value, n = 8)
  }
  
  map <-leaflet(data = map_data, 
                #the following two lines remove zoom control and reinstate it on the right
                #of the map so it doesn't obstruct drop down menu options
                options = leafletOptions(zoomControl = FALSE)
  ) %>%
    htmlwidgets::onRender("function(el, x) {
          L.control.zoom({ position: 'topright' }).addTo(this)}"
    ) %>%
    # Create background map - OpenStreetMap by default
    addProviderTiles(providers$CartoDB.VoyagerLabelsUnder) %>%
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
              #title = paste0(legend_content,", ", year),
              opacity = 1
    ) %>% 
    #this button is for resetting map view (centre on selected_la again)
    addEasyButton(easyButton(
      icon = icon("rotate-left", lib = "font-awesome"),
      title = 'Reset view',
      position = "topright",
      onClick = JS("function(btn, map) { 
       var groupLayer = map.layerManager.getLayerGroup('year_layer');
       map.fitBounds(groupLayer.getBounds());
    }")  
    )) %>%
    addPolygons(
      smoothFactor = 1,
      weight = 1.5,
      fillOpacity = 0.8,
      layerId = ~SubCouncil,
      group = "year_layer",
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
    )
  #render the map with no highlight (only add highlight if initial polygon selected)
  if (initial_polygon_click == TRUE) {
    default_selected_polygon <- shape_data %>% 
      filter(SubCouncil == default_area) %>% 
      pull(geometry)
    
    map <- map %>%
      addPolylines(stroke = TRUE,
                   weight = 3,
                   color = "orange",
                   opacity = 0.7,
                   data = default_selected_polygon,
                   group ="highlighted_polygon")
  }
  
  return(map)
} #end of create_map()

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

#function to re-render orange lines on map or plot lick. 
#Takes an integer (which in practice is is obtained from the plotly-click curveNumber or event$id)
get_plot_trace_aesthetics <- function(trace_number) {
  colours <- c(rep("lightgrey",24))
  colours[trace_number] <- "orange"
    
  opacity <- c(rep(0.7,24))
  opacity[trace_number] <- 1
  return(list(colours = colours, opacity = opacity))
}

update_across_area_proxy <- function(proxy, small_area, data){
  area_data <- data %>%
    filter(LongName == small_area)
  
  proxy %>%
  plotlyProxyInvoke("deleteTraces", list(as.integer(2)))%>%
    plotlyProxyInvoke("addTraces", list(list(x = area_data$Year,
                                             y = area_data$Value,
                                             text = c(rep("Population Index",13)),
                                             customdata = c(rep(str_replace(small_area, "\n", "<br>"), 13)),
                                             mode = "lines",
                                             line = list(color = 'orange', width = 2, shape = 'spline'),
                                             hovertemplate='<br>Area Name: %{customdata}<br>Year: %{x}<br>%{text}: %{y}<extra></extra>',
                                             name = str_replace_all(small_area, "\n", "<br>"))
    ))
}


filter_n_format <- function(dataframe, lookup, councils, ages, years, sex) {
  data <- as.data.table(dataframe)
  setkey(data, Council.Name, Area.Name, Year, Sex, Age)
  small_area_lookup <- as.data.table(lookup)
  setkey(small_area_lookup, Area.Name, Council.Name)
  
  filtered_data <- data[Council.Name %in% councils & 
                          Sex %in% sex & 
                          Age %in% ages & 
                          Year %in% years][
                            small_area_lookup, 
                            on =.(Area.Name = Area.Name), 
                            LongName := i.LongName
                          ] %>% 
    dcast(Council.Name + Level + Area.Name + LongName + Sex + Age ~ Year, value.var = "Population")
  
  filtered_data$Age[filtered_data$Age == 90] <- "90+"
  # filtered_data$Age <- as.character(filtered_data$Age)
  print(class(filtered_data$Age))
  
  formatted_data <- filtered_data %>% 
    mutate(LongName = ifelse(is.na(LongName), 
                             paste0(Area.Name, " Total"), 
                             stringr::str_replace_all(LongName, "\n", " "))) %>%
    select(!c(Level, Area.Name)) %>%
    dplyr::rename(Council = Council.Name, "Sub-Council Area" = LongName)
  
  return(as.data.frame(formatted_data))
}
