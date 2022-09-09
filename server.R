server <- function(input, output) {
  
# Functions ----------------------------------------------------------------------
  
  # Function to create Scotland level map - function name = create_scot_map
  create_scot_map <- function(){
    #create leaflet object using local authority shapefiles
    leaflet(data = la_shape_data) %>%
    #create background map - OpenStreetMap by default
      addTiles() %>%
    #Add polygons for Council areas
      addPolygons(
        smoothFactor = 1, 
        weight = 1.5, 
        fillOpacity = 0.8,
        layerId = ~NAME,
        color = "black", 
    #fill polygons grey
        fillColor = "grey",
    #add Council names when hovering
        label = ~NAME
      )
  }

  # Function to add total population index to data - function name = add_pop_index
  
  # Function to create line graphs - function name = create_line_plot
  
  
# Reactive expressions (and UI output) for input selections -------------------------------------------
  
  # Reactive expression to store selection from la_choice_tab_1 - variable name = selected_la_tab_1
  
  # Reactive expression to store default small area selection - variable name = selected_small_area_tab_1
  
  # Reactive expression to store selection from year_choice_tab_1 - variable name = selected_year_tab_1
  
  # Reactive expression to store selection from age_choice_tab_1 - variable name = selected_age_tab_1
  
  # Reactive expression to store selection from gender_choice_tab_1 - variable name = selected_gender_tab_1

  
  # Reactive expression to store selection from la_choice_tab_2 - variable name = selected_la_tab_2
  
  # RenderUi to create selectizeinput small_area_output_tab_2 - inputID = small_area_choice_tab_2 
  
  # Reactive expression to store selection from small_area_output_tab_2 - variable name = selected_small_area_tab_2
  
  # Reactive expression to store selection from year_choice_tab_2 - variable name = selected_year_tab_2
  
  # Reactive expression to store selection from measure_choice_tab_2 - variable name = selected_measure_tab_2
  
  
# Code for Population Size Tab (Tab 1) -------------------------------------------

  # Run create_scot_map - variable name = scot_map_tab_1
  
  # Run add_pop_index - variable name = indexed_data_tab_1
  
  # Create data for council level map - variable name = map_data_tab_1
  
  # Combine map data with shape file - variable name = map_data_tab_1
  
  # RenderLeaflet for council level map - output name = la_map_tab_1
  
  # Create observe event to update selected_small_area_tab_1
  
  # Filter data for across areas graph - variable name = across_areas_data_tab_1
  
  # Run create_line_plot - outputID = across_areas_plot_tab_1
  
  # Filter data for within areas graph - variable name = within_areas_data_tab_1
  
  # Run create_line_plot - outputID = within_areas_plot_tab_1
  
  
# Code for Similar Areas Tab (Tab 2) ---------------------------------------------
  
  # Run create_scot_map - variable name = scot_map_tab_2
  
  # Run add_pop_index - variable name = indexed_data_tab_2
  
  # Create ranked small area data set - variable name - ranked_data_tab_2
  
  # Create data for similar area map - variable name = map_data_tab_2
  
  # Combine map data with shape file - variable name = map_data_tab_2
  
  # RenderLeaflet for similar area map - output name = la_map_tab_2
  
  # Create observe event to update selected_small_area_tab_2
  
  # Filter data for across areas graph - variable name = across_areas_data_tab_2
  
  # Run create_line_plot - outputID = across_areas_plot_tab_2
  
  # Filter data for similar areas graph - variable name = similar_areas_data_tab_2
  
  # Run create_line_plot - outputID = similar_areas_plot_tab_2
  
}