server <- function(input, output) {
  
# Functions ----------------------------------------------------------------------
  
  # Function to create Scotland level map - function name = create_scot_map
  
  # Function to add total population index to data - function name = add_pop_index
  add_pop_index <- function(gender_selection, age_selection) {
    
    total_pop_data <- projection_data %>% 
      filter(Sex == gender_selection & Age %in% age_selection) %>%
      group_by(Council.Name, Level, Area.Name, Year, Sex) %>%
      summarise(Total.Population = sum(Population)) %>%
      select(-Sex)
    
    index_data <- total_pop_data %>% 
      pivot_wider(names_from = Year, values_from = Total.Population) %>% 
      mutate(across(`2019`:`2030`, ~ .x / `2018`)) %>%
      mutate(across(`2018`, ~ .x / `2018`))
    index_data <- index_data %>% 
      mutate(across(`2018`:`2030`, ~ round(.x * 100, 1))) %>%
      pivot_longer(cols = `2018`:`2030`, names_to = "Year", values_to = "Population.Index")
    index_data$Year <- as.numeric(index_data$Year)
    
    sex_data <- projection_data %>% 
      filter(Age %in% age_selection) %>%
      group_by(Council.Name, Level, Area.Name, Year, Sex) %>%
      summarise(Total.Population = sum(Population)) %>%
      pivot_wider(names_from = Sex, values_from = Total.Population) %>%
      mutate(Sex.Ratio = round((Males / Females) * 100, 1)) %>%
      pivot_longer(cols = `Females`:`Persons`, names_to = "Sex", values_to = "Total.Population") %>%
      filter(Sex == gender_selection) %>%
      select(Council.Name, Level, Area.Name, Year, Sex.Ratio)
    
    dependency_data <- projection_data %>% filter(Sex == gender_selection & Age %in% age_selection) %>%
      group_by(Council.Name, Level, Area.Name, Year, Sex) %>%
      arrange(Age) %>%
      filter(row_number()==1) %>%
      ungroup() %>%
      select(Council.Name, Level, Area.Name, Year, Dependency.Ratio)
    
    data <- merge(total_pop_data, index_data) %>%
      merge(dependency_data) %>% 
      merge(sex_data)
    
    return(data)
  }
  
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