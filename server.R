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
    
    data <- left_join(total_pop_data, index_data) %>%
      left_join(dependency_data) %>% 
      left_join(sex_data)
    
    return(data)
  }
  
  # Function to create line graphs - function name = create_line_plot
  create_line_plot <- function(dataset, council_selection, small_area_selection, measure_selection){
    
    x <- renderPlotly({
      
      data <- if(measure_selection == "Total Population") {
        filter(dataset, Measure == "Population.Index")
      } else {
        if(measure_selection == "Dependency Ratio") {
         filter(dataset, Measure  == "Dependency.Ratio")
        } else {
          if(measure_selection == "Sex Ratio") {
            filter(dataset, Measure  == "Sex.Ratio")
          }
        }
      }
    
      Title <- if(measure_selection == "Total Population") {
        "Total Population Index"
      } else {
        measure_selection
      }
    
      data <- data %>% mutate(Line.Colours = "grey") 
      data$Line.Colours <- if_else(data$Area.Name == council_selection, 
                                   "skyblue",
                                   if_else(data$Area.Name == "Scotland",
                                           "dimgrey",
                                           if_else(data$Area.Name == small_area_selection,
                                                   "steelblue",
                                                   "grey"
                                                   )
                                           )
                                   )
    
      all_area_names <- unique(data$Area.Name)
      area_factors <- if(length(all_area_names) > 3) {
        area_factors <- data %>% filter(Area.Name != small_area_selection) 
        area_factors <- c(small_area_selection, unique(area_factors$Area.Name))
        } else {
          area_factors <- c(small_area_selection, council_selection, "Scotland")
        }
    
      data$Area.Name <- factor(data$Area.Name, levels = area_factors)
      data <- data %>% arrange(Area.Name)
      area_colours <- data %>% 
        group_by(Council.Name, Level, Area.Name) %>% 
        filter(row_number()== 1) 
      area_colours <- area_colours$Line.Colours
    
      plot <- ggplot(data = data) +
        geom_line(
          aes(
          x = Year, 
          y = Value, 
          group = Area.Name, 
          colour = Area.Name,
          text = paste("Area Name:",`Area.Name`, "<br>", 
                       "Year:", `Year`,"<br>",
                       "Measure:", `Title`, "<br>", 
                       "Value:",`Value`)
          ), 
          size = 0.7
        ) +
        scale_color_manual(values = area_colours) +
        labs(title = Title) +
        theme(
          plot.title = element_text(size = 11), 
          legend.title = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(vjust = 0.3, angle = 20),
          axis.text.y = element_text(size = 7),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        ) +
        scale_x_continuous(breaks = 2018:2030)
      ggplotly(plot, tooltip = c("text")) %>% 
        config(displayModeBar = F) %>% 
        layout(xaxis = list(fixedrange = TRUE)) %>% 
        layout(yaxis = list(fixedrange = TRUE)) %>%
        layout(legend = list(x = 0, y = 1))
    })
    
    return(x)
  }

# Reactive expressions (and UI output) for input selections -------------------------------------------
  
  # Reactive expression to store selection from la_choice_tab_1 - variable name = selected_la_tab_1
  selected_la_tab_1 <- reactive({
    LA <- input$la_choice_tab_1
    return(LA)
  })  
  
  # Reactive expression to store default small area selection - variable name = selected_small_area_tab_1

  # Reactive expression to store selection from year_choice_tab_1 - variable name = selected_year_tab_1
  selected_year_tab_1 <- reactive({
    Y <- input$year_choice_tab_1
    return(Y)
  })  
  
  # Reactive expression to store selection from age_choice_tab_1 - variable name = selected_age_tab_1
  selected_age_tab_1 <- reactive({
    A <- input$age_choice_tab_1
    return(A)
  })   
  
    # Reactive expression to store selection from gender_choice_tab_1 - variable name = selected_gender_tab_1
  selected_gender_tab_1 <- reactive({
    G <- input$gender_choice_tab_1
    return(G)
  })   
  
  # Reactive expression to store selection from la_choice_tab_2 - variable name = selected_la_tab_2
  selected_la_tab_2 <- reactive({
    LA <- input$la_choice_tab_2
    return(LA)
  }) 
  
  # RenderUi to create selectizeinput small_area_output_tab_2 - inputID = small_area_choice_tab_2 
  
  # Reactive expression to store selection from small_area_output_tab_2 - variable name = selected_small_area_tab_2

  # Reactive expression to store selection from year_choice_tab_2 - variable name = selected_year_tab_2
  selected_year_tab_2 <- reactive({
    Y <- input$year_choice_tab_2
    return(Y)
  }) 
  
  # Reactive expression to store selection from measure_choice_tab_2 - variable name = selected_measure_tab_2
  selected_measure_tab_2 <- reactive({
    M <- input$measure_choice_tab_2
    return(M)
  })  
  
# Code for Population Size Tab (Tab 1) -------------------------------------------

  # Run create_scot_map - variable name = scot_map_tab_1
  
  # Run add_pop_index - variable name = indexed_data_tab_1
  
  # Create data for council level map - variable name = map_data_tab_1
  map_data_tab_1 <- reactive({
    indexed_data_tab_1 <- indexed_data_tab_1()
    #filter this data based on council and year
    council_map_data <- filter(indexed_data_tab_1, Year == input$year_choice_tab_1 & Council.Name == input$la_choice_tab_1) %>%
      filter(., Level == "Small Area")
    })

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