server <- function(input, output, session) {
  
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
      summarise(`Total Population` = sum(Population)) %>%
      select(-Sex)
    
    index_data <- total_pop_data %>% 
      pivot_wider(names_from = Year, values_from = `Total Population`) %>% 
      mutate(across(`2019`:`2030`, ~ .x / `2018`)) %>%
      mutate(across(`2018`, ~ .x / `2018`))
    index_data <- index_data %>% 
      mutate(across(`2018`:`2030`, ~ round(.x * 100, 1))) %>%
      pivot_longer(cols = `2018`:`2030`, names_to = "Year", values_to = "Population.Index")
    index_data$Year <- as.numeric(index_data$Year)
    
    sex_data <- projection_data %>% 
      filter(Age %in% age_selection) %>%
      group_by(Council.Name, Level, Area.Name, Year, Sex) %>%
      summarise(`Total Population` = sum(Population)) %>%
      pivot_wider(names_from = Sex, values_from = `Total Population`) %>%
      mutate(`Gender Ratio` = round((Males / Females) * 100, 1)) %>%
      pivot_longer(cols = `Females`:`Persons`, names_to = "Sex", values_to = "Total Population") %>%
      filter(Sex == gender_selection) %>%
      select(Council.Name, Level, Area.Name, Year, `Gender Ratio`)
    
    dependency_data <- projection_data %>% filter(Sex == gender_selection & Age %in% age_selection) %>%
      group_by(Council.Name, Level, Area.Name, Year, Sex) %>%
      arrange(Age) %>%
      filter(row_number()==1) %>%
      ungroup() %>%
      select(Council.Name, Level, Area.Name, Year, Dependency.Ratio)
    
    data <- left_join(total_pop_data, index_data) %>%
      left_join(dependency_data) %>% 
      left_join(sex_data) %>%
      left_join(lookup, by = c("Area.Name" = "ShortName", "Council.Name" = "Council"))
    data[is.na(data$LongName), "LongName"] <- data[is.na(data$LongName), "Area.Name"]
    
    data <- data %>% 
      pivot_longer(cols = `Total Population`:`Gender Ratio`, 
                   names_to = "Measure", 
                   values_to = "Value"
      )
    
    return(data)
  }
  
  # Function to identify four small areas (by Long Name) which are most similar 
  # to the user-selected similar area for the selected year and measure
  find_similar_areas <- function(data, selectedSmallArea, yr, msr) {
    
    small_area_data <- data %>%
      filter(Level == "Small Area",
             Measure == msr,
             Year == yr
             ) %>%
      arrange(desc(Value))
    
    index <- which(small_area_data$LongName == selectedSmallArea)
    value <- small_area_data$Value[index]
    
    ranked_data <- small_area_data %>%
      mutate(Difference = abs(Value - value)) %>%
      arrange(Difference)
    
    similar_areas <- ranked_data[1:5,] %>% 
      filter(LongName != selectedSmallArea) %>%
      pull(LongName)
    
    return(similar_areas)
  }
  
  # Function to create line graphs - function name = create_line_plot
  create_line_plot <- function(dataset, 
                               council_selection, 
                               small_area_selection, 
                               measure_selection,
                               graph_type) {
    
    # Graphs use population index but measure selection is called Total Population
    # Need these to match 
    data <- if(measure_selection == "Total Population") {
      filter(dataset, Measure == "Population.Index")
      } else {
        filter(dataset, Measure  == measure_selection)
        }
          
    # This will set the measure title for the graphs
    # The graphs use population index data but the drop down is labelled total population so 
    # use this to match them
    measure_title <- if(measure_selection == "Total Population") {
      "Total Population Index"
    } else {
      measure_selection
    }
    
    # Adds a column to the data with line colours
    # Sets the line colours dependent on the area
    data <- data %>% mutate(Line.Colours = "grey") 
    data$Line.Colours <- if_else(data$LongName == council_selection, 
                                 "skyblue",
                                 if_else(data$LongName == "Scotland",
                                         "dimgrey",
                                         if_else(data$LongName == small_area_selection,
                                                 "steelblue",
                                                 "grey"
                                                 )
                                         )
                                 )

    # The area names need to be stored as a factor so that the order of the areas can be set
    # If this is not done the areas will be ordered alphabetically and the colours will be out of order
    # This stores the levels if statement stores the levels for the factor
    # Across areas graph will show selected small area, selected council and Scotland - in this order
    # Within areas graph will show selected small area, all other small areas within that council - in this order
    # For within areas graph more than 3 areas will be in the data 
    # If statement tests whether there are more than 3 areas, if so levels for factor are set in line with within areas graph
    # If not levels for factor are set in line with across areas graph
    all_area_names <- unique(data$LongName)
    area_factors <- if(length(all_area_names) > 3) {
      area_factors <- data %>% filter(LongName != small_area_selection) 
      area_factors <- c(small_area_selection, unique(area_factors$LongName))
      } else {
        area_factors <- c(small_area_selection, council_selection, "Scotland")
      }
    
    data$LongName <- factor(data$LongName, levels = area_factors)
    data <- data %>% arrange(LongName)
    # Store the line colours arranged in order of how they should be mapped
    area_colours <- data %>% 
      group_by(Council.Name, Level, LongName) %>% 
      filter(row_number()== 1) 
    area_colours <- area_colours$Line.Colours
      
    # Create plot object
    plot <- ggplot(data = data) +
      geom_line(
        aes(
        x = Year, 
        y = Value, 
        group = LongName, 
        colour = LongName,
        # Creates text for hoverover label
        text = paste("Area Name:",`LongName`, "<br>", 
                     "Year:", `Year`,"<br>",
                     "Measure:", `measure_title`, "<br>", 
                     "Value:",`Value`)
        ), 
        size = 0.7
      ) +
      scale_color_manual(values = area_colours) +
      labs(title = graph_type) +
      theme(
        plot.title = element_text(size = 9), 
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
      config(displayModeBar = F) %>% 
      layout(xaxis = list(fixedrange = TRUE)) %>% 
      layout(yaxis = list(fixedrange = TRUE)) %>%
      layout(legend = list(orientation = 'h', title = "", font = list(size = 8)))
  }

# Reactive expressions (and UI output) for input selections -------------------------------------------
  
  # Reactive expression to store default small area selection - variable name = selected_small_area_tab_1
  
   #this will be initialised when the use selects an LA and will be updated
   #either when a new LA is selected or the user clicks on the map
  selected_small_area_tab_1 <- reactiveVal()
  
  # Reactive expression to store selection from gender_choice_tab_1 - variable name = selected_gender_tab_1
  selected_gender_tab_1 <- reactive({
    # Length greater than 1 means both male and female are selected so should return "Persons"
    # so the data can be filtered as such
    G <- if(length(input$gender_choice_tab_1) > 1){
      "Persons"
    } else {
      input$gender_choice_tab_1
    }
    return(G)
  })   
  
  # Reactive expression to store selection from age_choice_tab_1 - variable name = selected_age_tab_1
  selected_age_tab_1 <- reactive({
    # slider input only returns first and last values so need to create a vector with all values
    first_age <- input$age_choice_tab_1[1]
    last_age <- input$age_choice_tab_1[2]
    full_range <- c(first_age:last_age)

    return(full_range)
  })   
  
  # Reactive expression to store small areas within selected_la_tab_2 - variable name = small_area_choices_tab_2
  small_area_choices_tab_2 <- reactive({
    req(input$la_choice_tab_2)
    small_areas_subset <- small_area_lookup %>%
          filter(Council.Name == input$la_choice_tab_2) %>%
           pull(LongName)
    return(small_areas_subset)
    
  })
  
  # RenderUi to create selectizeinput small_area_output_tab_2 - inputID = small_area_choice_tab_2 
  output$small_area_output_tab_2 <- renderUI({
    selectizeInput(inputId = "small_area_choice_tab_2", 
                  choices = small_area_choices_tab_2(),
                  label = NULL,
                  options = list(placeholder = "Select Small Area",
                                 #placeholder = small_area_choices_tab_2()[1],
                                 onInitialize = I('function() { this.setValue(""); }')
                  )
    )
  })
  
# Code for Population Size Tab (Tab 1) -------------------------------------------

  # Run create_scot_map - variable name = scot_map_tab_1
  
  output$scot_map_tab_1 <- renderLeaflet({
    create_scot_map()
  })
  
  # Create reactive data for map - variable name = map_data_tab_1
  map_data_tab_1 <- reactive({
    # Run add_pop_index using input values
    indexed_data <- add_pop_index(gender_selection = selected_gender_tab_1(), 
                                  age_selection = selected_age_tab_1()
                                  )
    # Filter this data based on council and year
    council_map_data <- filter(indexed_data, 
                               Council.Name == input$la_choice_tab_1,
                               Level == "Small Area",
                               Year == input$year_choice_tab_1,
                               Measure == "Total.Population"
                               ) %>%
      ungroup()
    
    # Filter shape file to selected council before combining with data
    filtered_shape <- filter(shape_data, Council == input$la_choice_tab_1)
    # Combine map data with shape file 
    combined_data <- left_join(filtered_shape, 
                               council_map_data,
                               by = c("SubCouncil" = "LongName")
    )
  })

  # RenderLeaflet for council level map - output name = la_map_tab_1
  output$la_map_tab_1 <- renderLeaflet({
    
    # Call reactive map data
    map_data_tab_1 <- map_data_tab_1()
    
    # store selected age
    selected_age_tab_1 <- selected_age_tab_1()
    # label of the ages included, if more than 1 age is selected is will be presented as "16-64"
    age_label <- if(length(selected_age_tab_1) > 1) { 
      paste(first(selected_age_tab_1), "-", last(selected_age_tab_1))
    } else {
      selected_age_tab_1
    }

    # Store selected gender
    selected_gender_tab_1 <- selected_gender_tab_1()
    
    # Set colours for the map
    map_colours <- brewer.pal(8, "Blues")
    # Assign colours to quintiles
    map_colour_quintiles <- colorBin(map_colours, map_data_tab_1$Value, n = 8)
    
    # Create a leaflet object using small area shapefiles
    leaflet(map_data_tab_1) %>%
      # Create background map - OpenStreetMap by default
      addTiles() %>%
      # Add polygons for small areas
      addPolygons(smoothFactor = 1, 
                  weight = 1.5, 
                  fillOpacity = 0.8,
                  layerId = ~SubCouncil,
                  color = "black", 
                  # colour of polygons should map to population quintiles
                  fillColor = ~map_colour_quintiles(Value),
                  # Use HTML to create popover labels with all the selected info
                  label = (sprintf(
                    "<strong>%s</strong><br/>Year: %s<br/>Age: %s<br/>Gender: %s<br/>Population: %s",
                    map_data_tab_1$SubCouncil, 
                    map_data_tab_1$Year,
                    age_label,
                    selected_gender_tab_1,
                    map_data_tab_1$Value)
                    %>% lapply(htmltools::HTML)
                    ),
                  # Creates a white border on the polygon where the mouse hovers
                  highlightOptions = highlightOptions(color = "white", weight = 3, bringToFront = TRUE)
                  ) %>%
      addLegend("bottomleft", 
                colors = map_colours,
                labels = c("Smallest Population", "","","","","","","Largest Population"),
                title = "",
                opacity = 1
                ) 
  })
  
  # Create observe event to update selected_small_area_tab_1
  observe({
    event <- input$la_map_tab_1_shape_click
    if(is.null(event)){
      return()} 
    selected_small_area_tab_1(event$id)
  })
  
  observe({
    event <- input$la_choice_tab_1
    if(is.null(event)){
      return()} 
    small_area_options <- small_area_lookup %>%
      filter(Council.Name == input$la_choice_tab_1) %>%
      pull(LongName)
    default_area <- small_area_options[1]
    selected_small_area_tab_1(default_area)
    })

  # Create an overall title for the graphs on tab 1
  output$tab_1_plots_title <- renderText({
    "Indexed Change"
  })
  
  # Filter data for across areas graph - variable name = across_areas_data_tab_1
  across_areas_data_tab_1 <- reactive({
    # run function to add index to data then filter to selected Council and small area
    across_data <- add_pop_index(gender_selection = selected_gender_tab_1(), 
                                 age_selection = selected_age_tab_1()
                                 ) %>%
      filter(Council.Name %in% c(input$la_choice_tab_1, "Scotland") & 
               LongName %in% c(input$la_choice_tab_1, "Scotland", selected_small_area_tab_1())
             )
    return(across_data)
  })
  
  # Run create_line_plot - outputID = across_areas_plot_tab_1
  output$across_areas_plot_tab_1 <- renderPlotly({
    plot <- create_line_plot(dataset = across_areas_data_tab_1(), 
                             council_selection = input$la_choice_tab_1, 
                             small_area_selection = selected_small_area_tab_1(), 
                             measure_selection = "Total Population",
                             graph_type = "Across Areas"
    )
  })
  
  # Filter data for within areas graph - variable name = within_areas_data_tab_1
  within_areas_data_tab_1 <- reactive({
   x <- add_pop_index(gender_selection = selected_gender_tab_1(), 
                      age_selection = selected_age_tab_1()
                      ) %>%
                      filter(Council.Name == input$la_choice_tab_1 & Level == "Small Area")
   return(x)
   })

  # Run create_line_plot - outputID = within_areas_plot_tab_1
  output$within_areas_plot_tab_1 <- renderPlotly({
    plot <- create_line_plot(dataset = within_areas_data_tab_1(), 
                             council_selection = input$la_choice_tab_1, 
                             small_area_selection = selected_small_area_tab_1(), 
                             measure_selection = "Total Population",
                             graph_type = "Within Council Areas"
    )
  })
  
  
# Code for Similar Areas Tab (Tab 2) ---------------------------------------------
  
  # Run create_scot_map - variable name = scot_map_tab_2
  output$scot_map_tab_2 <- renderLeaflet({
    create_scot_map()
  })
  
  # Run add_pop_index - variable name = indexed_data_tab_2
  indexed_data_tab_2 <- add_pop_index(gender_selection = "Persons", 
                                      age_selection = c(0:90)
                                      )

# Create ranked small area data set - variable name - ranked_data_tab_2
  ranked_data_tab_2 <- reactive({
    # Step 1: call find_similar_areas() function  to return four areas most similar to 
    # selected small area based on other selections
    similar_areas <- find_similar_areas(data = indexed_data_tab_2, 
                                        selectedSmallArea = input$small_area_choice_tab_2, 
                                        yr = input$year_choice_tab_2, 
                                        msr = input$measure_choice_tab_2
                                        )

    # Step 2: create a new dataset (inherited from indexed_data_tab_2) with a new column 
    # which assigns similar area names (and the selected area name) 
    # with a colour Dark Blue or Light Blue or Grey.
    ranked_data_tab_2 <- indexed_data_tab_2 %>% 
      filter(Level == "Small Area") %>%
      mutate(Shape.Colour = "grey")
    ranked_data_tab_2$Shape.Colour[ranked_data_tab_2$LongName %in% similar_areas] <- "#a6bddb"
    ranked_data_tab_2$Shape.Colour[ranked_data_tab_2$LongName == input$small_area_choice_tab_2] <- "#034e7b"
    
    return(ranked_data_tab_2)
  })
  
  # RenderLeaflet for similar area map - output name = la_map_tab_2
  output$la_map_tab_2 <- renderLeaflet({
    
    # Call reactive map data
    map_data_tab_2 <- ranked_data_tab_2()
    
    # Filter data to selected year and selected measure
    map_data_tab_2 <- map_data_tab_2 %>% 
      filter(Year == input$year_choice_tab_2, Measure == input$measure_choice_tab_2)
    
    # Combine map data with shape file
    map_data_tab_2 <- left_join(shape_data, 
                                map_data_tab_2, 
                                by = c("SubCouncil" = "LongName")
    )
    
    # Create a leaflet object using small area shapefiles
    leaflet(map_data_tab_2) %>%
      # Create background map - OpenStreetMap by default
      addTiles() %>%
      # Add polygons for small areas
      addPolygons(smoothFactor = 1, 
                  weight = 1.5, 
                  fillOpacity = 0.8,
                  layerId = ~SubCouncil,
                  color = "black", 
                  # colour of polygons should map to Shape.Colour column
                  fillColor = ~Shape.Colour,
                  # Use HTML to create popover labels with all the selected info
                  label = (sprintf(
                    "<strong>%s</strong><br/>Council: %s<br/>Year: %s<br/>Measure: %s<br/>Value: %s",
                    map_data_tab_2$SubCouncil, 
                    map_data_tab_2$Council,
                    map_data_tab_2$Year,
                    map_data_tab_2$Measure,
                    map_data_tab_2$Value)
                    %>% lapply(htmltools::HTML)
                  ),
                  # Creates a white border on the polygon where the mouse hovers
                  highlightOptions = highlightOptions(color = "white", weight = 3, bringToFront = TRUE)
      ) %>%
      addLegend("bottomleft", 
                colors = c("#034e7b","#a6bddb","grey"),
                labels = c(input$small_area_choice_tab_2, "Similar Areas", "All Other Areas"),
                title = "",
                opacity = 1
      ) 
  })
  
  # Create observe event to update small area drop-down (id = small_area_choice_tab_2)
  # to reflect value of small area clicked 
  observe({
    event <- input$la_map_tab_2_shape_click
    if(is.null(event)){
      return()} 
    updateSelectizeInput(session, inputId = "small_area_choice_tab_2", selected = event$id)
  })
  
  # Filter data for across areas graph - variable name = across_areas_data_tab_2
  
  # Run create_line_plot - outputID = across_areas_plot_tab_2
  
  # Filter data for similar areas graph - variable name = similar_areas_data_tab_2
  
  # Run create_line_plot - outputID = similar_areas_plot_tab_2
  
}
