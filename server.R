server <- function(input, output) {
# Input Validation --------------------------------
  # initialise an InputValidator object
  iv <- InputValidator$new()
  
  # Add validation rules: council and year choice cannot be null
  iv$add_rule("la_choice_tab_1", sv_required())
  iv$add_rule("year_choice_tab_1", sv_required())
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
  add_pop_index <- function(data, gender_selection, age_selection) {
    setDT(data)
    setkey(data, Council.Name, Area.Name, Year, Sex, Age)
    setDT(lookup)
    setkey(lookup, ShortName, Council)
    total_pop_data <- data[Sex == gender_selection & Age %in% age_selection, 
                           .(Total.Population = sum(Population)), 
                           by = .(Council.Name, Level, Area.Name, Year, Sex)
                           ][, Sex:= NULL
                             ][, 
                               Population.Index := lapply(.SD, function(x) round((x/x[1])*100,1)), 
                               by = .(Council.Name, Level, Area.Name), 
                               .SDcols = c("Total.Population")
                               ][lookup, on =.(Area.Name = ShortName), LongName := i.LongName
                                 ] %>%
      melt(total_pop_data, 
           id.vars = c("Council.Name", "Level", "Area.Name", "LongName", "Year"),
           measure.vars = c("Total.Population", "Population.Index"),
           variable.name = "Measure",
           value.name = "Value"
           )
    
    total_pop_data$LongName[is.na(total_pop_data$LongName)] <- total_pop_data$Council.Name[is.na(total_pop_data$LongName)]
    return(total_pop_data)
  }
  
  # Function to create line graphs - function name = create_line_plot
  create_line_plot <- function(dataset, 
                               council_selection, 
                               small_area_selection, 
                               measure_selection,
                               graph_type
                               ) {
    
    measure_title <- measure_selection
    
    all_area_names <- unique(dataset$LongName)
    
    line_colours <- c("steelblue", "skyblue", "dimgrey")
    if(length(all_area_names) > 3) {
      line_colours <- c("steelblue", rep("grey", 23))
    }
    alpha_settings <- c(1,1,1)
    if(length(all_area_names) > 3) {
      alpha_settings <- c(1, rep(0.5, 23))
    }

    # Create plot object
    plot <- ggplot(data = dataset) +
      geom_line(
        aes(
        x = Year, 
        y = Value, 
        group = LongName, 
        colour = LongName,
        alpha = LongName,
        # Creates text for hoverover label
        text = paste("Area Name:",`LongName`, "<br>", 
                     "Year:", `Year`,"<br>",
                     "Measure:", `measure_title`, "<br>", 
                     "Value:",`Value`)
        ), 
        size = 0.7
       ) +
      scale_color_manual(values = line_colours) +
      scale_alpha_manual(values = alpha_settings) +
      labs(title = "",#paste0(graph_type, " Population Index"),
           #labels for all aes() setting must be changed to change legend label
           color = "",
           alpha = "") +
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
      layout(legend = list(orientation = 'v', title = ""))
  }

# Reactive expressions (and UI output) for input selections -------------------------------------------
  #'turn on' user input validation when submit clicked for first time - missing fields will then show error
  observeEvent(input$submit_tab_1, {
    iv$enable()
  })
  
  render_across_scotland_text <- eventReactive(
    list(input$submit_tab_1, input$la_map_tab_1_shape_click), {
      req(iv$is_valid())
      paste0("This graph shows projected population change for ", 
             selected_small_area_tab_1(),
             ", for ",
             input$la_choice_tab_1, 
             ", and for Scotland as a whole. Change the small area shown by clicking on the map."
             )
      }
    )
  
  output$across_scotland_text <- renderText({render_across_scotland_text()})
  
  render_within_la_text <- eventReactive(
    list(input$submit_tab_1, input$la_map_tab_1_shape_click), {
    req(iv$is_valid())
    paste0("This graph shows population change for ", 
           selected_small_area_tab_1(),
           " compared to other small areas in ",
           input$la_choice_tab_1,
           ". Hover over map or click on small areas in the legend to explore the data."
           )
    }
    )
  
  output$within_la_text <- renderText({render_within_la_text()})
  
  # Calculate population index based on user input for gender and age
  pop_index_data <- reactive({
    data <- projection_data %>% 
      add_pop_index(gender_selection = selected_gender_tab_1(),
                    age_selection = selected_age_tab_1()
                    ) 
    return(data)
  })
  
  total_population_index_data <- reactive({
    filtered_data <- pop_index_data() %>%
      filter(Measure == "Population.Index")
    return(filtered_data)
  })
  
  # The first time submit is clicked with all inputs, show a notification 
  # which signposts less obvious dashboard functionality
  observeEvent(input$submit_tab_1, {
    req(iv$is_valid())
    showNotification(
      "Hover over small areas to see population index for the selected year. Click on small areas to update data shown on the graphs.",
      duration = NA, 
      closeButton = TRUE,
      type = "warning",
      session = getDefaultReactiveDomain()
      )
  }, once = TRUE
  )
  
  # Reactive expression to store default small area selection - variable name = selected_small_area_tab_1
  
  # This will be initialised when the use selects an LA and will be updated
  # either when a new LA is selected or the user clicks on the map
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
    # Filter this data based on council and year
    council_map_data <- pop_index_data() %>%
      filter(Council.Name == input$la_choice_tab_1,
             Level == "Small Area",
             Year == input$year_choice_tab_1,
             Measure == "Total.Population") %>%
      ungroup()
    
    # Filter shape file to selected council before combining with data
    filtered_shape <- filter(shape_data, Council == input$la_choice_tab_1)
    # Combine map data with shape file 
    combined_data <- left_join(filtered_shape, 
                               council_map_data,
                               by = c("SubCouncil" = "LongName")
    )
  })
  
  
  render_la_map_tab_1 <- eventReactive(input$submit_tab_1, {
    #do not run unless all input present
    req(iv$is_valid())
    
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

  # RenderLeaflet for council level map - output name = la_map_tab_1
  output$la_map_tab_1 <- renderLeaflet({
   
    render_la_map_tab_1()
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
  
  # Filter data for across areas graph - variable name = across_areas_data_tab_1
  across_areas_data_tab_1 <- reactive({
    # use indexed data then filter to selected Council and small area
    across_data <- total_population_index_data() %>%
      filter(Council.Name %in% c(input$la_choice_tab_1, "Scotland") & 
               LongName %in% c(input$la_choice_tab_1, 
                               "Scotland", 
                               selected_small_area_tab_1()
                               )
             )
    # The area names need to be stored as a factor so that the order of the areas can be set
    # If this is not done the areas will be ordered alphabetically and the colours will be out of order
    across_data$LongName <- factor(across_data$LongName, 
                                   levels = c(selected_small_area_tab_1(), 
                                              input$la_choice_tab_1, 
                                              "Scotland"),
                                   ordered = TRUE
                                   )
    return(across_data)
  })
  
  #reactive will render ONLY when user has clicked 'submit selections'
  render_across_areas_plot_tab_1 <- eventReactive(
    list(input$submit_tab_1, input$la_map_tab_1_shape_click),{
    #do not run unless all input present
    req(iv$is_valid())
    
    plot <- create_line_plot(dataset = across_areas_data_tab_1(), 
                             council_selection = input$la_choice_tab_1, 
                             small_area_selection = selected_small_area_tab_1(), 
                             measure_selection = "Total Population Index",
                             graph_type = "Across Areas"
    )
  }
  )
  
  # Run create_line_plot - outputID = across_areas_plot_tab_1
  output$across_areas_plot_tab_1 <- renderPlotly({
   render_across_areas_plot_tab_1()
  })
  
  # Filter data for within areas graph - variable name = within_areas_data_tab_1
  within_areas_data_tab_1 <- reactive({
   data <- total_population_index_data() %>%
     filter(Council.Name == input$la_choice_tab_1 & Level == "Small Area")
   council_small_areas <- unique(data$LongName)
   # The area names need to be stored as a factor so that the order of the areas can be set
   # If this is not done the areas will be ordered alphabetically and the colours will be out of order
   area_factors <- c(selected_small_area_tab_1(), 
                      council_small_areas[!council_small_areas == selected_small_area_tab_1()])
   data$LongName <- factor(data$LongName, levels = area_factors, ordered = TRUE)
   return(data)
   })

  # Run create_line_plot - outputID = within_areas_plot_tab_1
  # will render only when 'Submit Selections' is clicked and no empty selections
  render_within_areas_plot_tab_1 <- eventReactive(list(input$submit_tab_1, input$la_map_tab_1_shape_click), {
    #do not run unless all input present
    req(iv$is_valid())
    plot <- create_line_plot(dataset = within_areas_data_tab_1(), 
                             council_selection = input$la_choice_tab_1, 
                             small_area_selection = selected_small_area_tab_1(), 
                             measure_selection = "Total Population Index",
                             graph_type = "Within Council Areas"
    )
  })
  
  output$within_areas_plot_tab_1 <- renderPlotly({
    render_within_areas_plot_tab_1()
  })
  
  
# Code for Similar Areas Tab (Tab 2) ---------------------------------------------

 }
