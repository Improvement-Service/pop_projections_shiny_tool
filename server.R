server <- function(input, output, session) {
  
# Input Validation --------------------------------
  # initialise an InputValidator object - do for both tab 1 and tab 2
  iv_tab_1 <- InputValidator$new()
  iv_tab_2 <- InputValidator$new()
  
  # Add validation rules: council and year choice cannot be null.
  # These rules can be enforced by calling req(iv_tab_1$is_valid()) as required.
  # Do this for both tab 1 and tab 2
  iv_tab_1$add_rule("la_choice_tab_1", sv_required())
  iv_tab_1$add_rule("year_choice_tab_1", sv_required())
  
  iv_tab_2$add_rule("la_choice_tab_2", sv_required())
  iv_tab_2$add_rule("year_choice_tab_2", sv_required())
  iv_tab_2$add_rule("measure_choice_tab_2", sv_required())

# Functions ----------------------------------------------------------------------

  # Function to add total population index to data
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
                               Population.Index := lapply(.SD, function(x) round((x/x[1]) * 100, 1)), 
                               by = .(Council.Name, Level, Area.Name), 
                               .SDcols = c("Total.Population")
                               ][lookup, on =.(Area.Name = ShortName), 
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
    plot <- ggplot(data = dataset) +
      geom_line(aes(x = Year, 
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
                                 `Value`
                                 )
                    ), 
                size = 0.7
                ) +
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
  
  #Tab 1: Reactive data objects / selected variables ---------------------
  
  # Reactive expression to store selection from gender_choice_tab_1 - variable name = selected_gender_tab_1
  selected_gender_tab_1 <- reactive({
    # Length greater than 1 means both male and female are selected so should return "Persons"
    # so the data can be filtered as such
    G <- if (length(input$gender_choice_tab_1) > 1) {
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
  
  # Reactive expression to store default small area selections - variable name = selected_small_area_tab_1
  # This will be initialised when the user selects an LA and will be updated
  # either when a new LA is selected or the user clicks on the map
  selected_small_area_tab_1 <- reactiveVal()
  selected_small_area_tab_2 <- reactiveVal()
  
# Tab 1: Map Data ---------------
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
  
# Tab 1: Create Map LA Output ---------------
  render_la_map_tab_1 <- eventReactive({
    # Will render the map whenever these inputs are changed
    input$submit_tab_1
    input$la_choice_tab_2
    input$year_choice_tab_2
    }, 
    {
    # Do not run unless all input present
    req(iv_tab_1$is_valid())
    
    # Call reactive map data
    map_data_tab_1 <- map_data_tab_1()
    
    # Store selected age
    selected_age_tab_1 <- selected_age_tab_1()
    # Label of the ages included, if more than 1 age is selected is will be presented as "16-64"
    age_label <- if (length(selected_age_tab_1) > 1) { 
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
    
    default_selected_polygon <- shape_data %>% 
      filter(SubCouncil == selected_small_area_tab_1()) %>% 
      pull(geometry)
    # Create a leaflet object using small area shapefiles
    leaflet(data = map_data_tab_1, options = leafletOptions(zoomControl = FALSE)) %>%
    htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }"
                          ) %>%
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
                  label = (sprintf("<strong>%s</strong><br/>Year: %s<br/>Age: %s<br/>Gender: %s<br/>Population: %s",
                                   map_data_tab_1$SubCouncil, 
                                   map_data_tab_1$Year,
                                   age_label,
                                   selected_gender_tab_1,
                                   map_data_tab_1$Value
                                   ) %>% 
                             lapply(htmltools::HTML)
                           ),
                  # Creates a white border on the polygon where the mouse hovers
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 5, 
                                                      bringToFront = FALSE
                                                      )
                  ) %>%
      addLegend("bottomleft", 
                colors = map_colours,
                labels = c("Smallest Population", 
                           "",
                           "",
                           "",
                           "",
                           "",
                           "",
                           "Largest Population"
                           ),
                title = paste0("Population, ", input$year_choice_tab_1),
                opacity = 1
                ) %>%
      addPolylines(stroke = TRUE, 
                   weight = 3,
                   color = "orange",
                   opacity = 0.7,
                   data = default_selected_polygon, 
                   group ="highlighted_polygon"
                   )
    })

  # RenderLeaflet for council level map - output name = la_map_tab_1
  output$la_map_tab_1 <- renderLeaflet({
    render_la_map_tab_1()
  })
  
# Tab 1: Highlight selected polygon ------------
  proxy_tab_1 <- leafletProxy("la_map_tab_1")
  
  observeEvent({
    # Will change the polygon highlighted if any of these change
    input$la_map_tab_1_shape_click
    input$la_map_tab_2_shape_click
    input$la_choice_tab_2
    input$submit_tab_1
    }, 
    {
    req(iv_tab_1$is_valid())
    # Get the selected polygon and extract the label point 
    selected_polygon <- shape_data %>% 
      filter(SubCouncil == selected_small_area_tab_1()) %>% 
      pull(geometry)

    # Remove any previously highlighted polygon
    proxy_tab_1 %>% clearGroup("highlighted_polygon")
    # Add a slightly thicker red polygon on top of the selected one
    proxy_tab_1 %>% addPolylines(stroke = TRUE, 
                           weight = 3,
                           color = "orange",
                           opacity = 0.7,
                           data = selected_polygon, 
                           group = "highlighted_polygon"
                           )
    })
  
# Tab 1: Across Areas Plot Data-----------
  
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
                                              "Scotland"
                                              ),
                                   ordered = TRUE
                                   )
    return(across_data)
  })
  
# Tab 1: Create Across Areas Plot---------
  
  # Reactive will render ONLY when user has clicked 'submit selections'
  render_across_areas_plot_tab_1 <- eventReactive(
    # Will render the plot whenever these inputs are changed
    list(input$submit_tab_1, 
         input$la_map_tab_1_shape_click, 
         input$la_map_tab_2_shape_click,
         input$la_choice_tab_2 
         ), {
    # Do not run unless all input present
    req(iv_tab_1$is_valid())
    
    plot <- create_line_plot(dataset = across_areas_data_tab_1(), 
                             council_selection = input$la_choice_tab_1, 
                             small_area_selection = selected_small_area_tab_1(), 
                             measure_selection = "Total Population Index",
                             graph_type = "Across Areas"
                             )
    }
  )
  
  output$across_areas_plot_tab_1 <- renderPlotly({
   render_across_areas_plot_tab_1()
  })
  
# Tab 1: Within Areas Plot Data-----------
  # Filter data for within areas graph - variable name = within_areas_data_tab_1
  within_areas_data_tab_1 <- reactive({
   data <- total_population_index_data() %>%
     filter(Council.Name == input$la_choice_tab_1 & Level == "Small Area")
   council_small_areas <- unique(data$LongName)
   # The area names need to be stored as a factor so that the order of the areas can be set
   # If this is not done the areas will be ordered alphabetically and the colours will be out of order
   area_factors <- c(selected_small_area_tab_1(), 
                     council_small_areas[!council_small_areas == selected_small_area_tab_1()]
                     )
   data$LongName <- factor(data$LongName, levels = area_factors, ordered = TRUE)
   return(data)
   })

# Tab 1: Create Within Areas Plot----------
  # Will render only when 'Submit Selections' is clicked and no empty selections
  render_within_areas_plot_tab_1 <- eventReactive(
    # Will render the plot whenever these inputs are changed
    list(input$submit_tab_1, 
         input$la_map_tab_1_shape_click,
         input$la_map_tab_2_shape_click,
         input$la_choice_tab_2
         ), {
    # Do not run unless all input present
    req(iv_tab_1$is_valid())
    plot <- create_line_plot(dataset = within_areas_data_tab_1(), 
                             council_selection = input$la_choice_tab_1, 
                             small_area_selection = selected_small_area_tab_1(), 
                             measure_selection = "Total Population Index",
                             graph_type = "Within Council Areas"
                             )
    }
    )
  
  output$within_areas_plot_tab_1 <- renderPlotly({
    render_within_areas_plot_tab_1()
  })
  
# Tab 1: Annotations for plots -------------------
  
  # Text to accompany/annotate the 'Population Index Across Scotland' plot
  render_across_scotland_text <- eventReactive(
    # Will render the text whenever these inputs are changed
    list(input$submit_tab_1, 
         input$la_map_tab_1_shape_click, 
         input$la_map_tab_2_shape_click,
         input$la_choice_tab_2
         ), {
           req(iv_tab_1$is_valid())
           HTML(paste0("Showing projected population change for <b>",
                       selected_gender_tab_1(),
                       "</b> aged <b>",
                       input$age_choice_tab_1[1],
                       "-",
                       input$age_choice_tab_1[2],
                       "</b> for <b>",
                       selected_small_area_tab_1(),
                       "</b>, for <b>",
                       input$la_choice_tab_1, 
                       "</b>, and for Scotland as a whole.<br>   "
                       )
                )
           }
  )
  
  # Plot text will not render until requirements within render_across_scotland_text() are met
  output$across_scotland_text <- renderText({render_across_scotland_text()})
  
  # Text to accompany/annotate the 'Population Index Within Council Areas' plot
  render_within_la_text <- eventReactive(
    # Will render the text whenever these inputs are changed
    list(input$submit_tab_1, 
         input$la_map_tab_1_shape_click,
         input$la_map_tab_2_shape_click,
         input$la_choice_tab_2
         ), {
           req(iv_tab_1$is_valid())
           paste0("Showing projected population change for <b>",
                  selected_gender_tab_1(),
                  "</b> aged <b>",
                  input$age_choice_tab_1[1],
                  "-",
                  input$age_choice_tab_1[2],
                  "</b> in <b>",
                  selected_small_area_tab_1(),
                  "</b> compared to other small areas in <b>",
                  input$la_choice_tab_1,
                  "</b>. <br>Click on the map or click on sub-council areas in the legend to explore the data.<br>   "
                  )
           }
  )
  
  # Plot text will not render until requirements within render_within_la_text() are met
  output$within_la_text <- renderText({
    render_within_la_text()
    })
  
# Observe Events -------------------------------------------
  
  # 'turn on' user input validation when submit clicked for first time - 
  # missing fields, if there are any, will then show error
  # Tab 1
  observeEvent(input$submit_tab_1, {
    iv_tab_1$enable()
  })
  # Tab 2
  observeEvent(input$submit_tab_2, {
    iv_tab_2$enable()
  })
  
  # Trigger 'bouncing' submit button when input is changed
  # Tab 1
  observeEvent({
    input$la_choice_tab_1
    input$year_choice_tab_1
    input$age_choice_tab_1
    input$gender_choice_tab_1
  },
  {
    req(iv_tab_1$is_valid())
    startAnim(session, id = "submit_tab_1", "bounce")
  })
  # Tab 2
  observeEvent({
    input$la_choice_tab_2
    input$year_choice_tab_2
    input$measure_choice_tab_2
  },
  {
    req(iv_tab_2$is_valid())
    startAnim(session, id = "submit_tab_2", "bounce")
  })
  
  # When submit is clicked ensure that any update to selected LA 
  # carries between the two tabs
  # Tab 1
  observeEvent(input$submit_tab_1, {
    updateSelectizeInput(inputId = "la_choice_tab_2",
                         selected = input$la_choice_tab_1
    )
  })
  # Tab 2
  observeEvent(input$submit_tab_2, {
    updateSelectizeInput(inputId = "la_choice_tab_1",
                         selected = input$la_choice_tab_2
    )
  })
  
  # When submit is clicked ensure that any update to selected Year 
  # carries between the two tabs
  # Tab 1
  observeEvent(input$submit_tab_1, {
    updateSelectizeInput(inputId = "year_choice_tab_2",
                         selected = input$year_choice_tab_1
    )
  })
  # Tab 2
  observeEvent(input$submit_tab_2, {
    updateSelectizeInput(inputId = "year_choice_tab_1",
                         selected = input$year_choice_tab_2
    )
  })
  
  # Create observe event to update selected small areas when maps are clicked
  # Tab 1 click
  observeEvent(input$la_map_tab_1_shape_click, {
    event <- input$la_map_tab_1_shape_click
    selected_small_area_tab_1(event$id)
    selected_small_area_tab_2(event$id)
  })
  # Tab 2 click
  observeEvent(input$la_map_tab_2_shape_click, {
    event <- input$la_map_tab_2_shape_click
    selected_small_area_tab_1(event$id)
    selected_small_area_tab_2(event$id)
  })
  
  # Determine default selected small area for tab when LA 
  # is selected/changed (responsive to either tab)
  # Tab 1
  observeEvent({
    input$submit_tab_1
    input$la_choice_tab_1
    input$la_choice_tab_2
  },
  {
    req(input$la_choice_tab_1 != "")
    small_area_options <- small_area_lookup %>%
      filter(Council.Name == input$la_choice_tab_1) %>%
      pull(LongName)
    default_area <- small_area_options[1]
    selected_small_area_tab_1(default_area)
  })
  
  # Tab 2
  observeEvent({
    input$submit_tab_2
    input$la_choice_tab_1
    input$la_choice_tab_2
  },
  {
    req(input$la_choice_tab_2 != "")
    small_area_options <- small_area_lookup %>%
      filter(Council.Name == input$la_choice_tab_2) %>%
      pull(LongName)
    default_area <- small_area_options[1]
    selected_small_area_tab_2(default_area)
  })
  
# Tab 2: Code for Other Measures Tab ---------------------------------------------
 
# Tab 2: Map Data ---------------
  # Create reactive data for map - variable name = map_data_tab_2
  map_data_tab_2 <- reactive({
    # Filter this data based on council, measure and year
    council_map_data <- measures_data %>%
      filter(Council.Name == input$la_choice_tab_2,
             Year == input$year_choice_tab_2,
             Measure == input$measure_choice_tab_2,
             Level == "Small Area") %>%
      ungroup()
    
    # Filter shape file to selected council before combining with data
    filtered_shape <- filter(shape_data, Council == input$la_choice_tab_2)
    # Combine map data with shape file 
    combined_data <- left_join(filtered_shape, 
                               council_map_data,
                               by = c("SubCouncil" = "LongName")
    )
  })
  
# Tab 2: Create Map LA Output ---------------
  render_la_map_tab_2 <- eventReactive({
    input$submit_tab_2
    input$la_choice_tab_1
    input$year_choice_tab_1
  },
  {
    # Do not run unless all input present
    req(iv_tab_2$is_valid())
    # Call reactive map data
    map_data_tab_2 <- map_data_tab_2()
    
    # Set colours for the map
    map_colours <- brewer.pal(8, "Blues")
    # Assign colours to quintiles
    map_colour_quintiles <- colorBin(map_colours, map_data_tab_2$Value, n = 8)
    
    default_selected_polygon <- shape_data %>% 
      filter(SubCouncil == selected_small_area_tab_2()) %>% 
      pull(geometry)
    # Create a leaflet object using small area shapefiles
    leaflet(data = map_data_tab_2, options = leafletOptions(zoomControl = FALSE)) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }"
      ) %>%
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
                  label = (sprintf("<strong>%s</strong><br/>Year: %s<br/>%s: %s",
                                   map_data_tab_2$SubCouncil, 
                                   map_data_tab_2$Year,
                                   map_data_tab_2$Measure,
                                   map_data_tab_2$Value
                  ) %>% 
                    lapply(htmltools::HTML)
                  ),
                  # Creates a white border on the polygon where the mouse hovers
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 5, 
                                                      bringToFront = FALSE
                  )
      ) %>%
      addLegend("bottomleft", 
                colors = map_colours,
                labels = c("Smallest Value", 
                           "",
                           "",
                           "",
                           "",
                           "",
                           "",
                           "Largest Value"
                ),
                title = paste0(input$measure_choice_tab_2, 
                               ", ", 
                               input$year_choice_tab_2
                               ),
                opacity = 1
      ) %>%
      addPolylines(stroke = TRUE, 
                   weight = 3,
                   color = "orange",
                   opacity = 0.7,
                   data = default_selected_polygon, 
                   group ="highlighted_polygon"
      )
  })
  
# Tab 2: Highlight selected polygon ------------
  proxy_tab_2 <- leafletProxy("la_map_tab_2")
  
  observeEvent({
    input$la_map_tab_1_shape_click
    input$la_map_tab_2_shape_click
    input$la_choice_tab_1
    input$la_choice_tab_2
    input$submit_tab_1
    input$submit_tab_2
  }, 
  {
    req(iv_tab_2$is_valid())
    # Get the selected polygon and extract the label point 
    selected_polygon <- shape_data %>% 
      filter(SubCouncil == selected_small_area_tab_2()) %>% 
      pull(geometry)
    
    # Remove any previously highlighted polygon
    proxy_tab_2 %>% clearGroup("highlighted_polygon")
    # Add a slightly thicker red polygon on top of the selected one
    proxy_tab_2 %>% addPolylines(stroke = TRUE, 
                           weight = 3,
                           color = "orange",
                           opacity = 0.7,
                           data = selected_polygon, 
                           group = "highlighted_polygon"
    )
  })
  
  
  
  # RenderLeaflet for council level map - output name = la_map_tab_1
  output$la_map_tab_2 <- renderLeaflet({
    render_la_map_tab_2()
  })

# Tab2: Within Areas Plot Data-------------------------------------------
  
  # Filters measure_data by selected council and measure. Is updated when either changes.
  # Additionally changes factor levels so that the selected council is first followed by the rest
  # alphabetically. This allows the selected council to have a different line colour - controlled
  # by create_line_graph function.
  measures_data_tab_2 <- reactive({
    measures_data <- measures_data  %>%
      filter(Council.Name == input$la_choice_tab_2 &
               Measure == input$measure_choice_tab_2 &
               Level == "Small Area"
             )
    council_small_areas <- unique(measures_data$LongName)
    area_factors <- c(selected_small_area_tab_2(), 
                      council_small_areas[!council_small_areas == selected_small_area_tab_2()]
                      )
    measures_data$LongName <- factor(measures_data$LongName, 
                                     levels = area_factors, 
                                     ordered = TRUE
                                     )
    return(measures_data)
  })
  
# Tab 2: Create Within Areas Plot-------------------------------------------------  
  # Once council and input measure is given, render line plot using reactive 
  # data object: measures_data_tab_2()
  
  # Will render only when 'Submit Selections' is clicked and no empty selections
  render_within_areas_plot_tab_2 <- eventReactive(
    # Will render the plot whenever these inputs are changed
    list(input$submit_tab_2, 
         input$la_map_tab_1_shape_click,
         input$la_map_tab_2_shape_click,
         input$la_choice_tab_1
         ), {
           # Do not run unless all input present
           req(iv_tab_2$is_valid())
           
           plot <- create_line_plot(dataset = measures_data_tab_2(), 
                                    council_selection = input$la_choice_tab_2, 
                                    small_area_selection = selected_small_area_tab_2(), 
                                    measure_selection = input$measure_choice_tab_2,
                                    graph_type = "Within Areas"
                                    )
           }
    )
  
  output$within_areas_plot_tab_2 <- renderPlotly({
    render_within_areas_plot_tab_2()
  })
  
# Download data button ---------------------------------------------
  #when Download Data button (Ui title) is clicked, pop up appears and gives two download options
  observeEvent(input$download_pop_up, {
    showModal(modalDialog(
      title = "Download",
      paste0("To download ", input$measure_choice_tab_2, " data for ", input$la_choice_tab_2, 
      " (currently shown on the graph), click 'Download Selected Data'. Or for all measures for all councils, select 'Download All Data'."),
      footer = tagList(
        downloadButton(outputId = "download_selected_data", "Download Selected Data"),
        downloadButton(outputId = "download_all_data", "Download All Data"),
        modalButton("Cancel")
      ),
      easyClose = TRUE
    ))
  })
  
  #code to handle downloading selected data
  output$download_selected_data <- downloadHandler(
    filename = paste0(input$measure_choice_tab_2 ," data - ", input$la_choice_tab_2, ".csv"),
    content = function(con){
      on.exit(removeModal())
      data.table::fwrite(measures_data_tab_2(), con)
    }
  )
  
  #handles download of entire dataset
  output$download_all_data <- downloadHandler(
    filename = paste0("population projections data.csv"),
    content = function(con){
      on.exit(removeModal())
      data.table::fwrite(measures_data, con)
    }
  )
  
}



