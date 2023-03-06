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


#Global variables -----------
  #the following 'Global' reactive variables store the values which should be consistent across both tabs
  # these will be updated by a submit button event on either tab
  selected_la <- reactiveVal()
  selected_year <- reactiveVal()
  selected_small_area <- reactiveVal() #this var is also updated by map clicks in either tab

  

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
  
# Tab 1: Map Data ---------------
  # Create reactive data for map - variable name = map_data_tab_1
  map_data_tab_1 <- reactive({
    # Filter this data based on council and year
    council_map_data <- pop_index_data() %>%
      filter(Council.Name == selected_la(),
             Level == "Small Area",
             Year == selected_year(),
             Measure == "Total.Population") %>%
      ungroup()
    
    # Filter shape file to selected council before combining with data
    filtered_shape <- filter(shape_data, Council == selected_la())
    # Combine map data with shape file 
    combined_data <- left_join(filtered_shape, 
                               council_map_data,
                               by = c("SubCouncil" = "LongName")
                               )
    return(combined_data)
  })
  

  
# Tab 1: Create Map LA Output ---------------
  render_la_map_tab_1 <- eventReactive({
    # Will render the map whenever these inputs are changed
    input$submit_tab_1
    input$submit_tab_2
    input$la_choice_tab_2
    input$year_choice_tab_2
    },
    {
    # Do not run unless all input present
    req(iv_tab_1$is_valid())

    # Call reactive map data
    map_data_tab_1 <- map_data_tab_1()

    # Store selected age then construct label if range selected ("min-max")
    selected_age_tab_1 <- selected_age_tab_1()
    age_label <- if (length(selected_age_tab_1) > 1) {
      paste(first(selected_age_tab_1), "-", last(selected_age_tab_1))
    } else {
      selected_age_tab_1
    }

    # Store selected gender
    selected_gender_tab_1 <- selected_gender_tab_1()

    #create leaflet output for tab 1
    map <- create_map(map_data = map_data_tab_1,
               council = selected_la(),
               year =selected_year(),
               tab_num = 2,
               age_label = age_label,
               gender = selected_gender_tab_1)
    
    update_highlighted_polygon(proxy_tab_1, selected_small_area())
    

    return(map)

    })

  # RenderLeaflet for council level map - output name = la_map_tab_1
  output$la_map_tab_1 <- renderLeaflet({
    render_la_map_tab_1()
  })
  
  #map proxy which will be updated to reflect selected small area;
    #when small are clicked or submit clicked, update proxy with update_highlighted_polygon()
  proxy_tab_1 <- leafletProxy("la_map_tab_1")
  

  
# Tab 1: Across Areas Plot Data-----------
  
  # Filter data for across areas graph - variable name = across_areas_data_tab_1
  across_areas_data_tab_1 <- reactive({
    # use indexed data then filter to selected Council and small area
    across_data <- total_population_index_data() %>%
      filter(Council.Name %in% c(selected_la(), "Scotland") & 
               LongName %in% c(selected_la(), 
                               "Scotland", 
                               selected_small_area()
                               )
             )
    # The area names need to be stored as a factor so that the order of the areas can be set
    # If this is not done the areas will be ordered alphabetically and the colours will be out of order
    across_data$LongName <- factor(across_data$LongName, 
                                   levels = c(selected_small_area(), 
                                              selected_la(), 
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
         input$submit_tab_2 
         ), {
    # Do not run unless all input present
    req(iv_tab_1$is_valid())
    
    plot <- create_line_plot(dataset = across_areas_data_tab_1(), 
                             council_selection = selected_la(), 
                             small_area_selection = selected_small_area(), 
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
     filter(Council.Name == selected_la() & Level == "Small Area")
   council_small_areas <- unique(data$LongName)
   # The area names need to be stored as a factor so that the order of the areas can be set
   # If this is not done the areas will be ordered alphabetically and the colours will be out of order
   area_factors <- c(selected_small_area(), 
                     council_small_areas[!council_small_areas == selected_small_area()]
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
         input$submit_tab_2
         ), {
    # Do not run unless all input present
    req(iv_tab_1$is_valid())
    plot <- create_line_plot(dataset = within_areas_data_tab_1(), 
                             council_selection = selected_la(), 
                             small_area_selection = selected_small_area(), 
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
         input$submit_tab_1
         ), {
           req(iv_tab_1$is_valid())
           HTML(paste0("Showing projected population change for <b>",
                       selected_gender_tab_1(),
                       "</b> aged <b>",
                       input$age_choice_tab_1[1],
                       "-",
                       input$age_choice_tab_1[2],
                       "</b> for <b>",
                       selected_small_area(),
                       "</b>, for <b>",
                       selected_la(), 
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
         input$submit_tab_2
         ), {
           req(iv_tab_1$is_valid())
           paste0("Showing projected population change for <b>",
                  selected_gender_tab_1(),
                  "</b> aged <b>",
                  input$age_choice_tab_1[1],
                  "-",
                  input$age_choice_tab_1[2],
                  "</b> in <b>",
                  selected_small_area(),
                  "</b> compared to other small areas in <b>",
                  selected_la(),
                  "</b>. <br>Click on the map or click on sub-council areas in the legend to explore the data.<br>   "
                  )
           }
  )
  
  # Plot text will not render until requirements within render_within_la_text() are met
  output$within_la_text <- renderText({
    render_within_la_text()
    })
  
# Observe Events: validators -------------------------------------------
  
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
  
  # Observe Events: record/update global selections -------------------------------------------
  
  #on submit_tab_1 click, update global variables and outputs on both tabs to reflect tab 1 selections
  observeEvent(input$submit_tab_1, {
    #update 'global' values for LA, year and small area
    selected_la(input$la_choice_tab_1)
    selected_year(input$year_choice_tab_1)
    small_area_options <- small_area_lookup %>%
      filter(Council.Name == input$la_choice_tab_1) %>%
      pull(LongName)
    default_area <- small_area_options[1]
    selected_small_area(default_area)
    #update tab 2 selections to match tab 1
    updateSelectizeInput(inputId = "year_choice_tab_2",
                         selected = selected_year())
    updateSelectizeInput(inputId = "la_choice_tab_2",
                         selected = selected_la())
    #update highlighted polygon on both tabs
    update_highlighted_polygon(proxy = proxy_tab_1, selected_small_area())
    update_highlighted_polygon(proxy = proxy_tab_2, selected_small_area())
    
    
  })
  
  #on submit_tab_2 click, update global variables and outputs on both tabs to reflect tab 2 selections
  observeEvent(input$submit_tab_2, {
    #update 'global' values for LA, year and small area
    selected_la(input$la_choice_tab_2)
    selected_year(input$year_choice_tab_2)
    small_area_options <- small_area_lookup %>%
      filter(Council.Name == input$la_choice_tab_2) %>%
      pull(LongName)
    default_area <- small_area_options[1]
    selected_small_area(default_area)
    #update tab 1 selections to match tab 2
    updateSelectizeInput(inputId = "year_choice_tab_1",
                         selected = selected_year())
    updateSelectizeInput(inputId = "la_choice_tab_1",
                         selected = selected_la())
    #update highlighted polygon on both tabs
    update_highlighted_polygon(proxy = proxy_tab_1, selected_small_area())
    update_highlighted_polygon(proxy = proxy_tab_2, selected_small_area())
  })
  
  # Observe Events: trigger button bounce -------------------------------------------
#The following observe events react to user input and cause the submit button 
  #to bounce to prompt the user to click submit and update the app. 
  #Measures have been taken to PREVENT button bounce in the cases where an input is
  #being changed programmatically (server-side) by an updateSelectizeInput() instead 
  #of user input.
  
  # Tab 1
  #if new input detected for age/gender, bounce to prompt user-click
  observeEvent({
    input$age_choice_tab_1
    input$gender_choice_tab_1
  },
  {
    req(iv_tab_1$is_valid())
    startAnim(session, id = "submit_tab_1", "bounce")
  })
  
  #if the selected LA in tab 1 is the SAME as the LA already selected in tab 2,
    #this indicates that input$la_choice_tab_1 event has been triggered programmatically 
    #by updateSelectizeInput() following a user-change to tab 2.
    #User has not updated tab 1. Do no trigger button bounce.
  observeEvent(input$la_choice_tab_1, {
    req(iv_tab_1$is_valid())
    if (input$la_choice_tab_2 != input$la_choice_tab_1) {
      #must be user input >> bounce
      startAnim(session, id= "submit_tab_1", "bounce")
    }
  })
  
  # Tab 2
  observeEvent({
    input$measure_choice_tab_2
  },
  {
    req(iv_tab_2$is_valid())
    startAnim(session, id = "submit_tab_2", "bounce")
  })
  
  observeEvent({
    input$la_choice_tab_2
  },
  {
    req(iv_tab_2$is_valid())
    if (input$la_choice_tab_1 != input$la_choice_tab_2) {
      startAnim(session, id= "submit_tab_2", "bounce")
    }
  })
  
  observeEvent(input$year_choice_tab_1, {
    req(iv_tab_1$is_valid())
    if (input$year_choice_tab_1 != input$year_choice_tab_2) {
      startAnim(session, id= "submit_tab_1", "bounce")
    }
  })
  
  observeEvent(input$year_choice_tab_2,{
    req(iv_tab_1$is_valid())
    if (input$year_choice_tab_1 != input$year_choice_tab_2) {
      startAnim(session, id= "submit_tab_2", "bounce")
    }
  })
  
  #Observe Events: map shape clicks--------------------
  
  # Create observe event to update selected small areas when maps are clicked
  # Tab 1 click
  observeEvent(input$la_map_tab_1_shape_click, {
    event <- input$la_map_tab_1_shape_click
    selected_small_area(event$id)
    update_highlighted_polygon(proxy = proxy_tab_1, selected_small_area())
    update_highlighted_polygon(proxy = proxy_tab_2, selected_small_area())
  })
  # Tab 2 click
  observeEvent(input$la_map_tab_2_shape_click, {
    event <- input$la_map_tab_2_shape_click
    selected_small_area(event$id)
    update_highlighted_polygon(proxy = proxy_tab_1, selected_small_area())
    update_highlighted_polygon(proxy = proxy_tab_2, selected_small_area())
  })
  
  #The selected_small_area() reactiveVal depends on a submit click or a map click to be updated.
  # This presents problems on initial page-load when LA is absent, because selected_small_area is null which throws warnings server-side.
  #The following code runs once when LA is selected the first time to initialise the selected_small_area variable.
  
  #tab 1
  observeEvent({
    input$la_choice_tab_1
  },
  {
    req(input$la_choice_tab_1 != "")
    small_area_options <- small_area_lookup %>%
      filter(Council.Name == input$la_choice_tab_1) %>%
      pull(LongName)
    default_area <- small_area_options[1]
    selected_small_area(default_area)
    #print(selected_small_area())
  }, ignoreInit = TRUE, once = TRUE)

  
  #tab 1
  observeEvent({
    input$la_choice_tab_2
  },
  {
    req(input$la_choice_tab_2 != "")
    small_area_options <- small_area_lookup %>%
      filter(Council.Name == input$la_choice_tab_2) %>%
      pull(LongName)
    default_area <- small_area_options[1]
    selected_small_area(default_area)
    #(selected_small_area())
  }, ignoreInit = TRUE, once = TRUE)
  
  
# Tab 2: Map Data ---------------
  # Create reactive data for map - variable name = map_data_tab_2
  map_data_tab_2 <- reactive({
    # Filter this data based on council, measure and year
    council_map_data <- measures_data %>%
      filter(Council.Name == selected_la(),
             Year == selected_year(),
             Measure == input$measure_choice_tab_2,
             Level == "Small Area") %>%
      ungroup()
    
    # Filter shape file to selected council before combining with data
    filtered_shape <- filter(shape_data, Council == selected_la())
    # Combine map data with shape file 
    combined_data <- left_join(filtered_shape, 
                               council_map_data,
                               by = c("SubCouncil" = "LongName")
    )
  })
  
# Tab 2: Create Map LA Output ---------------
  render_la_map_tab_2 <- eventReactive({
    input$submit_tab_2
    input$submit_tab_1
    input$la_choice_tab_1
    input$year_choice_tab_1
  },
  {
    # Do not run unless all input present
    req(iv_tab_2$is_valid())
    # Call reactive map data
    map_data_tab_2 <- map_data_tab_2()
    
    map <- create_map(map_data_tab_2, selected_la(), selected_year(), 2)
    
    update_highlighted_polygon(proxy_tab_2, selected_small_area())

    
    return(map)

  })
  
  # RenderLeaflet for council level map - output name = la_map_tab_1
  output$la_map_tab_2 <- renderLeaflet({
    render_la_map_tab_2()
  })

  proxy_tab_2 <- leafletProxy("la_map_tab_2")

  
# Tab2: Within Areas Plot Data-------------------------------------------
  
  # Filters measure_data by selected council and measure. Is updated when either changes.
  # Additionally changes factor levels so that the selected council is first followed by the rest
  # alphabetically. This allows the selected council to have a different line colour - controlled
  # by create_line_graph function.
  measures_data_tab_2 <- reactive({
    measures_data <- measures_data  %>%
      filter(Council.Name == selected_la() &
               Measure == input$measure_choice_tab_2 &
               Level == "Small Area"
             )
    council_small_areas <- unique(measures_data$LongName)
    area_factors <- c(selected_small_area(), 
                      council_small_areas[!council_small_areas == selected_small_area()]
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
                                    council_selection = selected_la(), 
                                    small_area_selection = selected_small_area(), 
                                    measure_selection = input$measure_choice_tab_2,
                                    graph_type = "Within Areas"
                                    )
           }
    )
  
  output$within_areas_plot_tab_2 <- renderPlotly({
    render_within_areas_plot_tab_2()
  })
  
# Tab 2: Annotation for plot ---------------------------------
  
  # Text to describe selected measure
  measure_info <- reactive({
    text <- if (input$measure_choice_tab_2 == "Net Migration") {
      "<b>Net Migration</b> = people moving in minus people moving away."
    } else {
      if (input$measure_choice_tab_2 == "Natural Change") {
        "<b>Natural Change</b> = births minus deaths"
      } else {
        if (input$measure_choice_tab_2 == "Sex Ratio") {
          "<b>Sex Ratio</b> = proportion of males to females"
        } else {
          if (input$measure_choice_tab_2 == "Dependency Ratio") {
            "<b>Dependency Ratio</b> = population aged 0-15 & 65+ as a proportion of population aged 16-64"
          } else {
            if (input$measure_choice_tab_2 == "Life Expectancy - Persons") {
              "<b>Life Expectancy - Persons</b> = expectation of life as an average across males and females"
            } else {
              return()
            }
          }
        }
      }
    }
    text
  })
  
  # Text to accompany/annotate the Within Council Areas plot
  render_within_la_text_tab_2 <- eventReactive(
    # Will render the text whenever these inputs are changed
    list(input$submit_tab_2, 
         input$la_map_tab_1_shape_click,
         input$la_map_tab_2_shape_click,
         input$la_choice_tab_1
    ), {
      req(iv_tab_2$is_valid())
      paste0("Showing projected change in <b>",
             input$measure_choice_tab_2,
             "</b> in <b>",
             selected_small_area(),
             "</b> compared to other small areas in <b>",
             selected_la(),
             "</b>.<br>",
             measure_info(),
             "<br>Click on the map or click on sub-council areas in the legend to explore the data.<br>   "
      )
    }
  )
  
  # Plot text will not render until requirements within render_within_la_text_tab_2() are met
  output$within_la_text_tab_2 <- renderText({
    render_within_la_text_tab_2()
  })
  
  

# Tab 3 - Data filter  ---------------------------------------------
  
  dl_measures_data <- reactive({
    if (input$measure_choice_tab_3 != "Detailed Population Data") {
      dta <- filter(measures_data, 
                    Council.Name %in% input$la_choice_tab_3 & Measure == input$measure_choice_tab_3
                    )
      # Pivot_wider
      dta$Value <- round(dta$Value, 2)
      dta <- dta %>% 
        select(Council.Name, LongName, Year, Value) %>% 
        # Remove newlines from long sub-council area names
        mutate(LongName = stringr::str_replace_all(LongName, "\n", " ")) %>%
        pivot_wider(names_from = Year, values_from = Value) %>%
        dplyr::rename(Council = Council.Name, "Sub-Council Area" = LongName)
      } else {
        # Store selected age range
        age_range <- c(input$age_choice_tab_3[1]:input$age_choice_tab_3[2])
        # Store selected year range
        year_range <- as.character(c(input$year_select_tab3[1]:input$year_select_tab3[2]))
        # Filter data based on selections
        dta <- filter(projection_data, 
                      Council.Name %in% input$la_choice_tab_3 & Sex %in% input$gender_choice_tab_3 & Age %in% age_range) %>%
          mutate(Population = round(Population, 1)) %>%
          left_join(., small_area_lookup[2:3], by = "Area.Name")
        # Replace any missing long names with "Council Total"
        dta[is.na(dta$LongName), "LongName"] <- "Council Total"
        dta <- dta %>% 
          select(Council.Name, LongName, Year, Sex, Age, Population) %>%
          # Remove newlines from long sub-council area names
          mutate(LongName = stringr::str_replace_all(LongName, "\n", " ")) %>%
          # Pivot to wide data frame
          pivot_wider(names_from = Year, values_from = Population) %>%
          # Keep only selected years  
          select(Council.Name, LongName, Sex, Age, year_range) %>%
          dplyr::rename(Council = Council.Name, "Sub-Council Area" = LongName)
        }
    })
  
# Tab 3 - Data preview table ----------------------------------------------------
  
  output$preview_table_tab3 <- DT::renderDataTable({
    dl_measures_data <- dl_measures_data()
    # Format values with thousand separator
    dl_measures_data <- dl_measures_data %>% 
      mutate_if(is.numeric, ~prettyNum(., big.mark = ",", scientific = FALSE))
    })


# Tab 3 - Data Download Button -------------------------------------------------
  
  # Data to download based on selections in tab 3
  output$dl_data_tab_3 <- downloadHandler(
    filename = paste(paste0("population_dl_", Sys.Date()), ".csv", sep = ""),
    content = function(con) {
      write.csv(dl_measures_data(), con, row.names = FALSE)
      }
  )
}