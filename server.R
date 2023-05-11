server <- function(input, output, session) {
  
# Tab links --------------------------------------
  #Set up links to tabs

  observeEvent(input$pop_size_link, {
    updateTabsetPanel(session, "main_tabs", selected = "population_size")
  })
  
  observeEvent(input$other_measures_link, {
    updateTabItems(session, "main_tabs", selected = "other_measures")
  })
  
  observeEvent(input$data_tab_link, {
    updateTabItems(session, "main_tabs", selected = "data_download")
  })
  
  observeEvent(input$feedback_tab_link, {
    updateTabItems(session, "main_tabs", selected = "feedback_tab")
  })
  
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

# Global variables -----------
  
  # The following 'Global' reactive variables store values which should be consistent across both tabs
  # these will be updated by a submit button event on either tab
  selected_la <- reactiveVal()
  selected_year <- reactiveVal(2018)
  selected_small_area <- reactiveVal("") #this var is also updated by map clicks and plot trace clicks in either tab
  
  #this variable indicates whether user has made first small area selection
  initial_polygon_click <- reactiveVal(FALSE)
  
# Tab 1: Reactive data objects / selected variables ---------------------
  
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
    first_age <- if_else(input$age_choice_tab_1[1] == "90+", 90, as.numeric(input$age_choice_tab_1[1]))
    last_age <- if_else(input$age_choice_tab_1[2] == "90+", 90, as.numeric(input$age_choice_tab_1[2]))
    full_range <- c(first_age:last_age)
    
    return(full_range)
  })
  
  # Generate label string showing selected single age or range for pop-ups and map annotation text
  selected_age_label <- reactive({
    if (length(selected_age_tab_1()) > 1) {
      first <- if_else(first(selected_age_tab_1()) == 90, "90+", as.character(first(selected_age_tab_1())))
      last <- if_else(last(selected_age_tab_1()) == 90, "90+", as.character(last(selected_age_tab_1())))
      return(paste(first, "-", last))
    } else {
      label <- if_else(selected_age_tab_1() == 90, "90+", as.character(selected_age_tab_1()))
      return(label)
    }
    #return(age_label)
  })
  
  # Calculate population index based on user input for gender and age
  pop_index_data <- reactive({
    data <- projection_data %>% 
      add_pop_index(gender_selection = selected_gender_tab_1(),
                    age_selection = selected_age_tab_1()
                    ) 
    return(data)
  })
  
  # Filter out population count data
  total_population_index_data <- reactive({
    filtered_data <- pop_index_data() %>%
      filter(Measure == "Population.Index")# %>%
      #mutate(Value = Value -100)
    return(filtered_data)
  })
  
  population_map_data <- reactive({
    
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
                               by = c("SubCouncil" = "LongName", "Council" = "Council.Name")
    ) %>% mutate(population_density = Value/detailed_area,
                 alt_layer_id = paste0("+",SubCouncil))
    
    return(combined_data)
    
  })
  

# Tab 1: Create Map LA Output ---------------
  
  # RenderLeaflet for council level map - output name = la_map_tab_1
  output$la_map_tab_1 <- renderLeaflet({
    #map reacts to this reactive data object which itself reacts to submit clicks and year changes
    req(isTruthy(!(selected_la() %in% c('', NULL))))
    
    create_map(map_data = population_map_data(),
               tab_num = 1,
               default_area = isolate(selected_small_area()),
               #if the param below is TRUE then map will be rendered with selected polygon highlighted
               initial_polygon_click = initial_polygon_click(),
               # isolate other reactives (we don't want to re-render map when these change)default_area = isolate(selected_small_area()),
               age_label = isolate(selected_age_label()),
               gender = isolate(selected_gender_tab_1())
               )
    }) %>% bindEvent(c(selected_la(), selected_year()))
  
  # create map proxy which will be updated to reflect selected small area without refreshing the whole output;
  # when small area is clicked or submit clicked, update proxy aesthetics with update_highlighted_polygon()
  proxy_tab_1 <- leafletProxy("la_map_tab_1")

# Tab 1: tabsetPanel UI output --------------------------------

  output$help1 <- renderUI({
    if (initial_polygon_click() == FALSE) {
      return(div())
      } else {
    actionButton(
      inputId = "help_tab1",
      label = NULL,
      icon = icon("question")
    )
      }
  })
  
  output$help2 <- renderUI({
    if (initial_polygon_click() == FALSE) {
      return(div())
    } else {
      actionButton(
        inputId = "help_tab2",
        label = "",
        icon = icon("question")
      )
    }
  })

    
  #only render the tabsetPanel UI once the user has selected a polygon (causing initial_polygon_click() to 
  #be updated to TRUE)
  output$population_graph_tabset <- renderUI ({
    if (initial_polygon_click() == FALSE) {
      return(div("Click on a sub-council area on the map to begin exploring projected change for your selected population."))
    } else {
      div (
      tabsetPanel(id = "tab_1_plots",
                  type = "tabs",
                  tabPanel(
                    title = "Population Index Across Scotland",
                    value = "across_areas",
                    plotlyOutput("across_areas_plot_tab_1", 
                                 height = "340px") %>% 
                      withSpinner(type = 6),
                    
                    span(htmlOutput("across_scotland_text"), 
                         style = "color:#526470; font-size = 12px"
                         )
                    ), #end of across areas tab panel
                  
                  tabPanel(
                    title = "Population Index Within Council Areas", 
                    value = "within_areas",
                    
                    plotlyOutput("within_areas_plot_tab_1", 
                                        height = "340px") %>% 
                      withSpinner(type = 6), 
                    
                    span(htmlOutput("within_la_text"), 
                         style = "color:#526470; font-size = 12px"
                           ) #end of span
                  ) #end of within_areas tabPanel
              ) ) # End of tabsetPanel 
    } #end of else
  }) #end of tabsetPanel renderUI
  
   
# Tab 1: Across Areas Plot Data-----------
  
  # Filter data for across areas graph - variable name = across_areas_data_tab_1
  across_areas_data_tab_1 <- eventReactive(list(selected_la(),
                                                initial_polygon_click() #respond to initial polygon click (single event)
                                                ), {
    # Use indexed data then filter to selected Council and small area
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
                                   levels = c(selected_la(),
                                              "Scotland",
                                              selected_small_area()
                                              ),
                                   ordered = TRUE
                                   )
    return(across_data)
  })
  
# Tab 1: Create Across Areas Plot---------
  
  # The reactive parameters passed to the create_line_plot function are all updated only when
  # map polygon or submit buttons are clicked, this means this output is only refreshed 
  # in response to these events too
  output$across_areas_plot_tab_1 <- renderPlotly({
    req(initial_polygon_click())
    
    isolate({
      create_line_plot(dataset = across_areas_data_tab_1(), 
                       small_area_selection = isolate(selected_small_area()), 
                       graph_type = "Across Areas",
                       tab = 1,
                       yrange = c(
                         min(min(within_areas_data_tab_1()$Value),min(across_areas_data_tab_1()$Value)),
                         max(max(within_areas_data_tab_1()$Value),max(across_areas_data_tab_1()$Value))
                       )
                       )
    })
    
    }) %>% bindEvent(c(selected_la(),
                       initial_polygon_click(),
                       across_areas_data_tab_1()))
  
  across_areas_proxy <- plotlyProxy("across_areas_plot_tab_1", session)
    
# Tab 1: Within Areas Plot Data-----------
  # Filter data for selections when submit buttons or map polygons are clicked in either tab
  within_areas_data_tab_1 <- eventReactive(list(selected_la(),
                                                initial_polygon_click()),{
   data <- total_population_index_data() %>%
     filter(Council.Name == selected_la() & Level == "Small Area")

   #ensure Longname is a factor with levels in alphabetical order (for order of traces on graph)
   #these levels are used to lookup area and change colours on plotlyProxy when trace is clicked by user
   council_small_areas <- sort(unique(data$LongName))
   data$LongName <- factor(data$LongName, levels = council_small_areas, ordered = TRUE)
   return(data)
   })

  #create a proxy which will show aesthetic updates when user clicks line trace
    within_areas_1_proxy <- eventReactive (list(input$submit_tab_1,
                                                input$submit_tab_2), {
      plotlyProxy("within_areas_plot_tab_1")
    })
    
    # Tab 1: Create Within Areas Plot----------
    output$within_areas_plot_tab_1 <- renderPlotly({
      req(initial_polygon_click())
      
      isolate({
        create_line_plot(dataset = within_areas_data_tab_1(), 
                         small_area_selection = selected_small_area(), 
                         graph_type = "Within Council Areas",
                         tab = 1
        )
      })
      
    }) %>%
      bindEvent(within_areas_data_tab_1()) #provoke update when submits are clicked
    
    
    #create a proxy which will show aesthetic updates when user clicks line trace
    within_areas_1_proxy <- eventReactive (list(input$submit_tab_1,
                                                input$submit_tab_2), {
                                                  plotlyProxy("within_areas_plot_tab_1")
                                                })
    

  
# Tab 1: Annotations for plots -------------------
  
  # Plot text will not render until requirements within render_across_scotland_text() are met
  output$across_scotland_text <- renderText({
    #render_across_scotland_text()
    req(initial_polygon_click())
    isolate({
      HTML(paste0("Showing projected population change for <b>",
                  isolate(selected_gender_tab_1()),
                  "</b> aged <b>",
                  isolate(selected_age_label()),
                  "</b> for <b>",
                  selected_small_area(),
                  "</b>, for <b>",
                  isolate(selected_la()), 
                  "</b>, and for Scotland as a whole.<br>
                <br><b>Population Index</b> = the projected population size as a percentage of 
                  the population size in 2018. For example, a population index of 96 by 2030 means that the area's population 
                  is projected to be 96% of its size in 2018."
      ))
    })
    }) %>% bindEvent(selected_small_area(), selected_la())
  

  # Plot text will not render until requirements within render_within_la_text() are met
  output$within_la_text <- renderText({
    #render_within_la_text()
    req(initial_polygon_click())
    isolate({
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
             ".</b><br>
                  <br><b>Population Index</b> = the projected population size as a percentage of 
                  the population size in 2018. For example, a population index of 96 by 2030 means that the area's population 
                  is projected to be 96% of its size in 2018."
      )
    })
    }) %>% bindEvent(selected_small_area(), selected_la())
  
  # Observe Events: Input Validation --------------------------------
  
  #when la is clicked for the first time (in either tab), enable submit button (in both tabs)
  observeEvent(list(input$la_choice_tab_1,
                    input$la_choice_tab_2), {
                      shinyjs::enable("submit_tab_1")
                      shinyjs::enable("submit_tab_2")
                    }, ignoreInit = TRUE, once = TRUE)
  
  #when map polygon is clicked for the first time (in either tab) the initial_polygon_click() reactive
  #variable is updated to TRUE. This triggers rendering of the tab 1 tabsetPanel outputs and also ensures that subsequent 
  #submit clicks will cause a default polygon to be selected for selected council
  observeEvent(list(input$la_map_tab_1_shape_click,
                    input$la_map_tab_2_shape_click), {
                      initial_polygon_click(TRUE)
                    }, ignoreInit = TRUE, once = TRUE) #after this event has happened once, stop observing
  


# Observe Events: record/update global selections -------------------------------------------
  
  # On submit_tab_1 click, update global variables and outputs on both tabs to reflect tab 1 selections
  #sequence is important so that tabs don't diverge from another

  observeEvent(input$submit_tab_1, {
    # Update 'global' value for LA, and dropdown in tab 2
    
    #first: reset selected area (this means that outputs which react to changes to this
    #variable will be triggered when submit is clicked even if user has not altered LA)
    selected_la('')
    #now update globally referenced LA to be the selected LA on the tab where submit is clicked
    selected_la(input$la_choice_tab_1)
    updateSelectizeInput(inputId = "la_choice_tab_2",
                         selected = selected_la())
    
    # Determine default small area and update highlighted polygon on both tabs if initial polygon click has been made,
    #otherwise no highlighted polygon will be shown
    if (initial_polygon_click() == TRUE) {
      
      #get small areas within selected council
      small_area_options <- small_area_lookup %>%
        filter(Council.Name == input$la_choice_tab_1) %>%
        pull(LongName)
      
      default_area <- small_area_options[1] #extract first alphabetically
      selected_small_area(default_area) #update global variable
      
      update_highlighted_polygon(proxy = proxy_tab_1, selected_small_area(),selected_la())
      update_highlighted_polygon(proxy = proxy_tab_2, selected_small_area(),selected_la())
    }
    
  })
  
  # On submit_tab_2 click, update global variables and outputs on both tabs to reflect tab 2 selections
  observeEvent(input$submit_tab_2, {
    
    #first: reset selected area (this means that outputs which react to changes to this
    #variable will be triggered when submit is clicked even if user has not altered LA)
    selected_la('')
    #now update globally referenced LA to be the selected LA on the tab where submit is clicked
    selected_la(input$la_choice_tab_2)
    updateSelectizeInput(inputId = "la_choice_tab_1",
                         selected = selected_la())
    
    #on initial submit click there should be no highlighted polygon. 
    #once user has clicked a polygon for the first time, submit will cause a polygon
    #to be highlighted with default small area for a council
    if (initial_polygon_click() == TRUE) {
      #update highlighted polygon on both tabs
      small_area_options <- small_area_lookup %>%
        filter(Council.Name == input$la_choice_tab_2) %>%
        pull(LongName)
      default_area <- small_area_options[1]
      selected_small_area(default_area)
      
      update_highlighted_polygon(proxy = proxy_tab_1, selected_small_area(),selected_la())
      update_highlighted_polygon(proxy = proxy_tab_2, selected_small_area(),selected_la())
    }
  })
  
  #keep year consistent across both tabs
  observeEvent(input$year_choice_tab_1, {
    selected_year(input$year_choice_tab_1)
    updateSelectizeInput(inputId = "year_choice_tab_2", selected = selected_year())
  }, ignoreInit = TRUE)
  
  observeEvent(input$year_choice_tab_2, {
    selected_year(input$year_choice_tab_2)
    updateSelectizeInput(inputId = "year_choice_tab_1", selected = selected_year())
  }, ignoreInit = TRUE)

#Observe Events: line plot clicks -------------------------------

  observeEvent(event_data("plotly_click", source = "within_areas_1"), {
    #get event data
    plot_click_data <- (event_data("plotly_click", source = "within_areas_1"))
    #update selected small area by finding which trace was clicked (curveNumber gives index of small area in LongName factor levels)
    selected_small_area(levels(within_areas_data_tab_1()$LongName)[plot_click_data$curveNumber + 1])
    #update polygon highlighting on plots
    update_highlighted_polygon(proxy = proxy_tab_1, selected_small_area(),selected_la())
    update_highlighted_polygon(proxy = proxy_tab_2, selected_small_area(),selected_la())
    restyle_obj <- get_plot_trace_aesthetics(plot_click_data$curveNumber + 1)

    plotlyProxyInvoke(within_areas_1_proxy(), "restyle", list(opacity=restyle_obj$opacity, line.color=restyle_obj$colours),within_areas_1_proxy()$curveNumber)
    plotlyProxyInvoke(within_areas_2_proxy, "restyle", list(opacity=restyle_obj$opacity, line.color=restyle_obj$colours),within_areas_2_proxy$curveNumber)

    #update line trace for selected_small_area on across areas plot proxy (tab 1)
    update_across_area_proxy(plotlyProxy("across_areas_plot_tab_1", session),
                             selected_small_area(), 
                             within_areas_data_tab_1())
   
  })
  
  observeEvent(event_data("plotly_click", source = "within_areas_2"), {
    #get event data
    plot_click_data <- (event_data("plotly_click", source = "within_areas_2"))
    #update selected small area by finding which trace was clicked (curveNumber gives index of small area in data levels)
    selected_small_area(levels(measures_data_tab_2()$LongName)[plot_click_data$curveNumber + 1])
    #update polygon highlighting on plots
    update_highlighted_polygon(proxy = proxy_tab_1, selected_small_area(),selected_la())
    update_highlighted_polygon(proxy = proxy_tab_2, selected_small_area(),selected_la())
    
    #obtain a list of colours and opacity values to pass to restyle proxy
    restyle_obj <- get_plot_trace_aesthetics(plot_click_data$curveNumber + 1)
    #update opacity/colour of traces on within_areas plot on both tabs
    plotlyProxyInvoke(within_areas_1_proxy(), "restyle", list(opacity=restyle_obj$opacity, line.color=restyle_obj$colours),within_areas_1_proxy()$curveNumber)
    plotlyProxyInvoke(within_areas_2_proxy, "restyle", list(opacity=restyle_obj$opacity, line.color=restyle_obj$colours),within_areas_2_proxy$curveNumber)
    
    #update line trace for selected_small_area on across areas plot proxy (tab 1)
    update_across_area_proxy(plotlyProxy("across_areas_plot_tab_1", session),
                             selected_small_area(), 
                             within_areas_data_tab_1())
  })
  
  
# Observe Events: trigger button bounce -------------------------------------------

  # The following observe events react to user input and cause the submit button 
  # to bounce to prompt the user to click submit and update the app. 
  # Measures have been taken to PREVENT button bounce in the cases where an input is
  # being changed programmatically (server-side) by an updateSelectizeInput() instead 
  # of user input.
  
  # Tab 1
  # If new input detected for age/gender, bounce to prompt user-click
  observeEvent({
    input$age_choice_tab_1
    input$gender_choice_tab_1
  },
  {
    startAnim(session, id = "submit_tab_1", "bounce")
  }, ignoreInit = TRUE)
  
  # If the selected LA in tab 1 is the SAME as the LA already selected in tab 2,
  # this indicates that input$la_choice_tab_1 event has been triggered programmatically 
  # by updateSelectizeInput() following a user-change to tab 2.
  # User has not updated tab 1. Do no trigger button bounce.
  observeEvent(input$la_choice_tab_1, {
    if (input$la_choice_tab_2 != input$la_choice_tab_1) {
      #must be user input >> bounce
      startAnim(session, id= "submit_tab_1", "bounce")
    }
  }, ignoreInit = TRUE)
  
  # Tab 2
  observeEvent(input$measure_choice_tab_2, {
    startAnim(session, id = "submit_tab_2", "bounce")
  }, ignoreInit = TRUE)
  
  observeEvent(input$la_choice_tab_2, {
    if (input$la_choice_tab_1 != input$la_choice_tab_2) {
      startAnim(session, id= "submit_tab_2", "bounce")
    }
  })
  
  observeEvent({
    input$age_choice_tab_3
    input$gender_choice_tab_3
    input$year_select_tab3
  },
  {
    startAnim(session, id = "apply_filters", "bounce")
  }, ignoreInit = TRUE)
# Observe Events:map layer clicks----------
  
  obs <- observeEvent(input$la_map_tab_1_groups, {
    req(initial_polygon_click())
    update_highlighted_polygon(proxy = proxy_tab_1, selected_small_area(), selected_la())
  })

    
# Observe Events: map shape clicks--------------------
  
  # Create observe event to update selected small areas when maps are clicked
  # Tab 1 click
  observeEvent(input$la_map_tab_1_shape_click, {
   
    event <- input$la_map_tab_1_shape_click
    clicked_area <- event$id
    # if statement prevents app crashing if user clicks the highlighted line instead of polygon
    # when this occurs the event$id is NULL
    if(!is.null(clicked_area)) {

      if(substr(clicked_area,1,1) == "+") {
        clicked_area <- substr(clicked_area, 2, nchar(clicked_area))
      }
    selected_small_area(clicked_area)

    update_highlighted_polygon(proxy = proxy_tab_1, selected_small_area(), selected_la())
    update_highlighted_polygon(proxy = proxy_tab_2, selected_small_area(), selected_la())

    #update within_areas aes
    index <- match(clicked_area, levels(within_areas_data_tab_1()$LongName))
    restyle_obj <- get_plot_trace_aesthetics(index)
    plotlyProxyInvoke(within_areas_1_proxy(), "restyle", list(opacity=restyle_obj$opacity, line.color=restyle_obj$colours),within_areas_1_proxy()$curveNumber)
    plotlyProxyInvoke(within_areas_2_proxy, "restyle", list(opacity=restyle_obj$opacity, line.color=restyle_obj$colours),within_areas_2_proxy$curveNumber)

    plotlyProxy("across_areas_plot_tab_1", session) %>%
      update_across_area_proxy(clicked_area, within_areas_data_tab_1())
    }
    
  })
  
  # Tab 2 click
  observeEvent(input$la_map_tab_2_shape_click, {
    
    event <- input$la_map_tab_2_shape_click
    
    if(!is.null(event$id)) {
    selected_small_area(event$id)

    update_highlighted_polygon(proxy = proxy_tab_1, selected_small_area(),selected_la())
    update_highlighted_polygon(proxy = proxy_tab_2, selected_small_area(),selected_la())
    
    index <- match(event$id, levels(measures_data_tab_2()$LongName))
    
    restyle_obj <- get_plot_trace_aesthetics(index)
    plotlyProxyInvoke(within_areas_2_proxy, "restyle", list(opacity=restyle_obj$opacity, line.color=restyle_obj$colours),within_areas_2_proxy$curveNumber)

    plotlyProxyInvoke(within_areas_1_proxy(), "restyle", list(opacity=restyle_obj$opacity, line.color=restyle_obj$colours),within_areas_1_proxy()$curveNumber)
    
    update_across_area_proxy(plotlyProxy("across_areas_plot_tab_1", session),
                               event$id, 
                               within_areas_data_tab_1())
    }
  })
  
  # The selected_small_area() reactiveVal depends on a submit click or a map click to be updated.
  # This presents problems on initial page-load when LA is absent, because selected_small_area is null which throws warnings server-side.
  # The following code runs once when LA is selected the first time to initialise the selected_small_area variable.
  
  # Tab 1
  observeEvent(input$la_choice_tab_1, {
    req(input$la_choice_tab_1 != "") 
    small_area_options <- small_area_lookup %>%
      filter(Council.Name == input$la_choice_tab_1) %>%
      pull(LongName)
    default_area <- small_area_options[1]
    selected_small_area(default_area)
  }, 
  ignoreInit = TRUE, once = TRUE)
  
  # Tab 2
  observeEvent(input$la_choice_tab_2, {
    req(input$la_choice_tab_2 != "")
    small_area_options <- small_area_lookup %>%
      filter(Council.Name == input$la_choice_tab_2) %>%
      pull(LongName)
    default_area <- small_area_options[1]
    selected_small_area(default_area)
  }, 
  ignoreInit = TRUE, once = TRUE)

# Observe Event: Intro/Guide --------------

  observeEvent(input$help_tab1,{
    #set tab view
    updateTabsetPanel(session, "tab_1_plots",
                      selected = "within_areas")
    #filter introducion step df
    these_steps <- intro_df %>% filter(tab == "population")
    #run intro sequence
    introjs(session, 
            options = list("nextLabel"="Next",
                           "prevLabel"="Back",
                           "skipLabel"="Skip",
                           steps = these_steps) 
            
    )
  })
  
  observeEvent(input$help_tab2,{
    #filter intro df for tab specific steps
    this <- intro_df %>% filter(tab == "other_measures")
    #run intro sequence
    introjs(session, 
            options = list("nextLabel"="Next",
                           "prevLabel"="Back",
                           "skipLabel"="Skip",
                           steps = this) 
            
    )
  })
  
  observeEvent(input$help_tab3,{
    #set choices
    updateSelectizeInput(session, "measure_choice_tab_3", selected = "Population Data")
    updateRadioButtons(session, "granularity_selection", selected = "Custom Population Data")
    this <- intro_df %>% filter(tab == "download")
    #run intro sequence
    introjs(session, 
            options = list("nextLabel"="Next",
                           "prevLabel"="Back",
                           "skipLabel"="Skip",
                           steps = this) 
            
    )
  })
  
# Tab 2: Create Map LA Output ---------------

  # RenderLeaflet for council level map - output name = la_map_tab_1
  output$la_map_tab_2 <- renderLeaflet({
    
   isolate({ 
     council_map_data <- measures_data %>%
      filter(Council.Name == selected_la(),
             Year == selected_year(),
             Measure == input$measure_choice_tab_2,
             Level == "Small Area"
      ) %>%
      ungroup()
    
    # Filter shape file to selected council before combining with data
    filtered_shape <- filter(shape_data, Council == selected_la())
    # Combine map data with shape file 
    combined_data <- left_join(filtered_shape, 
                               council_map_data,
                               by = c("SubCouncil" = "LongName")
    )
    
    create_map(combined_data,
               tab = 2,
               #only consider changes to selected_small_area() when other inputs have also changed
               #because small_area change on its own is reflected in the output by proxy_tab_2
               default_area = selected_small_area(),
               initial_polygon_click = initial_polygon_click())
    })
  }) %>% bindEvent(c(selected_la(),
                     selected_year()))

  proxy_tab_2 <- leafletProxy("la_map_tab_2")

# Tab 2: tab_2_graphs UI output---------------
  
  output$tab_2_graphs <- renderUI ({
    if (initial_polygon_click() == FALSE) {
      return(div(paste0("Click on a  sub council areas on the map to begin exploring projected change in ", input$measure_choice_tab_2, ".")))
    } else {
      div(
        plotlyOutput("within_areas_plot_tab_2", 
                   height = "360px"
                   ) %>% 
          withSpinner(type = 6),
        span(htmlOutput("within_la_text_tab_2"), 
           style = "color:#526470; font-size = 12px")
        ) #div end
    }
  })
   
# Tab2: Within Areas Plot Data-------------------------------------------
  
  # Filters measure_data by selected council and measure. Is updated when either changes.
  # Additionally changes factor levels so that the selected council is first followed by the rest
  # alphabetically. This allows the selected council to have a different line colour - controlled
  # by create_line_graph function.
  measures_data_tab_2 <- eventReactive(list(selected_la(),
                                             initial_polygon_click()
                                            ), {
    measures_data <- measures_data  %>%
      filter(Council.Name == selected_la() &
               Measure == input$measure_choice_tab_2 &
               Level == "Small Area"
             )
    council_small_areas <- sort(unique(measures_data$LongName))

    measures_data$LongName <- factor(measures_data$LongName,
                                     levels = council_small_areas,
                                     ordered = TRUE)
    return(measures_data)
  })
  
# Tab 2: Create Within Areas Plot-------------------------------------------------  
  # Since the reactive parameters passed to create_line_plot are themselves only updated in 
  # response to submit buttons or map clicks, this output is also only reactive to these events
  output$within_areas_plot_tab_2 <- renderPlotly({
    req(initial_polygon_click())
    
    isolate({
      create_line_plot(dataset = measures_data_tab_2(),
                       small_area_selection = isolate(selected_small_area()),
                       # Only consider input$measure when other parameter are changed (since these 
                       # are changed by submit/polygon clicks)
                       graph_type = "Within Areas",
                       tab = 2)
    })
  }) %>% bindEvent(c(measures_data_tab_2()))
  
  within_areas_2_proxy <- plotlyProxy("within_areas_plot_tab_2", session)
  
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
          "<b>Sex Ratio</b> = number of males per 100 females"
        } else {
          if (input$measure_choice_tab_2 == "Dependency Ratio") {
            "<b>Dependency Ratio</b> = population of children (aged 0-15) and elderly people (aged 65+) as a proportion of the working age population (aged 16-64)."
          } else {
            if (input$measure_choice_tab_2 == "Life Expectancy") {
              "<b>Life Expectancy</b> = expectation of life as an average across males and females"
            } else {
              return()
            }
          }
        }
      }
    }
    text
  })

  # Plot text will not render until requirements within render_within_la_text_tab_2() are met
  output$within_la_text_tab_2 <- renderText({
    #render_within_la_text_tab_2()
    isolate({
      paste0("Showing projected change in <b>",
             input$measure_choice_tab_2,
             "</b> in <b>",
             selected_small_area(),
             "</b> compared to other small areas in <b>",
             selected_la(),
             "</b>.<br><br>",
             measure_info(),
             "<br><br>Click on the map or click on sub-council areas in the legend to explore the data.<br>   "
      )
    }) #end of isolate
  }) %>% bindEvent(selected_small_area(), selected_la())
  
  

# Tab 3 - Data filter  ---------------------------------------------
  
  #render download button only when there is data on display
  output$download_button <- renderUI ({
    if(input$measure_choice_tab_3 == '' || is.null(input$la_choice_tab_3)) {
      div()
    } else {
      downloadButton("dl_data_tab_3", "Download this Selection")
    }
    
  })
  
  selected_ages_dwnld <- reactiveVal(c(0, 90))
  selected_sex_dwnld <- reactiveVal(c("Females", "Males", "Persons"))
  selected_years_dwnld <- reactiveVal(c(2018,2030))
  
  observeEvent(input$apply_filters,{
     selected_ages <- as.numeric(input$age_choice_tab_3)
     if(is.na(selected_ages[1])) {selected_ages[1] <- 90}
     if(is.na(selected_ages[2])) {selected_ages[2] <- 90}
    selected_ages_dwnld(selected_ages)
    selected_sex_dwnld(input$gender_choice_tab_3)
    selected_years_dwnld(input$year_select_tab3)
  })
  
  
  
  dl_measures_data <- reactive({
    req(input$la_choice_tab_3)
    if (input$measure_choice_tab_3 != "Population Data") {
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
      if (input$granularity_selection == "Custom Population Data") {
        # Store selected age range
        age_range <- c(selected_ages_dwnld()[1]:selected_ages_dwnld()[2])
        # Store selected year range
        year_range <- as.character(c(selected_years_dwnld()[1]:selected_years_dwnld()[2]))
        # Filter data based on selections
        dta <- filter_n_format(projection_data, 
                               small_area_lookup, 
                               input$la_choice_tab_3, 
                               age_range, 
                               year_range, 
                               selected_sex_dwnld())
      } else {
        dta <- filter(measures_data, 
                      Council.Name %in% input$la_choice_tab_3 & Measure == "Total Population"
        )
        # Pivot_wider
        dta$Value <- round(dta$Value, 2)
        dta <- dta %>% 
          select(Council.Name, LongName, Year, Value) %>% 
          # Remove newlines from long sub-council area names
          mutate(LongName = stringr::str_replace_all(LongName, "\n", " ")) %>%
          pivot_wider(names_from = Year, values_from = Value) %>%
          dplyr::rename(Council = Council.Name, "Sub-Council Area" = LongName)
      }
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
  output$dl_data_tab_3 <- downloadHandler(
    filename = paste(paste0("population_dl_", Sys.Date()), ".csv", sep = ""),
    content = function(con) {
      write.csv(dl_measures_data(), con, row.names = FALSE)
    }
  )
  
  # descriptionfile <- reactive({
  #   if(input$measure_choice_tab_3 == "Population Data" & 
  #      input$granularity_selection == "Custom Population Data") {
  #     decsription_file <- make_txt_file(measure = "Custom Population Data",
  #                                       councils = input$la_choice_tab_3,
  #                                       ages = c(selected_ages_dwnld[1]:selected_ages_dwnld[2]),
  #                                       sex = selected_sex_dwnld,
  #                                       years = selected_years_dwnld)
  #   } else {
  #     description_file <- make_txt_file(measure = input$measure_choice_tab_2,
  #                                       councils = input$la_choice_tab_3)
  #       
  #   }
  # }) %>% bindEvent(input$dl_da)
  # # Data to download based on selections in tab 3
  # output$dl_data_tab_3 <- downloadHandler(
  #   filename = paste(paste0("population_dl_", Sys.Date()), ".zip", sep = ""),
  #   content = function(file) {
  #     tmpdir <- tempdir()
  #     setwd(tempdir())
  #     print(tempdir())
  # 
  #     if(input$measure_choice_tab_3 == "Population Data" & 
  #        input$granularity_selection == "Custom Population Data") {
  #       make_txt_file(measure = "Custom Population Data",
  #                                         councils = input$la_choice_tab_3,
  #                                         ages = c(selected_ages_dwnld[1]:selected_ages_dwnld[2]),
  #                                         sex = selected_sex_dwnld,
  #                                         years = selected_years_dwnld)
  #     } else {
  #       make_txt_file(measure = input$measure_choice_tab_2,
  #                     councils = input$la_choice_tab_3)
  #       
  #     }
  #     write.csv(dl_measures_data(), "data.csv", row.names = FALSE)
  #     zip(zipfile = file, 
  #         files = c("data.csv", 
  #                   "data_description.txt")
  #         )
  #     if(file.exists(paste0(file, ".zip"))) {file.rename(paste0(file, ".zip"), file)}
  #     },
  #   contentType = "application/zip"
  # )
}