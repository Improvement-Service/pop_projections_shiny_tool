ui <- navbarPage(
  title = "Small Area Population Projections",

# Population Size Tab (Tab 1)-----------------------------------------------------------  
  
  tabPanel("Population size", 
           fluidRow(
             # Add selectize input for council dropdown - inputID = la_choice_tab_1
             column(3,
                    selectizeInput(inputId = "la_choice_tab_1", 
                                   choices = councils, 
                                   label = NULL,
                                   options = list(placeholder = 'Select Council',
                                                  onInitialize = I('function() { this.setValue(""); }')
                                   )
                    )
             ),
             # Add selectize input for year dropdown - inputID = year_choice_tab_1
             column(3,
                    selectizeInput(inputId = "year_choice_tab_1", 
                                   choices = years, 
                                   label = NULL,
                                   options = list(placeholder = 'Select Year',
                                                  onInitialize = I('function() { this.setValue(""); }')
                                   )
                    )
             ),
             # Add sliderinput for age range - inputID = age_choice_tab_1
             column(3,
                    sliderInput(inputId = "age_choice_tab_1", 
                                label = "Select ages to include:", 
                                min = 0, 
                                max = 90, 
                                step = 1, 
                                value = c(0,90), 
                                dragRange = TRUE )
             ),
             # Add checkbox input for gender - inputID = gender_choice_tab_1
             column(3,
                    checkboxGroupInput(
                      inputId = "gender_choice_tab_1",
                      label = "Select genders to include:",
                      choices = c("Males", "Females"),
                      selected = c("Males", "Females"),
                      inline = FALSE
                    )
             )
           ),
           fluidRow(
             # Add conditionalPanel, if council input is blank show Scotland map - outputID = scot_map_tab_1
             # This will not include absolute panel with across areas graph and within areas graph
             conditionalPanel(condition = "input.la_choice_tab_1 == `` | input.year_choice_tab_1 == ``",
                              leafletOutput("scot_map_tab_1", width = "100%") %>%
                                # Creates a loading spinner
                                withSpinner(type = 6)
             ),
             # Add 2nd conditionalPanel, if council input is not blank show council map - outputID = la_map_tab_1
             conditionalPanel(condition = "input.la_choice_tab_1 != `` && input.year_choice_tab_1 != ``",
                              leafletOutput("la_map_tab_1", width = "100%") %>%
                                # Creates a loading spinner
                                withSpinner(type = 6),
                              absolutePanel(# Gives the panel a border
                                            class = "panel panel-default",
                                            fixed = TRUE,
                                            draggable = FALSE, 
                                            top = 160, 
                                            left = "auto", 
                                            right = 20, 
                                            bottom = "auto",
                                            width = 400, 
                                            height = "auto",
                                            plotlyOutput("across_areas_plot_tab_1", height = "300px")
                                            )
                              )
             # This will include absolute panel with across areas graph and within areas graph;
             # across areas graph - outputID = across_areas_plot_tab_1
             # within areas graph - outputID = within_areas_plot_tab_1
           )),

# Similar Areas Tab (Tab 2) --------------------------------------------------------------------

  tabPanel("Comparison of similar areas",
           fluidRow(
             # Add selectize input for council dropdown - inputID = la_choice_tab_2
             column(3,
                    selectizeInput(inputId = "la_choice_tab_2", 
                                   choices = councils, 
                                   label = NULL,
                                   options = list(placeholder = 'Select Council',
                                                  onInitialize = I('function() { this.setValue(""); }')
                                   )
                    )
             ),
             # Add UiOutput for small area dropdown - outputID = small_area_output_tab_2
             column(3, uiOutput("small_area_output_tab_2")
             ),
             # Add selectize input for year dropdown - inputID = year_choice_tab_2
             column(3,
                    selectizeInput(inputId = "year_choice_tab_2", 
                                   choices = years, 
                                   label = NULL,
                                   options = list(placeholder = 'Select Year',
                                                  onInitialize = I('function() { this.setValue(""); }')
                                   )
                    )
             ),
             # Add selectize input for indicator dropdown - inputID = measure_choice_tab_2
             column(3,
                    selectizeInput(inputId = "measure_choice_tab_2",  
                                   choices = c("Total Population", "Dependency Ratio", "Gender Ratio"), 
                                   label = "Show me similar areas based on:",
                                   options = list(placeholder = 'Select measure',
                                                  onInitialize = I('function() { this.setValue(""); }')
                                                  )
                                   )
                    )
             ),
           fluidRow(
             # Add conditionalPanel, if council and small area input are blank show Scotland map - outputID = scot_map_tab_2
             # This will not include absolute panel with across areas graph and similar areas graph
             
             # Add 2nd conditionalPanel, if council and small area input are not blank show council map - outputID = la_map_tab_2
             # This will include absolute panel with across areas graph and similar areas graph;
             # across areas graph - outputID = across_areas_plot_tab_2
             # similar areas graph - outputID = similar_areas_plot_tab_2
           ))
)