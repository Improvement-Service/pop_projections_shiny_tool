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
             column(2,
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
             column(2,
                    checkboxGroupInput(
                      inputId = "gender_choice_tab_1",
                      label = "Select genders to include:",
                      choices = c("Males", "Females"),
                      selected = c("Males", "Females"),
                      inline = FALSE
                    )
             ),
             column(2,
                    actionButton("submit_tab_1", "Submit Selections", icon("paper-plane"), 
                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                    )
             ),
           fluidRow(
             # Add conditionalPanel, if council input is blank show Scotland map - outputID = scot_map_tab_1
             # This will not include absolute panel with across areas graph and within areas graph
             conditionalPanel(condition = "input.submit_tab_1 == 0 | input.la_choice_tab_1 == `` | input.year_choice_tab_1 == ``",
                              leafletOutput("scot_map_tab_1", width = "100%") %>%
                                # Creates a loading spinner
                                withSpinner(type = 6)
                              ),
             # Add 2nd conditionalPanel, if council input is not blank show council map - outputID = la_map_tab_1
             column(6,
                    conditionalPanel(condition = "input.submit_tab_1 != 0 && input.la_choice_tab_1 != `` && input.year_choice_tab_1 != `` ",
                                     leafletOutput("la_map_tab_1", width = "100%") %>%
                                       withSpinner(type = 6))),
             column(6,
                    conditionalPanel(condition = "input.submit_tab_1 != 0 && input.la_choice_tab_1 != `` && input.year_choice_tab_1 != `` ",# Gives the panel a border
                      tabsetPanel(type = "tabs",
                                  tabPanel("Population Index Across Scotland", span(textOutput("across_scotland_text"), style = "color:#526470; font-size = 12px"), plotlyOutput("across_areas_plot_tab_1", height = "360px") %>% withSpinner(type = 6)),
                                  tabPanel("Population Index Within Council Areas", plotlyOutput("within_areas_plot_tab_1", height = "360px") %>% withSpinner(type = 6), span(textOutput("within_la_text"), style = "color:#526470; font-size = 12px"))
                      
                      )
                    )
             )
           )),

# Similar Areas Tab (Tab 2) --------------------------------------------------------------------

  tabPanel("Comparison of similar areas",
            fluidRow(
           #   # Add selectize input for council dropdown - inputID = la_choice_tab_2
           #   column(3,
           #          selectizeInput(inputId = "la_choice_tab_2", 
           #                         choices = councils, 
           #                         label = NULL,
           #                         options = list(placeholder = 'Select Council',
           #                                        onInitialize = I('function() { this.setValue(""); }')
           #                         )
           #          )
           #   ),
           #   # Add UiOutput for small area dropdown - outputID = small_area_output_tab_2
           #   column(3, uiOutput("small_area_output_tab_2")
           #   ),
           #   # Add selectize input for year dropdown - inputID = year_choice_tab_2
           #   column(3,
           #          selectizeInput(inputId = "year_choice_tab_2", 
           #                         choices = years, 
           #                         label = NULL,
           #                         options = list(placeholder = 'Select Year',
           #                                        onInitialize = I('function() { this.setValue(""); }')
           #                         )
           #          )
           #   ),
           #   # Add selectize input for indicator dropdown - inputID = measure_choice_tab_2
           #   column(3,
           #          selectizeInput(inputId = "measure_choice_tab_2",  
           #                         choices = c("Total Population", "Dependency Ratio", "Gender Ratio"), 
           #                         label = "Show me similar areas based on:",
           #                         options = list(placeholder = 'Select measure',
           #                                        onInitialize = I('function() { this.setValue(""); }')
           #                                        )
           #                         )
           #          )
           #   ),
           # fluidRow(
           #   # Add conditionalPanel, if council and small area input are blank show Scotland map - outputID = scot_map_tab_2
           #   # This will not include absolute panel with across areas graph and similar areas graph
           #   
           #   # Add 2nd conditionalPanel, if council and small area input are not blank show council map - outputID = la_map_tab_2
           #   # This will include absolute panel with across areas graph and similar areas graph;
           #   # across areas graph - outputID = across_areas_plot_tab_2
           #   # similar areas graph - outputID = similar_areas_plot_tab_2
            )
           )
           
)
