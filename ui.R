ui <- tagList(tags$head(withAnim(),
                        useShinyjs(),
                        introjsUI(),
                        tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
                        #use_tota11y() requires shinya11y package, for assessing accessibility
                        ),
              navbarPage(title = "Sub-Council Population Projections", id = "navbar",
# Population Size Tab (Tab 1)-----------------------------------------------------------                           
                         tabPanel("Population size", value = "population_size", 
                                  # Add selectize input for council dropdown - inputID = la_choice_tab_1
                                  fluidRow(column(3,
                                                  selectizeInput(inputId = "la_choice_tab_1", 
                                                                 choices = councils, 
                                                                 label = "Select Local Authority:",
                                                                 options = list(placeholder = 'Select Council',
                                                                                onInitialize = I('function() { this.setValue(""); }')
                                                                                )
                                                                 )
                                                  ),
                                           column(3,
                                                  sliderTextInput(
                                                    inputId = "age_choice_tab_1",
                                                    label = "Select age / range to include:",  
                                                    choices = age_names,
                                                    selected = age_names[c(1, 91)],
                                                    dragRange = TRUE
                                                  )
                                                  
                                                  ),
                                           column(2,
                                                  # Add checkbox input for gender - inputID = gender_choice_tab_1
                                                  checkboxGroupInput(inputId = "gender_choice_tab_1",
                                                                     label = "Select Sex to include:",
                                                                     choices = c("Males", "Females"),
                                                                     selected = c("Males", "Females"),
                                                                     inline = FALSE
                                                                     )
                                                  ),
                                           column(2,
                                                  actionButton("submit_tab_1", 
                                                               "Submit Selections",
                                                               icon("paper-plane"),
                                                               disabled = '' #disabled on initial load until LA selected
                                                               )
                                                  ),
                                           column(1),
                                           column(1,
                                                  uiOutput("help1")
                                                  )
                                           # End of fluidRow
                                           ), 
                                  # If council input and year are not blank show council map
                                  fluidRow(conditionalPanel(condition = "input.submit_tab_1 != 0 || input.submit_tab_2 != 0", 
                                                            column(6,
                                                                   div(
                                                                     #h3("Population Size"),
                                                                     leafletOutput("la_map_tab_1", width = "100%", height = "80vh") %>%
                                                                     withSpinner(type = 6),
                                                                   absolutePanel(id = "year_panel_1",
                                                                                 class = "panel panel-default", 
                                                                                 fixed = FALSE,
                                                                                 draggable = FALSE,
                                                                                 # top = "0", 
                                                                                 # left = "280", 
                                                                                 top = "10", 
                                                                                 left = "20", 
                                                                                 right = "auto", 
                                                                                 bottom = "auto",
                                                                                 width = 210,
                                                                                 opacity = 0.8,
                                                                                 
                                                                                 pickerInput(
                                                                                   inputId = "year_choice_tab_1",
                                                                                   label = "Population size in:", 
                                                                                   selected = years[1],
                                                                                   choices = list(
                                                                                     "Real" = years[1],
                                                                                     "Projected" = years[2:13])
                                                                                 )
                                                                   )) #end of absolutePanel / div
                                                                   ),
                                                            column(6,
                                                                   #rendered dynamically when inital polygon click detected
                                                                   uiOutput("tabsetPanel")
                                                                   # End of column
                                                                   ) 
                                                            # End of post-input conditionalPanel
                                                            ) 
                                           # End of fluidRow
                                           ) 
                                  # End of tabPanel
                                  ), 
# Other Measures Tab (Tab 2) --------------------------------------------------------------------
                         tabPanel(title = "Other Measures", 
                                  value = "other_measures",
                                  fluidRow(
                                    column(3,
                                           # Add selectize input for council dropdown - inputID = la_choice_tab_2
                                           selectizeInput(inputId = "la_choice_tab_2", 
                                                          choices = councils, 
                                                          label = "Select Local Authority:",
                                                          options = list(placeholder = 'Select Council',
                                                                         onInitialize = I('function() { this.setValue(""); }')
                                                                         )
                                                          )
                                           ),
                                    column(3,
                                           # Add selectize input for indicator dropdown - inputID = measure_choice_tab_2
                                           selectizeInput(inputId = "measure_choice_tab_2",  
                                                          choices = c("Total Population", 
                                                                      "Net Migration", 
                                                                      "Natural Change",
                                                                      "Sex Ratio", 
                                                                      "Dependency Ratio", 
                                                                      "Life Expectancy - Persons"
                                                                      ), 
                                                          label = "Select measure:",
                                                          selected = "Dependency Ratio"
                                                          )
                                           ),
                                    column(3,
                                           actionButton("submit_tab_2", 
                                                        "Submit Selections", 
                                                        icon("paper-plane"),
                                                        disabled = '' #disabled on initial load until LA selected
                                                        )
                                           ),
                                    column(2),
                                    column(1,
                                           uiOutput("help2")
                                    )
                                    # End of fluidRow
                                    ),
                                  fluidRow(conditionalPanel(condition = "input.submit_tab_1 != 0 || input.submit_tab_2 != 0", #if either button has ever been clicked
                                                            column(6, 
                                                                   leafletOutput("la_map_tab_2", width = "100%", height = "80vh") %>%
                                                                     withSpinner(type = 6),
                                                                   
                                                                   absolutePanel(id = "year_panel_2",
                                                                                 class = "panel panel-default", 
                                                                                 fixed = FALSE,
                                                                                 draggable = FALSE,
                                                                                 top = "10", 
                                                                                 left = "20", 
                                                                                 right = "auto", 
                                                                                 bottom = "auto",
                                                                                 width = 200,
                                                                                 
                                                                                 pickerInput(
                                                                                   inputId = "year_choice_tab_2",
                                                                                   label = "Value in:", 
                                                                                   selected = years[1],
                                                                                   choices = list(
                                                                                     "Real" = years[1],
                                                                                     "Projected" = years[2:13])
                                                                                 )
                                                                   ) #end of absolutePanel
                                                                   
                                                                   ), #end of column
                                                            column(6,
                                                                   uiOutput("tab_2_graphs") #and annotations
                                                                   )
                                                            )
                                           )
                                  # End of tabPanel
                                  ),

# Data Table / Data Download (Tab 3)--------------------------------------------
  tabPanel(
    title = "Data Download",
    value = "download",
           fluidRow(
             # Add selectize input for council dropdown - inputID = la_choice_tab_3
             column(4,
                    pickerInput(
                      inputId = "la_choice_tab_3",
                      choices = c("Scotland", councils), 
                      label = "Select Local Authorities:",
                      options = list(
                        `actions-box` = TRUE), 
                      multiple = TRUE
                    )
                    ),
            # Selectize input for measure
            column(4,
                   selectizeInput(inputId = "measure_choice_tab_3",  
                                  choices = c("Detailed Population Data",
                                              "Net Migration", 
                                              "Natural Change",
                                               "Sex Ratio", 
                                               "Dependency Ratio", 
                                               "Life Expectancy - Persons"
                                              ), 
                                  label = "Select measure:",
                                  options = list(placeholder = 'Select measure:',
                                                 onInitialize = I('function() { this.setValue(""); }')
                                                 )
                                  )
                   ),
            column(4,
                   
                   p(style = "display:inline-block", "All data can be accessed on the IS"), 
                   a(href = "https://www.improvementservice.org.uk/products-and-services/data-and-intelligence2/sub-council-area-population-projections/downloads", 
                     target = "_blank",
                     "website"
                   ),
                   # Data download button
                   downloadButton("dl_data_tab_3", "Download this Selection")
            )
           ), #end of fluidRow
    fluidRow (
      # Conditional panel to show other select options when measure is "Detailed Data"
      conditionalPanel(condition = "input.measure_choice_tab_3 == 'Detailed Population Data'",
                       column(2,
                              radioButtons("granularity_selection",
                                           label = "Select detail:",
                                           choices = c("Total Area Population", "Custom Population Data"),
                                           selected = "Total Area Population"
                                           )
                              ),
                       conditionalPanel(condition = "input.granularity_selection == 'Custom Population Data'",
                                        column(3,
                                               sliderTextInput(
                                                 inputId = "age_choice_tab_3",
                                                 label = "Select ages to include:",  
                                                 choices = age_names,
                                                 selected = age_names[c(1, 91)],
                                                 dragRange = TRUE
                                               )
                                               
                                               ),
                                        column(2,
                                               # Gender choice slider input
                                               checkboxGroupInput(inputId = "gender_choice_tab_3",
                                                                  label = "Select sex to include:",
                                                                  choices = c("Males" = "Males", 
                                                                              "Females" = "Females",
                                                                              "Total" = "Persons"
                                                                              ),
                                                                  selected = c("Males", 
                                                                               "Females",
                                                                               "Persons"
                                                                               ),
                                                                  inline = FALSE
                                                                  )
                                               ),
                                        column(3, 
                                               # Year choice slider input
                                               sliderInput(inputId = "year_select_tab3",
                                                           label = "Select years to include:",
                                                           min = 2018,
                                                           max = 2030,
                                                           step = 1,
                                                           value = c(2018,2030),
                                                           sep = ""
                                                           )
                                               ),
                                        column(2, 
                                               # Year choice slider input
                                               actionButton(inputId = "apply_filters",
                                                           label = "Apply Filters"
                                               )
                                        )
                                        ) #end of conditionalPanel
                       ) #end of conditionalPanel
      ), #end of fluidRow
    # Data table 
    DT::DTOutput("preview_table_tab3") %>%
      withSpinner(type = 6)
    
    ) # TabPanel closing bracket
)# End of navbar
) # End of tags$List