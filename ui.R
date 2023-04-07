ui <- tagList(tags$head(withAnim(),
                        useShinyjs(),
                        tags$style(
                          HTML(' #year_panel {background-color: rgba(255,255,255, 0.5);
                          border-bottom-color:rgba(0,0,255,0);
                          border-left-color:rgba(0,0,255,0);
                          border-right-color:rgba(0,0,255,0);
                              border-top-color:rgba(0,0,255,0);}')) #this ensures background of year panel is transparent
                        #use_tota11y() requires shinya11y package, for assessing accessibility
                        ),
              navbarPage(title = "Sub-Council Population Projections",
# Population Size Tab (Tab 1)-----------------------------------------------------------                           
                         tabPanel("Population size", 
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
                                                  # Add sliderinput for age range - inputID = age_choice_tab_1
                                                  sliderInput(inputId = "age_choice_tab_1", 
                                                              label = "Select ages to include:", 
                                                              min = 0, 
                                                              max = 90, 
                                                              step = 1, 
                                                              value = c(0, 90), 
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
                                                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                                               disabled = '' #disabled on initial load until LA selected
                                                               )
                                                  )
                                           ),
                                           # End of fluidRow
                                           #), 
                                  # If council input and year are not blank show council map
                                  fluidRow(conditionalPanel(condition = "input.submit_tab_1 != 0 || input.submit_tab_2 != 0", 
                                                            column(6,
                                                                   div(
                                                                     #h3("Population Size"),
                                                                     leafletOutput("la_map_tab_1", width = "100%", height = "80vh") %>%
                                                                     withSpinner(type = 6),
                                                                   absolutePanel(id = "year_panel",
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
                                                        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                                        disabled = '' #disabled on initial load until LA selected
                                                        )
                                           ),
                                    # End of fluidRow
                                    ),
                                  fluidRow(conditionalPanel(condition = "input.submit_tab_1 != 0 || input.submit_tab_2 != 0", #if either button has ever been clicked
                                                            column(6, 
                                                                   leafletOutput("la_map_tab_2", width = "100%", height = "80vh") %>%
                                                                     withSpinner(type = 6),
                                                                   
                                                                   absolutePanel(id = "year_panel",
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
                   # Data download button
                   downloadButton("dl_data_tab_3", "Download this Selection"),
                   p(style = "display:inline-block", "All data can be accessed on the IS"), 
                   a(href = "https://www.improvementservice.org.uk/products-and-services/data-and-intelligence2/sub-council-area-population-projections/downloads", 
                     target = "_blank",
                     "website"
                   )
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
                                               # Age choice slider input
                                               sliderInput(inputId = "age_choice_tab_3", 
                                                           label = "Select ages to include:", 
                                                           min = 0, 
                                                           max = 90, 
                                                           step = 1, 
                                                           value = c(0,90), 
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