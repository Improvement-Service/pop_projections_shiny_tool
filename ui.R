ui <- tagList(tags$head(withAnim()),
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
                                           column(2,
                                                  # Add selectize input for year dropdown - inputID = year_choice_tab_1
                                                  selectizeInput(inputId = "year_choice_tab_1", 
                                                                 choices = years, 
                                                                 label = "Select year:",
                                                                 options = list(placeholder = 'Select Year',
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
                                                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                                               )
                                                  )
                                           # End of fluidRow
                                           ), 
                                  # If council input and year are not blank show council map
                                  fluidRow(conditionalPanel(condition = "input.submit_tab_1 != 0 && input.la_choice_tab_1 != `` && input.year_choice_tab_1 != `` ", 
                                                            column(6,
                                                                   leafletOutput("la_map_tab_1", width = "100%") %>%
                                                                     withSpinner(type = 6)
                                                                   ),
                                                            column(6,
                                                                   tabsetPanel(type = "tabs",
                                                                               tabPanel("Population Index Across Scotland",
                                                                                        plotlyOutput("across_areas_plot_tab_1", 
                                                                                                     height = "360px"
                                                                                                     ) %>% 
                                                                                          withSpinner(type = 6),
                                                                                        span(htmlOutput("across_scotland_text"), 
                                                                                             style = "color:#526470; font-size = 12px"
                                                                                             )
                                                                                        ),
                                                                               tabPanel("Population Index Within Council Areas", 
                                                                                        plotlyOutput("within_areas_plot_tab_1", 
                                                                                                     height = "360px") %>% 
                                                                                          withSpinner(type = 6), 
                                                                                        span(htmlOutput("within_la_text"), 
                                                                                             style = "color:#526470; font-size = 12px"
                                                                                             )
                                                                                        )
                                                                               # End of tabsetPanel
                                                                               ) 
                                                                   # End of column
                                                                   ) 
                                                            # End of post-input conditionalPanel
                                                            ) 
                                           # End of fluidRow
                                           ) 
                                  # End of tabPanel
                                  ), 
# Similar Areas Tab (Tab 2) --------------------------------------------------------------------
                         tabPanel("Other Measures", 
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
                                           # Add selectize input for year dropdown - inputID = year_choice_tab_2
                                           selectizeInput(inputId = "year_choice_tab_2", 
                                                          choices = years, 
                                                          label = "Select year:",
                                                          options = list(placeholder = 'Select Year',
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
                                                          options = list(placeholder = 'Select measure:',
                                                                         onInitialize = I('function() { this.setValue(""); }')
                                                                         )
                                                          )
                                           ),
                                    column(3,
                                           actionButton("submit_tab_2", 
                                                        "Submit Selections", 
                                                        icon("paper-plane"),
                                                        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                                        )
                                           ),
                                    # End of fluidRow
                                    ),
                                  fluidRow(conditionalPanel(condition = "input.submit_tab_2 != 0 && input.la_choice_tab_2 != `` && input.year_choice_tab_2 != `` && input.measure_choice_tab_2 != `` ",
                                                            column(6, 
                                                                   leafletOutput("la_map_tab_2", width = "100%") %>%
                                                                     withSpinner(type = 6)
                                                                   ),
                                                            column(6,
                                                                   plotlyOutput("within_areas_plot_tab_2", 
                                                                                height = "360px"
                                                                                ) %>% 
                                                                     withSpinner(type = 6),
                                                                   span(htmlOutput("within_la_text_tab_2"), 
                                                                        style = "color:#526470; font-size = 12px")
                                                                   )
                                                            )
                                           )
                                  # End of tabPanel
                                  ),

# Data Table / Data Download (Tab 3)--------------------------------------------
  tabPanel("Data Download",
           fluidRow(
             # Add selectize input for council dropdown - inputID = la_choice_tab_3
             column(2,
                    selectizeInput(inputId = "la_choice_tab_3", 
                                   choices = councils, 
                                   label = "Select Local Authority:",
                                   multiple = TRUE,
                                   options = list(placeholder = 'Select Council',
                                                  onInitialize = I('function() { this.setValue(""); }')
                                                  )
                                   )
                    ),
            # Selectize input for measure
            column(2,
                   selectizeInput(inputId = "measure_choice_tab_3",  
                                  choices = c("Detailed Population Data",
                                              "Total Population", 
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
            # Conditional panel to show other select options when measure is "Detailed Data"
            conditionalPanel(condition = "input.measure_choice_tab_3 == 'Detailed Population Data'",
                             column(2,
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
                                                       label = "Select Sex to include:",
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
                             column(2, 
                                    # Year choice slider input
                                    sliderInput(inputId = "year_select_tab3",
                                                label = "Select years to display:",
                                                min = 2018,
                                                max = 2030,
                                                step = 1,
                                                value = c(2018,2030),
                                                sep = ""
                                                )
                                    )
                             ),
            column(2,
                   # Data download button
                   downloadButton("dl_data_tab_3", "Download this Selection"),
                   p(style = "display:inline-block", "All data can be accessed on the IS"), 
                   a(href = "https://www.improvementservice.org.uk/products-and-services/data-and-intelligence2/sub-council-area-population-projections/downloads", 
                     target = "_blank",
                     "website"
                     )
                   )
            # FluidRow closing bracket
            ),
           # Data table 
           DT::DTOutput("preview_table_tab3")
           # TabPanel closing bracket
           )
                          # End of navbar
                          ) 
              # End of tags$List
              ) 