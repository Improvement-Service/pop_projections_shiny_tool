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
                                                                     label = "Select genders to include:",
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
                                  fluidRow(conditionalPanel(condition = "input.la_choice_tab_2 != 0 | input.measure_choice_tab_2 != 0",
                                                            leafletOutput("la_map_tab_2", width = "100%") %>%
                                                              withSpinner(type = 6)
                                                            )
                                           )
                                  # End of tabPanel
                                  )
                          # End of navbar
                          ) 
              # End of tags$List
              ) 