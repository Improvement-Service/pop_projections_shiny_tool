ui <- tagList(tags$head(withAnim()
                        #use_tota11y() requires shinya11y package, for assessing accessibility
                        ),
# Footer ----------------------------------------------------------------------------              
              tags$footer(column(6,
                                 # Link to IS website
                                 tags$a(href="https://www.improvementservice.org.uk",
                                        tags$b("Improvement Service"),
                                        class = "externallink",
                                        style = "color: white; text-decoration: none"
                                 )
              ),
              column(6,
                     # Link to research email
                     tags$a(href="mailto:research@improvementservice.org.uk",
                            tags$b("Contact us"),
                            class="externallink",
                            style = "color: white; text-decoration: none"
                     )
              ),
              style = 
                "position:fixed;
                                      text-align:center;
                                      left: 0;
                                      bottom:0;
                                      width:100%;
                                      z-index:1000;
                                      height:20px; /* Height of the footer */
                                      color: white;
                                      padding: 0px;
                                      font-weight: bold;
                                     background-color: #1995dc"
              
              ),
# navbarPage set up -----------------------------------------------------
              navbarPage(title = "Sub-Council Area Population Projections",
                         id = "main_tabs",
# Home Page ------------------------------------------------------------
                         tabPanel(title = "Home Page",
                                  fluidRow(column(6,
                                                  # About the tool text
                                                  tags$div(tags$h3("About this tool"),
                                                           tags$h5("This interactive visualisation presents sub-council area population projections produced by the Improvement Service in collaboration with the National Records of Scotland."),
                                                           tags$h5("Sub-council area population projections can help local authorities understand how the future size and structure of the population is likely to vary across the local authority area and how the rate of change varies across these areas."),
                                                           tags$h5("These insights can support a wide range of local level decision making and planning including:"),
                                                           tags$li("housing need"),
                                                           tags$li("healthcare planning"),
                                                           tags$li("demand assessment such as early years and education or social care provision.")
                                                           )
                                                  ),
                                           # Tab links and description
                                           column(6,
                                                  tags$div(actionLink("pop_size_link", 
                                                                      "Population Size", 
                                                                      style = "font-size: 14px;"
                                                                      ),
                                                           tags$h5("This tab allows users to analyse how the size and make up of the population is projected to change within a local authority area."),
                                                           actionLink("other_measures_link", 
                                                                      "Other Measures", 
                                                                      style = "font-size: 14px;"
                                                                      ),
                                                           tags$h5("This tab allows users to analyse the components of population change, including net migration and natural change, to understand how projected changes in these will contribute to growth or decline in population size. It also shows how the changing population is projected to impact on the make up of the population, including, dependency ratio, sex ratio and life expectancy."),
                                                           actionLink("data_tab_link", 
                                                                      "Data Download", 
                                                                      style = "font-size: 14px;"
                                                                      ),
                                                           tags$h5("This tab allows users to download bespoke selections of the data used throughout the tool."),
                                                           actionLink("feedback_tab_link", 
                                                                      "Feedback", 
                                                                      style = "font-size: 14px;"
                                                                      ),
                                                           tags$h5("If you'd like to provide feedback on the tool please complete the survey on this tab.")
                                                           )
                                                  )
                                           ),
                                  # Vertical line
                                  fluidRow(tags$div(tags$hr()),
                                           column(6,
                                                  # Further information text
                                                  tags$div(tags$h3("Further Information"),
                                                           tags$h5("Further information about the sub-council area population projections used in this tool can be found on the ", 
                                                                   tags$a(href = "https://www.improvementservice.org.uk/products-and-services/data-and-intelligence2/sub-council-area-population-projections", 
                                                                          "Improvement Service website."
                                                                          )
                                                                   ),
                                                           tags$h5("There you will find background information on the projection and a methodology note detailing how the projections were produced including assumptions made and caveats and limitations to consider when interpreting the data. The full sets of data can be downloaded from the website and answers to frequently asked questions can also be found."),
                                                           tags$h5("The following video provides an overview of how the projections were produced and how they may be used.")
                                                           )
                                                  ),
                                           column(6,
                                                  # Embedded youtube video
                                                  HTML('<iframe width="580" height="280" src="https://www.youtube-nocookie.com/embed/n8J_7SK3jlI?autoplay=0&showinfo=0&loop=1&rel=0" frameborder="0" allow="accelerometer; loop ;encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                                  )
                                           )
                                  ),
# Population Size Tab (Tab 1)-----------------------------------------------------------                           
                         tabPanel(title = "Population Size", 
                                  value = "population_size",
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
                                                                   leafletOutput("la_map_tab_1", width = "100%", height = "80vh") %>%
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
                                                                   leafletOutput("la_map_tab_2", width = "100%", height = "80vh") %>%
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
                          tabPanel(title = "Data Download",
                                   value = "data_download",
                                   fluidRow(column(2,
                                                   selectizeInput(inputId = "la_choice_tab_3", 
                                                                  choices = councils, 
                                                                  label = "Select Local Authority:",
                                                                  multiple = TRUE,
                                                                  options = list(placeholder = 'Select Council',
                                                                                 onInitialize = I('function() { this.setValue(""); }')
                                                                                 )
                                                                  )
                                                   ),
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
                                                                                value = c(0, 90), 
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
                                                                                value = c(2018, 2030),
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
                                   DT::DTOutput("preview_table_tab3") %>%
                                     withSpinner(type = 6)
                                   # TabPanel closing bracket
                                   ),
# Feedback tab ------------------------------------------------------------------
                          tabPanel(title = "Feedback",
                                   value = "feedback_tab",
                                   HTML('<iframe id="ss-embed-frame" onload="window.parent.parent.scrollTo(0,0)" src="https://www.smartsurvey.co.uk/s/NDC5JZ/" style="width:100%;height:550px;border:0px;padding-bottom:4px;" frameborder="0"><a href="https://www.smartsurvey.co.uk/s/NDC5JZ/">Please take our survey</a></iframe><div>Create your own online surveys with <a href="https://www.smartsurvey.co.uk">SmartSurvey</a>.</div>')
                                   ),
                        # End of navbar
                        ) 
              # End of tags$List
              ) 