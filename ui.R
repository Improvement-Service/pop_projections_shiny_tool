ui <- navbarPage(
  title = "Small Area Population Projections",

# Population Size Tab (Tab 1)-----------------------------------------------------------  
  
  tabPanel("Population size", 
           fluidRow(
             # Add selectize input for council dropdown - inputID = la_choice_tab_1
             column(3),
             # Add selectize input for year dropdown - inputID = year_choice_tab_1
             column(3),
             # Add sliderinput for age range - inputID = age_choice_tab_1
             column(3),
             # Add checkbox input for gender - inputID = gender_choice_tab_1
             column(3)
           ),
           fluidRow(
             # Add conditionalPanel, if council input is blank show Scotland map - outputID = scot_map_tab_1
             # This will include absolute panel with across areas graph and within areas graph;
             # across areas graph - outputID = across_areas_plot_tab_1
             # within areas graph - outputID = within_areas_plot_tab_1
            
             # Add 2nd conditionalPanel, if council input is not blank show council map - outputID = la_map_tab_1
             # This will include absolute panel with across areas graph and within areas graph;
             # across areas graph - outputID = across_areas_plot_tab_1
             # within areas graph - outputID = within_areas_plot_tab_1
           )),

# Similar Areas Tab (Tab 2) --------------------------------------------------------------------

  tabPanel("Comparison of similar areas",
           fluidRow(
             # Add selectize input for council dropdown - inputID = la_choice_tab_2
             column(3),
             # Add UiOutput for small area dropdown - outputID = small_area_output_tab_2
             column(3),
             # Add selectize input for year dropdown - inputID = year_choice_tab_2
             column(3),
             # Add selectize input for indicator dropdown - inputID = measure_choice_tab_2
             column(3)
           ),
           fluidRow(
             # Add conditionalPanel, if council and small area input are blank show Scotland map - outputID = scot_map_tab_2
             # This will include absolute panel with across areas graph and similar areas graph;
             # across areas graph - outputID = across_areas_plot_tab_2
             # similar areas graph - outputID = similar_areas_plot_tab_2
             
             # Add 2nd conditionalPanel, if council and small area input are not blank show council map - outputID = la_map_tab_2
             # This will include absolute panel with across areas graph and similar areas graph;
             # across areas graph - outputID = across_areas_plot_tab_2
             # similar areas graph - outputID = similar_areas_plot_tab_2
           ))
)