ui <- navbarPage(
  title = "Small Area Population Projections",
  tabPanel("Population size", 
           fluidRow(
             column(3),
             column(3),
             column(3),
             column(3)
           ),
           fluidRow()),
  tabPanel("Comparison of similar areas",
           fluidRow(
             column(3),
             column(3),
             column(3),
             column(3)
           ),
           fluidRow()
           )
)