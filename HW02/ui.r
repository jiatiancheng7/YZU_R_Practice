
library(shiny)

shinyUI(fluidPage(  
  titlePanel("Barcelona Accidents"),  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("loc", "Choose a location:",choices = unique(x[,2])),      

      radioButtons("dataset", "Column",choices = c("Injuries","Victims","Vehicles")),
      sliderInput("n", "number Range:", min = 1, max = 10335, value = 10300 ),
      submitButton("Update View")
    ),    
    mainPanel(
      h3(textOutput("caption")), 
      tabsetPanel(type="tabs",
                  tabPanel("Line plot",plotlyOutput("lineplot")),
                  tabPanel("Bar chart",plotlyOutput("barplot")),
                  tabPanel("Pie chart",plotlyOutput("pieplot"))
      ),
      verbatimTextOutput("summary"),  
      tableOutput("view")
    )
  )
))
