#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
  tags$br(),
  tags$img(src = "https://www.otago.ac.nz/_assets/_gfx/logo@2x.png", width = "160px", height = "80px"),  
  titlePanel("Greenhouse Gas Emissions"),
    tags$h4("University of Otago's Sustainability Office"),
    tags$br(),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          width = 4,
          tags$h3("Student (% change)"),
            sliderInput("bins",
                        "Variable 1",
                        min = 1,
                        max = 50,
                        value = 30),
          sliderInput("bins",
                      "Variable 2",
                      min = 1,
                      max = 50,
                      value = 30),
          tags$h3("Behavioural change"),
          sliderInput("bins",
                      "Variable 3",
                      min = 1,
                      max = 50,
                      value = 30),
          sliderInput("bins",
                      "Variable 4",
                      min = 1,
                      max = 50,
                      value = 30),
          tags$h3("NZ electricity grid - % renewables"),
          sliderInput("bins",
                      "Variable 3",
                      min = 1,
                      max = 50,
                      value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           tags$br(),
           tags$div(
             tags$p(tags$h3("About this dashboard")), 
             tags$p("The purpose of this dashboard is to help stakeholders easily understand the 
                    high-level impacts of various factors (from a University context) on 
                    net greenhouse gas emissions. 
                    
                    The University of Otago has set an ambitious goal of Net Carbon Zero by year 2030.
                    Net Carbon Zero refers to the equivalent of net zero greenhouse gas emissions after 
                    efforts to both reduce greenhouse gases and offsetting these emissions through 
                    various strategies or initiatives.")
           ),
           tags$div(
             tags$p(tags$h3("Further resources")), 
             tags$p("Links to other relevant resources")
           ), 
           tags$br()
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
