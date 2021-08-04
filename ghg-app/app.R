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
  style = "font-family: 'Open Sans', sans-serif;",
  tags$h4("University of Otago's Sustainability Office"),
  tags$br(),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      width = 4,
      tags$h3("Student numbers"),
      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: 	#00508F}")),
      sliderInput("bins",
                  "(Percentage change)",
                  min = 1,
                  max = 50,
                  value = 30),
      tags$h3("Behavioural change"),
      tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: 	#00508F}")),
      sliderInput("bins",
                  "(Lorem ipsum)",
                  min = 1,
                  max = 50,
                  value = 30),
      tags$h3("NZ electricity grid"),
      tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: 	#00508F}")),
      sliderInput("bins",
                  "(Percentage to renewables)",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tags$style(" {background-color: #dd4b39;}"),
      plotOutput("distPlot"),
      tags$br(),
      tags$div(
        tags$p(tags$h3("About this dashboard")), 
        tags$p("The purpose of this dashboard is to help you easily understand the 
                    high-level impacts of various factors on 
                    net greenhouse gas emissions (from a University context)."),
        tags$p("The University of Otago has set an ambitious goal of Net Carbon Zero by year 2030.
                    Net Carbon Zero refers to the equivalent of net zero greenhouse gas emissions after 
                    efforts to both reduce greenhouse gas emissions and offsetting these emissions through 
                    various strategies or initiatives."),
        tags$p("By engaging with this dashboard, we hope to drive awareness and behavioural change, 
                    alongside guiding the University's strategic approach towards Net Carbon Zero.
                    "),
        style = "font-family: 'Open Sans', sans-serif; font-size: 16px; font-weight: normal; line-height: 1.8;"
      ),
      tags$div(
        tags$p(tags$h3("Further information")), 
        tags$p("To add how many hectares of trees would need to be planted by 2025 in order to reach Net Zero emissions in 2030 for a given set of variables. 
        This is based on 1 hectare of new indigenous forest sequestering 7.8 tonnes of CO2-e by it’s fifth year, so the formula would = total university emissions 
        (in tonnes of CO-equivalent) / 7.8 = # of hectares to plant"),
        tags$p("How much it would cost to buy carbon credits from the market to offset this level of emissions = total emissions * $150"),
        tags$p(tags$h3("Resources")),
        style = "font-family: 'Open Sans', sans-serif; font-size: 16px; font-weight: normal; line-height: 1.8;",
        tags$p(tags$a(href="https://www.otago.ac.nz/sustainability/about/", "University of Otago's Sustainability Office")),
        tags$p(tags$a(href="https://www.otago.ac.nz/sustainability/news/otago828588.html", "University of Otago makes submission to ORC's draft 10 year plan")),
        tags$p(tags$a(href="https://www.otago.ac.nz/sustainability/otago824241.pdf", "University of Otago's 2019 Greenhouse Gas Inventory")),
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
