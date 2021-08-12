#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readr)
library(readxl)


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  tags$br(),
  tags$img(src = "https://www.otago.ac.nz/_assets/_gfx/logo@2x.png", width = "160px", height = "80px"),
  titlePanel(tags$strong("Greenhouse Gas Emissions Pathway Tool")),
  style = "font-family: 'Open Sans', sans-serif;",
  tags$h4("University of Otago's Sustainability Office"),
  tags$br(),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      width = 4,
      tags$h3("Student numbers"),
      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #00508F}")),
      sliderInput("StudentSlider",
                  "(Percentage change)",
                  min = 0,
                  max = 1,
                  value = 0,
                  step = 0.10),

      tags$h3("Behavioural change"),
      tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #00508F}")),
      sliderInput("BehaviourSlider",

                  "(Level of Change)",
                  min = -2,
                  max = +2,
                  value = 0),
      tags$h3("NZ electricity grid"),
      tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: 	#00508F}")),
      sliderInput("ElectricitySlider",
                  "(Percentage to renewables)",
                  min = 1,
                  max = 5,
                  value = 1)
    ),
   
    # Show a plot of the generated distribution
    mainPanel(
      tags$style(" {background-color: #dd4b39;}"),
      downloadButton('ExportPlot', 'Export as png'),
      
      plotOutput("plot"),

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
        tags$p("With the given set of variables currently i.e. student numbers, behavioural change, and NZ electricity grid,
        XXX hectares of trees would need to be planted by 2025 in order to reach net zero emissions in 2030.
        These figures are based on 1 hectare of new indigenous forest sequestering 7.8 tonnes of CO2-e by its fifth year."),
        tags$p("To buy carbon credits from the market to offset the current emissions, the cost would be XXX - total emissions * $150."),
        tags$p(tags$h3("Links to resources")),
        style = "font-family: 'Open Sans', sans-serif; font-size: 16px; font-weight: normal; line-height: 1.8;",
        tags$p(tags$a(href="https://www.otago.ac.nz/sustainability/about/", "University of Otago's Sustainability Office")),
        tags$p(tags$a(href="https://www.otago.ac.nz/sustainability/news/otago828588.html", "University of Otago makes submission to ORC's draft 10 year plan")),
        tags$p(tags$a(href="https://www.otago.ac.nz/sustainability/otago824241.pdf", "University of Otago's 2019 Greenhouse Gas Inventory"))
      ),
      tags$br()
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 
  Base_Scenario <- read_excel("Project_Figures.xlsx",
                              sheet = "4. Total base scenario", range = "A1:M11")
  #Pivoting base scenario table to make graphing easier.
  Base_Scenario <- Base_Scenario %>%
    pivot_longer(cols = `2021`:`2032`, names_to = "Year",
                 values_to ="Carbon_Emissions",
                 names_repair = "minimal")

Base_Scenario <- Base_Scenario %>% rename(Category = "Emissions")  

  #Rounding figures for base scenario table.
  Base_Scenario$Carbon_Emissions <- round(Base_Scenario$Carbon_Emissions,
                                          digits = 2)
  #Sorting the base scenario table for easier graphing.
  Base_Scenario <- Base_Scenario %>%
    group_by(Year) %>%
    arrange(Year)

  
  
  #Adjusted_Base_scenario
  Adjusted_Multiplier <- read_excel("Project_Figures.xlsx",

                                    sheet = "6.Adjustments", range = "A1:M11")
  #Pivoting Adjusted_Multiplier table to make graphing easier.
  Adjusted_Multiplier <- Adjusted_Multiplier %>%
    pivot_longer(cols = `2021`:`2032`, names_to = "Year",
                 values_to ="Multipliers",
                 names_repair = "minimal")
  #Rounding figures for Adjusted_Multiplier table.
  Adjusted_Multiplier$Multipliers <- round(Adjusted_Multiplier$Multipliers,
                                       digits = 2)

 Adjusted_Multiplier <- Adjusted_Multiplier %>% rename(Category = "Emissions") 
   
 #Level of behavioural change (LOBC)
 Lobc_Multipliers <- read_excel("Project_Figures.xlsx",
                                sheet = "2. Lobc", range = "A16:M26")
 
 #Pivoting Lobc_Multiplier table to make graphing easier.
 Lobc_Multipliers <- Lobc_Multipliers %>%
   pivot_longer(cols = `2021`:`2032`, names_to = "Year",
                values_to ="Lobc_Multiplier",
                names_repair = "minimal")
 #Rounding figures for Lobc_Multipliers table.
 Lobc_Multipliers$Lobc_Multiplier <- round(Lobc_Multipliers$Lobc_Multiplier,
                                           digits = 2)
 
 #Electricity Multiplier Table
 Electricity_Multipliers <- read_excel("Project_Figures.xlsx",
                                       sheet = "3. Electricity renewables %",
                                       range = "A7:M17")
 
 #Pivoting Electricity_Multipliers table to make graphing easier.
 Electricity_Multipliers <- Electricity_Multipliers %>%
   pivot_longer(cols = `2021`:`2032`, names_to = "Year",
                values_to ="Electricity_Multiplier",
                names_repair = "minimal")
 
 #Rounding figures for Electricity_Multipliers table.
 Electricity_Multipliers$Electricity_Multiplier <- round(Electricity_Multipliers$Electricity_Multiplier,
                                                         digits = 2)
 
 Electricity_Multipliers <- Electricity_Multipliers %>% rename(Category = "Emissions")
 
 #Scenarios foe electricity
 Scenarios <- read_excel("Project_Figures.xlsx",
                         sheet = "3. Electricity renewables %",
                         range = "A59:M64")
 
 #Pivoting Scenarios table to make graphing easier.
 Scenarios <- Scenarios %>%
   pivot_longer(cols = `2021`:`2032`, names_to = "Year",
                values_to ="Scenario_E",
                names_repair = "minimal")
 
 #Rounding figures for Electricity_Multipliers table.
 Scenarios$Scenario_E <- round(Scenarios$Scenario_E,
                               digits = 2)
 
 # Scenarios <- Scenarios %>%
 #   pivot_wider(names_from = Scenarios, values_from = Scenario_E)
 
 help("pivot_wider")
 
#The table for question one with the multiplied totals
The_Complete_Table <- left_join(Base_Scenario, Adjusted_Multiplier,
          by = c("Category", "Year"), keep = FALSE)
  
#The table for question two with the multiplied totals
The_Complete_Table <- left_join(The_Complete_Table, Lobc_Multipliers,
                                by = c("Category", "Year"), keep = FALSE) 

#The table for question three with the multiplied totals
The_Complete_Table <- left_join(The_Complete_Table, Electricity_Multipliers,
                                by = c("Category", "Year"), keep = FALSE)

  # #Pulls out Emissions category so we can use it in future function calls
  # BaseEmissions <- function(Category, TheYear){
  #   (Base_Scenario %>% filter(Emissions == Category, Year == TheYear) %>% select(Carbon_Emissions))$Carbon_Emissions
  # }
  # BaseEmissions( "Staff Air Travel - domestic and international",  2022)
  # 
  # #Pulls out multiplier so we can use it in future function calls
  # BaseMultiplier <- function(Category, TheYear){
  #   (Adjusted_Multiplier %>% filter(Emissions == Category, Year == TheYear) %>% select(Multipliers))$Multipliers
  #   #returns multiplier
  # }
  # BaseMultiplier("Staff Air Travel - domestic and international",  2022)
  

#NEW FUNCTION, REACTIVE GRAPH
   new_scenario <- reactive({
     req(input$ElectrictySlider)
   #         if (input$ElectricitySlider == 5){
   #                    result <- Scenarios$Scenario_5 * The_Complete_Table$Electricity_Multiplier
   #                  }else if (input$ElectricitySlider == 4){
   #                    result <- Scenarios$Scenario_4 * The_Complete_Table$Electricity_Multiplier
   #                  }else if (input$ElectricitySlider == 3){
   #                    result <- Scenarios$Scenario_3 * The_Complete_Table$Electricity_Multiplier
   #                  }else if (input$ElectricitySlider == 2){
   #                    result <- Scenarios$Scenario_2 * The_Complete_Table$Electricity_Multiplier
   #                  }else if (input$ElectricitySlider == 1){
   #                    result <- Scenarios$Scenario_1 * The_Complete_Table$Electricity_Multiplier
   #                  }else{
   #                    result <- 0
   #                  }
   })
    
    output$plot <- renderPlot({
  # req(new_scenario())
       Base_Scenario_Graph <- The_Complete_Table %>%
         mutate(Total_Emissions = Carbon_Emissions * (1 + (Multipliers * input$StudentSlider) 
                                                      + (Lobc_Multiplier * input$BehaviourSlider))) %>% 
         ggplot() +
         geom_col(aes(x = Year, y = Total_Emissions, fill = Category),
                  position = position_stack(reverse = TRUE), na.rm = TRUE,
                  color="black") +
         theme(legend.position="right") +
         guides(fill = guide_legend(reverse = TRUE)) +
         ylab("CO2 Emissions (Tonnes)") +
         ylim(0, 50000)
       Base_Scenario_Graph
    })

}

# download feature
# output$ExportPlot <- downloadHandler(
# file name
# filename <- 'plot.png',
# content
# content = function(file){
# create plot
# export(p = thePlot(), file = 'tempPlot.png')
# hand over the file
# file.copy('tempPlot.png',file)
# }
# )

# Run the application

shinyApp(ui = ui, server = server)

