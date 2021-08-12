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
library(shinyBS)


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  tags$br(),
  tags$img(src = "https://www.otago.ac.nz/_assets/_gfx/logo@2x.png", width = "160px", height = "80px"),
  titlePanel(tags$strong("GHG Emissions Pathway Tool")),
  style = "font-family: 'Open Sans', sans-serif;",
  tags$br(),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      width = 4,
      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #00508F}")),
      sliderInput("StudentSlider",
                  tags$h3("Student numbers"),
                  min = 0,
                  max = 1,
                  value = 0,
                  step = 0.10),
      bsPopover("StudentSlider", "Percentage change", "Growth or decline in Equivalent Full Time Students", trigger = "hover"),

      tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #00508F}")),
      sliderInput("BehaviourSlider",
                  tags$h3("Behavioural change"),
                  min = -2,
                  max = +2,
                  value = 0),
      bsPopover("BehaviourSlider", "Level of change", "Conscious degree of motivation in their personal contributions towards GHG reductions", trigger = "hover",
                options = NULL),
      tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: 	#00508F}")),
      sliderInput("ElectricitySlider",
                  tags$h3("NZ electricity grid"),
                  min = 1,
                  max = 5,
                  value = 1),
      bsPopover("ElectricitySlider", "Rate of conversion", "The rate at which NZ power grid changes to renewable energy sources", trigger = "hover",
                options = NULL)
    ),
   
    # Show a plot of the generated distribution
    mainPanel(
      tags$div(
        tags$p(tags$text(tags$strong("You can use this dashboard to change the high-level factors and easily understand their 
                    impacts on the University's greenhouse gas emissions up to year 2032."))),
        tags$style("text {color: #00508F;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
        )
      ),
      
      plotOutput("plot"),

      tags$br(),
      
      tags$div(
        
        textOutput("text1"),
        textOutput("text2"),
        tags$style("#text1 {color: black;
                                 font-size: 16px;
                                 }"
        ),
        tags$style("#text2 {color: black;
                                 font-size:16px;
                                 }"
        )
        
      ),
      
      tags$div(
        tags$p(tags$h3("Further information")),
        style = "font-family: 'Open Sans', sans-serif; font-size: 16px; font-weight: normal; line-height: 1.8;",
        tags$p(tags$a(href="https://www.otago.ac.nz/sustainability/about/", "University of Otago's Sustainability Office")),
        tags$p(tags$a(href="https://www.otago.ac.nz/sustainability/news/otago828588.html", "University of Otago makes submission to ORC's draft 10 year plan")),
        tags$p(tags$a(href="https://www.otago.ac.nz/sustainability/otago824241.pdf", "University of Otago's 2019 Greenhouse Gas Inventory"))
      ),
      tags$style("p {color: blue;
                                 font-size: 16px;
                                 font-style: normal;
                                 }"
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
 

 
 
#Scenarios table for electricity slider.
Scenarios <- read_excel("Project_Figures.xlsx",
                         sheet = "3. Electricity renewables %",
                         range = "A59:M64")
#pivoting Scenarios table for later use.
Scenarios <- Scenarios %>% 
  pivot_longer(cols = `2021`:`2032`, names_to = "Year",
                values_to ="Scenario_E",
                names_repair = "minimal")
<<<<<<< HEAD
#Rounding figures for Electricity_Multipliers table.
Scenarios$Scenario_E <- round(Scenarios$Scenario_E,
                              digits = 2)
#Pivot wider to get data ready for table merge
Scenarios <- Scenarios %>% 
  pivot_wider(names_from = Scenarios, values_from = Scenario_E)



=======
 
 #Rounding figures for Electricity_Multipliers table.
 Scenarios$Scenario_E <- round(Scenarios$Scenario_E,
                               digits = 2)
 
  Scenarios <- Scenarios %>%
    pivot_wider(names_from = Scenarios, values_from = Scenario_E)
 
>>>>>>> 11cac40001693074a9860b8fb5a412ceeb233802
#The table for question one with the multiplied totals
The_Complete_Table <- left_join(Base_Scenario, Adjusted_Multiplier,
          by = c("Category", "Year"), keep = FALSE)
  
#The table for question two with the multiplied totals
The_Complete_Table <- left_join(The_Complete_Table, Lobc_Multipliers,
                                by = c("Category", "Year"), keep = FALSE) 

#The table for question three with the multiplied totals
The_Complete_Table <- left_join(The_Complete_Table, Electricity_Multipliers,
                                by = c("Category", "Year"), keep = FALSE)

#The table for question three with the multiplied totals
The_Complete_Table <- left_join(The_Complete_Table, Scenarios,
                                by =  "Year", keep = FALSE)
<<<<<<< HEAD
rm(The_Complete_Table)  
help("geom_col")
=======
  

>>>>>>> 11cac40001693074a9860b8fb5a412ceeb233802
#NEW FUNCTION, REACTIVE GRAPH
   # new_scenario <- reactive({
   #   req(input$ElectrictySlider)
   #         case_when (input$ElectricitySlider == 5 ~  Scenario_5 * Electricity_Multiplier,
   #                  input$ElectricitySlider == 4 ~ Scenario_4 * Electricity_Multiplier,
   #                  input$ElectricitySlider == 3 ~  Scenario_3 * Electricity_Multiplier,
   #                  input$ElectricitySlider == 2 ~  Scenario_2 * Electricity_Multiplier,
   #                  input$ElectricitySlider == 1 ~ Scenario_1 * Electricity_Multiplier)
   # })
   # 
   # help(case_when)
#  new_scenario <- function(i)({
#    The_Complete_Table %>%
#    if (i == 5){
#      result = Scenario_5 * Electricity_Multiplier
#    }else if (i == 4){
#      result = Scenario_4 * Electricity_Multiplier
#    }else if (i == 3){
#      result = Scenario_3 * Electricity_Multiplier
#    }else if (i == 2){
#      result = Scenario_2 * Electricity_Multiplier
#    }else if (i == 1){
#      result = Scenario_1 * Electricity_Multiplier
#    }else{
#      result = 0
#    }
#    return(result)
# })
#  new_scenario(5)
    
    output$plot <- renderPlot({
  # req(new_scenario())
       Base_Scenario_Graph <- The_Complete_Table %>%
         mutate(Total_Emissions = Carbon_Emissions * (1 + (Multipliers * input$StudentSlider) 
                                                      + (Lobc_Multiplier * input$BehaviourSlider)
                                                      + (case_when (input$ElectricitySlider == 5 ~  Scenario_5 * Electricity_Multiplier,
                                                                    input$ElectricitySlider == 4 ~ Scenario_4 * Electricity_Multiplier,
                                                                    input$ElectricitySlider == 3 ~  Scenario_3 * Electricity_Multiplier,
                                                                    input$ElectricitySlider == 2 ~  Scenario_2 * Electricity_Multiplier,
                                                                    input$ElectricitySlider == 1 ~ Scenario_1 * Electricity_Multiplier)))) %>% 
         ggplot() +
         geom_col(aes(x = Year, y = Total_Emissions, fill = Category),
                  position = position_stack(reverse = TRUE), na.rm = TRUE,
                  color="black") +
         theme(legend.position="right") +
         guides(fill = guide_legend(reverse = TRUE)) +
         ylab("CO2 Emissions (Tonnes)") +
         ylim(0, 50000) +
         geom_hline(yintercept=20000, linetype="dashed", color = "black")
       Base_Scenario_Graph
    })

    
    output$text1 <- renderText({paste("Based on your selected inputs: " , input$StudentSlider , "percentage change in student numbers, " , 
                                      input$BehaviourSlider , "level of behavioural change, " ,
                                      input$ElectricitySlider , "times rate of conversion from electricity to renewable energy, " ,
                                      "XX" , "hectares of trees would 
                                      need to be planted by year 2025 in order to reach net zero emissions in 2030.")})
    
    
    output$text2 <- renderText({paste("To buy carbon credits from the market to offset current emissions, " ,
                                      "the cost would be " , "$" , "XX", ".")})
    
    
}

# Run the application

shinyApp(ui = ui, server = server)

