#All the package installs for project.
install.packages('Rcpp')
install.packages('tidyverse')
install.packages("RSQLite")
library(Rcpp)
library(tidyverse)
library(DBI)
library(readxl)
library(ggplot2)
library(RSQLite)


#Connecting database created in SQLite.
#mydb <- dbConnect(RSQLite::SQLite(), "Climate_Data.sqlite")

#Writing Climate_Data_Table into Climate_Data database in SQLite.
#dbWriteTable(mydb, "Climate_Data_Table", Climate_Data_Table, overwrite = TRUE)

#Check for list of tables in DB.
#dbListTables(mydb)





#BASE SCENARIO
#Read spreadsheet for base scenario into R.
Base_Scenario <- read_excel("Project_Figures.xlsx",
                            sheet = "4. Total base scenario", range = "A1:M11")
#Pivoting base scenario table to make graphing easier.
Base_Scenario <- Base_Scenario %>% 
  pivot_longer(cols = `2021`:`2032`, names_to = "Year",
               values_to ="Carbon_Emissions",
               names_repair = "minimal")
#Rounding figures for base scenario table.
Base_Scenario$Carbon_Emissions <- round(Base_Scenario$Carbon_Emissions,
                                        digits = 2)
#Sorting the base scenario table for easier graphing.
Base_Scenario <- Base_Scenario %>%
  group_by(Year) %>% 
  arrange(Year)

Base_Scenario$Year <- as.numeric(Base_Scenario$Year)
typeof("Year")


Base_Scenario[0,1]


#STUDENT NUMBERS
#Read spreadsheet for variable 1 into R.
Student_Numbers <- read_excel("Project_Figures.xlsx",
                              sheet = "1. Students %", range = "A1:M8")
#Pivoting Student_Numbers table to make graphing easier.
Student_Numbers <- Student_Numbers %>% 
  pivot_longer(cols = `2021`:`2032`, names_to = "Year",
               values_to ="Carbon_Emissions",
               names_repair = "minimal")
#Rounding figures for Student_Numbers table.
Student_Numbers$Carbon_Emissions <- round(Student_Numbers$Carbon_Emissions,
                                        digits = 2)
#Sorting the Student_Numbers table for easier future graphing or manipulation.
Student_Numbers <- Student_Numbers %>%
  group_by(Year) %>% 
  arrange(Year)





#LEVEL OF BEHAVIOURAL CHANGE
#Read spreadsheet for variable 2 into R.
Lobc <- read_excel("Project_Figures.xlsx",
                   sheet = "2. Lobc", range = "A1:M7")
#Pivoting Lobc table to make graphing easier.
Lobc <- Lobc %>% 
  pivot_longer(cols = `2021`:`2032`, names_to = "Year",
               values_to ="Carbon_Emissions",
               names_repair = "minimal")
#Rounding figures for Lobc table.
Lobc$Carbon_Emissions <- round(Lobc$Carbon_Emissions,
                                          digits = 2)
#Sorting the Student_Numbers table for easier future graphing or manipulation.
Lobc <- Lobc %>%
  group_by(Year) %>% 
  arrange(Year)





#ELECTRICITY RENEWABLES PERCENTAGE
#Read spreadsheet for variable 3 into R.
Electricity_Renewables <- read_excel("Project_Figures.xlsx",
                                     sheet = "3. Electricity renewables %",
                                     range = "A1:M4")
#Pivoting Electricity_Renewables table to make graphing easier.
Electricity_Renewables <- Electricity_Renewables %>% 
  pivot_longer(cols = `2021`:`2032`, names_to = "Year",
               values_to ="Carbon_Emissions",
               names_repair = "minimal")
#Rounding figures for Electricity_Renewables table.
Electricity_Renewables$Carbon_Emissions <- round(Electricity_Renewables$Carbon_Emissions,
                               digits = 2)
#Sorting the Electricity_Renewables table for easier future graphing or manipulation.
Electricity_Renewables <- Electricity_Renewables %>%
  group_by(Year) %>% 
  arrange(Year)





#CARBON OFFSETTING
#Read in spreadsheet for carbon offsetting total into R.
Carbon_Offsets <- read_excel("Project_Figures.xlsx",
                             sheet = "5.Carbon offsetting", range = "A1:M2")
#Pivoting Carbon_Offsets table to make graphing easier.
Carbon_Offsets <- Carbon_Offsets %>% 
  pivot_longer(cols = `2021`:`2032`, names_to = "Year",
               values_to ="Offsets",
               names_repair = "minimal")
#Rounding figures for Carbon_Offsets table.
Carbon_Offsets$Offsets <- round(Carbon_Offsets$Offsets,
                                        digits = 2)

#Adjusted_Base_scenario
Adjusted_Multiplier <- read_excel("Project_Figures.xlsx", 
                                  +     sheet = "6. Modelling BS numbers", range = "A1:M11")
#Pivoting Adjusted_Multiplier table to make graphing easier.
Adjusted_Multiplier <- Adjusted_Multiplier %>% 
  pivot_longer(cols = `2021`:`2032`, names_to = "Year",
               values_to ="Offsets",
               names_repair = "minimal")
#Rounding figures for Adjusted_Multiplier table.
Adjusted_Multiplier$Offsets <- round(Adjusted_Multiplier$Offsets,
                                digits = 2)

#setting the legend order for the graph
bar_order <- list(
  "`Staff Air Travel - domestic and international`" = 9,
  "`Student air travel - domestic and international`" = 8,
  "`Steam & MTHW - coal (incl losses)`" = 7,
  "`Electricity (incl transmission losses)`" = 6,
  "`Purchased Goods and Services - Food`" = 5,
  "`Waste from operations - to landfill, recycling and water processing`" = 4,
  "`Stationary Combustion - coal`" = 3,
  "`Employee Commuting - Private vehicles`" = 2,
  "`Stationary Combustion - LPG`" = 1,
  "`Other`" = 0
)
bar_order_v <- as.numeric(bar_order)
#Graphing the base scenario.
Base_Scenario_Graph <- Base_Scenario %>% 
  ggplot() +
  geom_col(aes(x = Year, y = Carbon_Emissions, fill = Emissions),
           position = "stack", na.rm = TRUE) +
  theme(legend.position="right") +
  
Base_Scenario_Graph

#Trying to overlay the carbon offsets on the base graph.
Base_Scenario_Test <- Base_Scenario %>% 
  ggplot(aes(x = Year, y = Carbon_Emissions, fill = Emissions)) +
  geom_col(position = "stack", na.rm = TRUE) +
  theme(legend.position="right") + 
  geom_col(data = Carbon_Offsets,
           aes(x = Year, y = Offsets, fill = NA))
Base_Scenario_Test



#density chart attempt
Carbon_Offsets %>% 
  ggplot(aes(Carbon_Emissions)) +
  geom_density(aes(fill = factor(Emissions), position = "stack"))


function(Base_Scenario, Emit){
  if (Year <= 2019 && Year <= 2021){
    
  }else if( Year == 2022){
    'Staff Air Travel - domestic and international'[0,11]* (1 + (0.025 * Emit))
  }else{
    'Staff Air Travel - domestic and international'[0,21] * (1 + (0.075 * Emit))
  }
}

Base_Scenario %>% 
  summarise('Staff Air Travel - domestic and international'[,1])

help()
Base_Scenario %>% 