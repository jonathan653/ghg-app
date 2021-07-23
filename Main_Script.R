#All the package installs for RSQLite database, reading xls files
#and tools needed.
install.packages("RSQLite")
library(RSQLite)
install.packages('tidyverse')
library(tidyverse)
install.packages('Rcpp')
library(Rcpp)

library(DBI)
library(readxl)

#Read spreadsheet into R
Climate_Data_Table <- read_excel("Net Carbon Zero and impact of different variables July 2021 - Copy.xlsx")

#Connecting database created in SQLite.
mydb <- dbConnect(RSQLite::SQLite(), "Climate_Data.sqlite")

#Writing Climate_Data_Table into Climate_Data database in SQLite.
dbWriteTable(mydb, "Climate_Data_Table", Climate_Data_Table)
dbListTables(mydb)
mydb

#PROBLEMS/QUESTIONS:

#1. Can't find table in database in SQLite. 

#2. How does the SQLite database work when this script is opened and run on
#another device?

#3. 