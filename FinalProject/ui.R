library(shiny)
library(shinydashboard)

# Load the year CSV file
year_df <- read.csv(file="data/year_df.csv", head=TRUE,  sep=",", stringsAsFactors = FALSE)
occupation_df <- read.csv(file="data/occupation_df.csv", head=TRUE,  sep=",", stringsAsFactors = FALSE)
salary_total_df <- read.csv(file="data/salary_total_df.csv", head=TRUE,  sep=",", stringsAsFactors = FALSE)
title_df <- read.csv(file="data/title_df.csv", head=TRUE,  sep=",", stringsAsFactors = FALSE)

# Top Occupations
# Input for Tab 1- Top Occupations - All
yearInput1 <- selectInput("year1", "Year :", choices = year_df$YEAR_VALUE, selected = "2015", width="100")

# Input for Tab 2 - Top Occupations/Category
yearInput2 <- selectInput("year2", "Year :", choices = year_df$YEAR_VALUE, selected = "2015", width="100")
occupationInput2 <- selectInput("occupationInput2", "Occupation :", choices = setNames(occupation_df$OCC_CODE_SHORT, occupation_df$TITLE), width="400")

# Input for Tab 3 - Salary History/Title
titleInput3 <- selectInput("titleInput3", "Occupation Title :", choices = setNames(title_df$OCC_CODE, title_df$TITLE), selected = "2015", width="500")

chartPanel1 <- htmlOutput("chartPanel1")
chartPanel2 <- htmlOutput("chartPanel2")
chartPanel3 <- htmlOutput("chartPanel3")

outerPanel1 <- verticalLayout(mainPanel(yearInput1), chartPanel1)
outerPanel2 <- verticalLayout(mainPanel(yearInput2, occupationInput2), chartPanel2)
outerPanel3 <- verticalLayout(mainPanel(titleInput3), chartPanel3)

dashboardPage(
 dashboardHeader(title = "DATA 608 Final Project - Occupation vs Salary", titleWidth = "100%"),
 dashboardSidebar(disable = T),
 dashboardBody(
   box(width=100,
       tabBox(width=12,id="tabBox_next_previous",
              tabPanel("Top Occupations - All",outerPanel1),
              tabPanel("Top Occupations/Category",outerPanel2),
              tabPanel("Salary History/Occupation",outerPanel3)
              )
   )
  )
)