library(shiny)
library(shinydashboard)
library(googleVis)
library(dplyr)

# Load the salary data from the CSV file
salary_total_df <- read.csv(file="data/salary_total_df.csv", head=TRUE,  sep=",", stringsAsFactors = FALSE)

shinyServer(function(input, output,session) {
  
  getSalaryData1 <- reactive({ 
    #print("--------------in getSalaryData1() function----------------")
    #print(input$year1)
    temp_df <- filter(salary_total_df, YEAR == input$year1)
    temp_df <- arrange(temp_df, desc(SALARY))
    #print(nrow(temp_df))
    return (head(temp_df, 30))
  })
  
  
  getSalaryData2 <- reactive({
    #print("--------------in getSalaryData2() function----------------")
    #print(input$year2)
    #print(input$occupationInput2)
    temp1_df <- filter(salary_total_df, YEAR == input$year2 & OCC_CODE_SHORT == input$occupationInput2 & OCC_GROUP != "major")
    temp1_df <- arrange(temp1_df, desc(SALARY))
    #print(temp1_df)
  })
  
  getSalaryData3 <- reactive({
    #print("--------------in getSalaryData3() function----------------")
    #print(input$occupationInput3)
    temp1_df <- filter(salary_total_df, OCC_CODE_SHORT == input$occupationInput3 & OCC_GROUP != "major")
    temp1_df <- arrange(temp1_df, TITLE)
    #print(temp1_df)
    return (temp1_df)
  })
  
  getSalaryData4 <- reactive({
    #print("--------------in getSalaryData4() function ----------------")
    #print(c("OCC_CODE_SHORT", "TITLE"))
    #print(c(input$occupationInput3, input$titleInput3))
    
    selectedTitle = input$titleInput3
    occCodeShort <- input$occupationInput3
    
    if (is.null(input$titleInput3)) {
        if (occCodeShort == "11") {
          selectedTitle = "11_3011"		
        } else if (occCodeShort == "13") {
          selectedTitle = "13_2011"
        } else if (occCodeShort == "15") {
          selectedTitle = "15_2011"
        } else if (occCodeShort == "17") {
          selectedTitle = "17_3021"
        } else if (occCodeShort == "19") {
          selectedTitle = "19_4011"
        } else if (occCodeShort == "21") {
          selectedTitle = "21_1021"
        } else if (occCodeShort == "23") {
          selectedTitle = "23_1021"
        } else if (occCodeShort == "25") {
          selectedTitle = "25_3011"
        } else if (occCodeShort == "27") {
          selectedTitle = "27_2011"
        } else if (occCodeShort == "29") {
          selectedTitle = "29_1061"
        } else if (occCodeShort == "31") {
          selectedTitle = "31_9091"
        } else if (occCodeShort == "33") {
          selectedTitle = "33_9011"
        } else if (occCodeShort == "35") {
          selectedTitle = "35_3011"
        } else if (occCodeShort == "37") {
          selectedTitle = "37_2019"
        } else if (occCodeShort == "39") {
          selectedTitle = "39_3091"
        } else if (occCodeShort == "41") {
          selectedTitle = "41_3011"
        } else if (occCodeShort == "43") {
          selectedTitle = "43_4199"
        } else if (occCodeShort == "45") {
          selectedTitle = "45_2091"
        } else if (occCodeShort == "47") {
          selectedTitle = "47_2011"
        } else if (occCodeShort == "49") {
          selectedTitle = "49_3011"
        } else if (occCodeShort == "51") {
          selectedTitle = "51_9191"
        } else if (occCodeShort == "53") {
          selectedTitle = "53_2021"
       }
    } else {
        selectedTitle = input$titleInput3
    }
    
    temp1_df <- filter(salary_total_df, OCC_CODE_SHORT == input$occupationInput3 & OCC_CODE == selectedTitle & OCC_GROUP != "major")
    #temp1_df <- filter(salary_total_df, OCC_CODE_SHORT == input$occupationInput3 & OCC_GROUP != "major")
    temp1_df <- select(temp1_df, matches("YEAR|SALARY"))
    temp1_df <- temp1_df[c("YEAR", "SALARY")]
    #print(nrow(temp1_df))
    #print(temp1_df)
    return (temp1_df)
  })
  
  
  getSalaryData5 <- reactive({
    #print("--------------in getSalaryData5() function----------------")
    #print(input$titleInput3)
    temp1_df <- filter(salary_total_df, OCC_CODE == input$titleInput3 & OCC_GROUP != "major")
    temp1_df <- temp1_df[c("YEAR", "SALARY")]
    temp1_df <- arrange(temp1_df, YEAR)
    #print(temp1_df)
  })
  
  
  # The googleVis bar chart rendition
  output$chartPanel1 <- renderGvis({gvisBarChart(getSalaryData1(), xvar="TITLE", yvar=c("SALARY"),
                                                   options=list(seriesType='bars',
                                                                width="100%", height="700", 
                                                                title="Occupation vs. Salary",
                                                                colors="['#00ff00']",
                                                                vAxis="{textStyle: {fontSize: '10'}}",
                                                                hAxis="{title:'Annual Salary'}",
                                                                vAxis="{title:'Occupation'}",
                                                                chartArea="{top:'20'}",
                                                                axes="{x: {0: {side: 'top', label: 'Annual Salary'}}}"))})
  
  output$chartPanel2 <- renderGvis({gvisBarChart(getSalaryData2(), xvar="TITLE", yvar=c("SALARY"),
                                                    options=list(seriesType='bars',
                                                                 width="100%", height="700", 
                                                                 title="Occupation vs. Salary",
                                                                 colors="['#ff0000']",
                                                                 vAxis="{textStyle: {fontSize: '10'}}",
                                                                 hAxis="{title:'Annual Salary'}",
                                                                 vAxis="{title:'Occupation'}",
                                                                 chartArea="{top:'20'}",
                                                                 axes="{x: {0: {side: 'top', label: 'Annual Salary'}}}"))})
  
  #output$titleInput3 <- renderUI({selectInput("titleInput3", "Title :", 
  #                                            choices = setNames(getSalaryData3()$OCC_CODE, getSalaryData3()$TITLE), 
  #                                            selected = getSalaryData3()$TITLE[1], width="400")
  #})

  #output$chartPanel3 <- renderGvis({gvisLineChart(getSalaryData4(), 
  #                                                options=list(
  #                                                    colors="['#008080']",
  #                                                    hAxis="{title:'Year'}",
  #                                                    vAxis="{title:'Annual Salary'}",
  #                                                    pointSize=8,
  #                                                    pointShape='circle',
  #                                                    height=600, width=900))})
  
  
  
  output$chartPanel3 <- renderGvis({gvisLineChart(getSalaryData5(), 
                                                  options=list(
                                                    colors="['#0000ff']",
                                                    hAxis="{title:'Year'}",
                                                    vAxis="{title:'Annual Salary'}",
                                                    pointSize=8,
                                                    pointShape='circle',
                                                    height=550, width=900))})
})