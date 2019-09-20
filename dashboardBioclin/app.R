#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)

options(shiny.maxRequestSize=50*1024^2)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  # Application title
    dashboardHeader(title = "Bioclin dataanalysis"),
    
  # sidebar
    dashboardSidebar( 
      fileInput(inputId = "parameters", label = h4("Insert parameters file (xlsx)"), multiple = FALSE),
      fileInput(inputId = "results", label = h4("Insert results file (CSV)"), multiple = FALSE),
      width = 250
      ),
    
  # body  
    dashboardBody(
      box(
        title = "Results",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
       dataTableOutput('tbl')
     )
    )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #read data
  data <- reactive({
    df_results <- input$results[1,4]#wat doe ik hier?
    if (!is.null(df_results))#wat doe ik hier?
    readr::read_csv(df_results, col_names = TRUE)%>%
    separate(Label, into = c("exp_id" , "plate_id" , "well_id" , "image_rep"), sep="_")
  })
  
  

  output$tbl <- renderDataTable(data(),
                               options = list(scrollX = TRUE,
                                              pageLength = 10,
                                              searching = FALSE))
  
  
   
}

# Run the application 

shinyApp(ui = ui, server = server)
