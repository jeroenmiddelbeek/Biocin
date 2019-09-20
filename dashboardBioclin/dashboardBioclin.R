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
    dashboardHeader(
        title = "Bioclin dataanalysis"),
    
  # sidebar
    dashboardSidebar( 
      fileInput(inputId = "parameters", label = h4("Insert parameters file (xlsx)"), multiple = FALSE),
      fileInput(inputId = "results", label = h4("Insert results file (CSV)"), multiple = FALSE),
      sliderInput(inputId = "area", label = h4("Set area cut-off (0-1000)"), value = 10, min = 0, max = 10000),
      width = 250
      ), #waarom werkt komma gebruik in shiny anders dan in markdown?
    
  # body  
    dashboardBody(
     
     box(
       title = "Data",
       width = 12,
       solidHeader = TRUE,
       status = "primary",
       dataTableOutput('tblMerge')
     ),
    
    box (
      title = "count",
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      plotOutput("plot")
    ) 
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #read data
  dataResults <- reactive({
    df_results <- input$results[1,4]#wat doe ik hier?
    if (!is.null(df_results))#wat doe ik hier?
    readr::read_csv(df_results, col_names = TRUE)%>%
    separate(Label, into = c("exp_id" , "plate_id" , "well_id" , "image_rep"), sep="_")%>%
    select (X1, well_id, image_rep, Area)%>%
    dplyr::filter(Area > input$area)
  })

  dataParameters <- reactive({
    workbook <- input$parameters[1,4]
    if (!is.null(workbook))
    workbook <- readxl::excel_sheets(workbook)

    df_parameters <- input$parameters[1,4]
    if (!is.null(df_parameters))
      readxl::read_xlsx(df_parameters, sheet = workbook[2]) %>%
      tidyr::unite(compound, "comp_name", "comp_id", "comp_trtmnt", sep = "-") %>%
      dplyr::mutate(plate_id = as.numeric(plate_id))
  })

  dataMerge <- reactive({
       dplyr::left_join(dataResults(), dataParameters(), by = c("well_id")) %>%
       dplyr::mutate(od_factor = as.factor(od)) %>%
       dplyr::mutate(comp_dil_factor = as.factor(comp_dil)) %>%
       dplyr::mutate(image_rep = as.factor(image_rep)) %>%
       dplyr::mutate(well_rep = as.factor(well_rep)) 
  })
   
  sumImageRep <- reactive({
    dataMerge() %>%
     group_by(od, comp_dil, compound, well_rep, image_rep) %>%
     summarize( 
       count = n(),
       mean_area = mean(Area, na.rm=TRUE),
       sd_area = sd(Area, na.rm=TRUE)
     )
  })
    
  output$tblMerge <- renderDataTable(dataMerge(), 
                                          options = list(scrollX = TRUE,
                                                         pageLength = 5,
                                                         searching = FALSE))
  
  output$plot  <- renderPlot({
    ggplot(data = sumImageRep(), mapping = aes(x = od, y = count)) +
      geom_line(
        mapping = aes(color = image_rep)) +
      ggtitle("Particle Count") +
      facet_grid(compound ~ well_rep, scale = "fixed")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
