# call libraries
library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(DT)
library(ggridges)

# set max file size
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
      
      box(
        style = "position: fixed",
        sliderInput(inputId = "area", label = h4("Set area cut-off (0-1000)"), value = 0, min = 0, max = 1000)
        ),
        
      box(
        #style = "position: fixed",
        title = "Number of records",
        background = "black",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        textOutput('numberUnfilteredRecords'),
        textOutput('numberFilteredRecords')
        ),
      
      width = 250
  ), 

  # body  
  dashboardBody(
    
    box(
      title = "Select Parameters",
      width = 12,
      #collapsible = TRUE, 
      #collapsed = TRUE,
      #background = "navy",
      solidHeader = TRUE,
      status = "primary",
      fluidRow(
        box (title = "Plate", width = 2, status = "warning", uiOutput(outputId = "checkboxPlate")),
        box (title = "Strain", width = 2, status = "warning", uiOutput(outputId = "checkboxStrain")),
        box (title = "Compound", width = 2, status = "warning", uiOutput(outputId = "checkboxCompound")),
        box (title = "Od", width = 2, status = "warning", uiOutput(outputId = "checkboxOd")),
        box (title = "Dye", width = 2, status = "warning", uiOutput(outputId = "checkboxDye")),
        box (title = "Compound dilution", width = 2, status = "warning", uiOutput(outputId = "checkboxCompDil")),
        box (title = "Image repeat"), width = 2, status ="warning", uiOutput(outputId = "checkboxImageRepeat")
        )
      ),
    
    box(
      title = "Filtered Data",
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      DT::dataTableOutput('tblDataMergeFiltered')
      ),
    
    box(
      selectInput(inputId = "axisX", 
                  label = "",
                  choices  = c("comp_dil", "od"),
                  selected = "comp_dil"), 
      
      checkboxInput(inputId = "log10transform",
                    label = "Log10 Transform",
                    value = TRUE)
      
      ),
   
    box (
      title = "Count Particles",
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      plotOutput("countPlot")
      ),
    
    box (
      title = "Density",
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      plotOutput("densityPlot")
      ),
    
    box (
      title = "Particles",
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      plotOutput("particlePlot")
    )
    
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #read & merge datafiles
  dataResults <- reactive({
    df_results <- input$results[1,4]#wat doe ik hier?
    if (!is.null(df_results))
    readr::read_csv(df_results, col_names = TRUE)%>%
    separate(Label, into = c("exp_id" , "plate_id" , "well_id" , "image_rep"), sep="_")%>%
    select (X1, well_id, image_rep, Area) 
    })

  dataParameters <- reactive({
    workbook <- input$parameters[1,4]
    if (!is.null(workbook))
    workbook <- readxl::excel_sheets(workbook)

    df_parameters <- input$parameters[1,4]
    if (!is.null(df_parameters))
      readxl::read_xlsx(df_parameters, sheet = workbook[2]) %>%
      tidyr::unite(compound, "comp_name", "comp_id", "comp_trtmnt", sep = "-") %>%
      dplyr::mutate(plate_id = as.numeric(plate_id)) %>%
      dplyr::mutate(comp_dil = ifelse(is.na(comp_dil), 0, comp_dil)) 
    })

  dataMerge <- reactive({
      dplyr::left_join(dataResults(), dataParameters(), by = c("well_id")) %>%
      dplyr::mutate(od = as.numeric(od)) %>%
      dplyr::mutate(od_factor = as.factor(od)) %>%
      dplyr::mutate(comp_dil = as.numeric(comp_dil)) %>%
      dplyr::mutate(comp_dil_factor = as.factor(comp_dil)) %>%
      dplyr::mutate(image_rep = as.factor(image_rep)) %>%
      dplyr::mutate(well_rep = as.factor(well_rep)) 
    })
  
  #filter dataMerge
  output$checkboxPlate <- renderUI({
    typePlateId <- reactive({
      dataPlateId <- unique(dataMerge()$plate_id)
    })
    
    checkboxGroupInput(inputId = "filterPlateId", label = "", 
                       choices  = typePlateId(), #yeaaaaaaaah --> let op de ()
                       selected = typePlateId())
  }) 
  
  output$checkboxStrain <- renderUI({
      typeStrain <- reactive({
      dataStrain <- unique(dataMerge()$strain)
    })
    
    checkboxGroupInput(inputId = "filterStrain", label = "", 
                       choices  = typeStrain(), #yeaaaaaaaah --> let op de ()
                       selected = typeStrain())
  })
  
  output$checkboxCompound <- renderUI({
    typeCompound <- reactive({
      dataCompound <- unique(dataMerge()$compound)
    })
    
    checkboxGroupInput(inputId = "filterCompound", label = "", 
                       choices  = typeCompound(), 
                       selected = typeCompound())
  })
  
  output$checkboxOd <- renderUI({
    typeOd <- reactive({
      dataOd <- unique(dataMerge()$od)
    })
    
    checkboxGroupInput(inputId = "filterOd", label = "", 
                       choices  = typeOd(), 
                       selected = typeOd())
  })
  
  output$checkboxDye <- renderUI({
    typeDye <- reactive({
      dataDye <- unique(dataMerge()$dye)
    })
    
    checkboxGroupInput(inputId = "filterDye", label = "", 
                       choices  = typeDye(), 
                       selected = typeDye())
  })
  
  output$checkboxCompDil <- renderUI({
    typeCompDil <- reactive({
      dataCompDil <- unique(dataMerge()$comp_dil)
    })
    
    checkboxGroupInput(inputId = "filterCompDil", label = "", 
                       choices  = typeCompDil(), 
                       selected = typeCompDil())
  })
  
  output$checkboxImageRepeat <- renderUI({
    typeImageRepeat <- reactive({
      dataImageRepeat <- unique(dataMerge()$image_rep)
    })
    
    checkboxGroupInput(inputId = "filterImageRep", label = "", 
                       choices  = typeImageRepeat(), 
                       selected = typeImageRepeat())
    })
  
  dataMergeFiltered <- reactive({
          filter <- dplyr::filter(dataMerge(), 
                                  Area > input$area, 
                                  plate_id %in% input$filterPlateId,
                                  strain %in% input$filterStrain, 
                                  compound %in% input$filterCompound,
                                  od %in% input$filterOd,
                                  dye %in% input$filterDye,
                                  comp_dil %in% input$filterCompDil, 
                                  image_rep %in% input$filterImageRep) # %in% matching operator --> filtered voor alle elementen in x (match) --> de == operator werkt niet omdattie dan opzoek gaat naar cellen die voldoen aan alle eigenschappen in input$strain
          })
  
  
  
  #generate output
  sumImageRep <- reactive({
    dataMergeFiltered() %>%
      group_by(od, comp_dil, compound, well_rep, image_rep) %>%
      summarize( 
        count = n(),
        mean_area = mean(Area, na.rm=TRUE),
        sd_area = sd(Area, na.rm=TRUE)
      )
  })
  
  output$numberUnfilteredRecords <- renderText({
                                paste("Number of unfiltered records is:", nrow(dataMerge()))
                  })
  
  output$numberFilteredRecords <- renderText({
                                paste("Number of filtered records is:", nrow(dataMergeFiltered()))
                  })
    
  output$tblResults <- DT::renderDataTable(dataResults(), 
                                                     options = list(scrollX = TRUE,
                                                                    pageLength = 5,
                                                                    searching = FALSE))
  
  output$tblParameters <- DT::renderDataTable(dataParameters(), 
                                           options = list(scrollX = TRUE,
                                                          pageLength = 5,
                                                          searching = FALSE))
  
  output$tblDataMergeFiltered <- DT::renderDataTable(dataMergeFiltered(), 
                                          options = list(scrollX = TRUE,
                                                         pageLength = 5,
                                                         searching = FALSE))
  
  output$countPlot  <- renderPlot({
    
    if(input$log10transform == TRUE){
    
      ggplot(data = sumImageRep(), mapping = aes_string(x = input$axisX, y = "count")) + #aes_string to be able to refer to input$axisX. reference to column count needs to be in between ""
        geom_line(
          mapping = aes(color = image_rep)) +
          ggtitle("Particle Count") +
          scale_x_continuous(trans = 'log10') +
          facet_grid(od ~ well_rep, scale = "fixed")
    
    } else {
        
      ggplot(data = sumImageRep(), mapping = aes_string(x = input$axisX, y = "count")) + #aes_string to be able to refer to input$axisX. reference to column count needs to be in between ""
        geom_line(
          mapping = aes(color = image_rep)) +
          ggtitle("Particle Count") +
          facet_grid(od ~ well_rep, scale = "fixed")
      }
      
      })

output$densityPlot <- renderPlot({
      ggplot(data = dataMergeFiltered(), mapping = aes(x = log10(Area), y = comp_dil_factor, fill = od)) +
        geom_density_ridges(
          mapping = aes(height=(stat(density))),
          bandwidth = 0.05,
          alpha = 0.5
          ) +
        ggtitle ("Area Distribution (Density)") +
        theme_ridges(center_axis_labels = TRUE) +
        scale_x_continuous(expand = c(0.01, 0)) +
        scale_y_discrete(expand = c(0.01, 0)) +
        facet_grid(~od, scale = "fixed")
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
