# call libraries
library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(DT)
library(ggridges)
library(cowplot)

theme_set(theme_cowplot(font_size=8))

# set max file size
options(shiny.maxRequestSize=125*1024^2)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  # Application title
  dashboardHeader(
        title = "Bioclin dataanalysis"),
    
  # sidebar
  dashboardSidebar( 
      
      fileInput(inputId = "parameters", label = h4("Insert parameters file (xlsx)"), multiple = FALSE),
      fileInput(inputId = "results", label = h4("Insert results file (CSV)"), multiple = FALSE),
      sliderInput(inputId = "area", label = h4("Set area cut-off (1-1000)"), value = 10, min = 0, max = 500, step = 5, ticks = FALSE, animate = TRUE),
      
      box(
        title = "Select Parameters",
        width = 12,
        background = "navy",
        solidHeader = TRUE,
        status = "primary",
        box (title = "Plate", width = 12, background = "black", status = "primary", uiOutput(outputId = "checkboxPlate")),
        box (title = "Strain", width = 12, background = "black", status = "primary", uiOutput(outputId = "checkboxStrain")),
        box (title = "Compound", width = 12, background = "black", status = "primary", uiOutput(outputId = "checkboxCompound")),
        box (title = "Od", width = 12, background = "black", status = "primary", uiOutput(outputId = "checkboxOd")),
        box (title = "Dye", width = 12, background = "black", status = "primary", uiOutput(outputId = "checkboxDye")),
        box (title = "Compound dilution", width = 12, background = "black", status = "primary", uiOutput(outputId = "checkboxCompDil")),
        box (title = "Image repeat", width = 12, background = "black", status ="primary", uiOutput(outputId = "checkboxImageRepeat"))
        ),   
      
      box(
        title = "Number of records",
        background = "navy",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        textOutput('numberUnfilteredRecords'),
        textOutput('numberFilteredRecords')
        ),
      
      box (
        title = "select images",
        width = 12,
        background = "navy",
        solidHeader = TRUE,
        status = "primary",
        uiOutput(outputId = "checkboxImage")
        ),
      
      width = 400
      ), 

  # body  
  dashboardBody(
    
    box(
      title = "Raw Data",
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      fluidRow(
        box (DT::dataTableOutput("parametersTbl")),
        box (DT::dataTableOutput("resultsTbl"))
        )
      ),
    
    box(
      title = "Filtered Data",
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      DT::dataTableOutput('dataMergeFilteredTbl')
      ),
    
    box(
      title = "Summary Data",
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      DT::dataTableOutput('sumExpTbl'),
      downloadButton(outputId = "downloadSumExpTbl", label = "Download Summary Data")
      ),
    
    
    
    box(
      width = 12,
      selectInput(inputId = "axisX", 
                  label = "Select X-axis",
                  choices  = c("compound", "comp_dil", "od"),
                  selected = "comp_dil")
      ),
 
    box( 
      width = 12,
      fluidRow(
        box (
          title = "Count Particles",
          width = 6,
          height = 625,
          solidHeader = TRUE,
          status = "primary",
          fluidRow(
            box(
              width = 4,
              height = 100,
              solidHeader = TRUE,
              checkboxInput(inputId = "log10transformCount",
                            label = "Log10 Transformation X-axis",
                            value = TRUE)
              ),
            
            box(
              width = 4,
              height = 100,
              solidHeader = TRUE,
              selectInput(inputId = "facetRowCount", 
                          label = "Facet Row",
                          choices = c(None=".", "compound", "od", "well_rep"),
                          selected = "well_rep")
              ),
            
            box(
              width = 4,
              height = 100,
              solidHeader = TRUE,
              selectInput(inputId = "facetColCount", 
                          label = "Facet Column",
                          choices = c(None=".", "compound", "od", "strain"))
              )
          ),
          plotOutput("countPlot"),
          downloadButton(outputId = "downloadCountPlot", label = "Download countPlot")
        ),
    
        box (
          title = "Density",
          width = 6,
          height = 625,
          solidHeader = TRUE,
          status = "primary",
          fluidRow(
            box(
              width = 4,
              height = 100,
              solidHeader = TRUE,
              selectInput(inputId = "facetRowDensity", 
                          label = "Facet Row",
                          choices = c("compound", compoundDilution = "comp_dil_factor" , od = "od_factor"))
              ),
            
            box(
              width = 4,
              height = 100,
              solidHeader = TRUE,
              selectInput(inputId = "facetColDensity", 
                          label = "Facet Column",
                          choices = c(None=".", "compound", compoundDilution = "comp_dil_factor", od = "od_factor"))
              ),
      
            box(
              width = 4,
              height = 100,
              solidHeader = TRUE,
              selectInput(inputId = "fillDensity",
                          label = "Fill",
                          choices = c("compound", compoundDilution = "comp_dil_factor", od = "od_factor"))
              )
            ),
          plotOutput("densityPlot"),
          downloadButton(outputId = "downloadDensityPlot", label = "Download densityPlot")
          )
        ),
      
    # box (
    #   title = "Particle",
    #   width = 12,
    #   solidHeader = TRUE,
    #   status = "primary",
    #   
    #   checkboxInput(inputId = "log10transformParticle",
    #                 label = "Log10 Transformation X-axis",
    #                 value = TRUE),
    #   
    #   selectInput(inputId = "facetRowParticle", 
    #               label = "Facet Row",
    #               choices = c(None=".", "compound", "od")),
    #   
    #   selectInput(inputId = "facetColParticle", 
    #               label = "Facet Column",
    #               choices = c(None=".", "compound", "od")),
    #   
    #   plotOutput("particlePlot")
    #   ),
  
    box(
      title = "Mean Area",
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      fluidRow(  
        box(
          width = 4,
          height = 100,
          solidHeader=TRUE,
          checkboxInput(inputId = "log10transformMeanArea",
                        label = "Log10 Transformation X-axis",
                        value = TRUE),
          checkboxInput(inputId = "plotSeries",
                        label = "Plot Series",
                        value = TRUE)
          ),
        
        box(
          width = 4,
          height = 100,
          solidHeader=TRUE,
          selectInput(inputId = "facetRowArea", 
                      label = "Facet Row",
                      choices = c(None=".", "compound", "od", "well_rep", "strain"),
                      selected = "well_rep"
                      )
          ),
        
        box(
          width = 4,
          height = 100,
          solidHeader=TRUE,
          selectInput(inputId = "facetColArea", 
                      label = "Facet Column",
                      choices = c(None=".", "compound", "od", "well_rep", "strain")
                      )
          ),
      
        box(
          width = 6,
          solidHeader=TRUE,
          plotOutput("meanAreaPlot")
          ),
      
        box(
          width = 6,
          solidHeader=TRUE,
          plotOutput("meanImagePlot")
          ),
    
        box(
          width = 6,
          solidHeader=TRUE,
          plotOutput("meanExpPlot"),
          downloadButton(outputId = "downloadMeanAreaPlot", label = "Download meanAreaPlot"),
          downloadButton(outputId = "downloadMeanImagePlot", label = "Download meanImagePlot"),
          downloadButton(outputId = "downloadMeanExpPlot", label = "Download meanExpPlot"), 
          downloadButton(outputId = "allGraphs", label = "Download Report")
          )
        
        ) #fluidrow
      ) # box
    ) # box
  ) #dashboard
) #ui dashboardPage

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #read & merge datafiles
  dataResults <- reactive({
    df_results <- input$results[1,4]
    if (!is.null(df_results))
    readr::read_csv(df_results, col_names = TRUE)%>%
    separate(Label, into = c("exp_id" , "plate_rep" , "well_id" , "image_rep"), sep="_")%>%
    tidyr::unite(id, "plate_rep", "well_id", sep = "-") %>%
    select (X1, id, image_rep, Area) 
    })

  dataParameters <- reactive({
    workbook <- input$parameters[1,4]
    if (!is.null(workbook))
    workbook <- readxl::excel_sheets(workbook)

    df_parameters <- input$parameters[1,4]
    if (!is.null(df_parameters))
      readxl::read_xlsx(df_parameters, sheet = workbook[2]) %>%
      tidyr::unite(compound, "comp_name", "comp_id", "comp_trtmnt", sep = "-") %>%
      tidyr::unite(id, "plate_rep", "well_id", sep = "-") %>%
      dplyr::mutate(comp_dil = ifelse(is.na(comp_dil), 0, comp_dil)) 
    })

  dataMerge <- reactive({
      dplyr::left_join(dataResults(), dataParameters(), by = c("id")) %>%
      dplyr::mutate(od = as.numeric(od)) %>%
      dplyr::mutate(od_factor = as.factor(od)) %>%
      dplyr::mutate(comp_dil = as.numeric(comp_dil)) %>%
      dplyr::mutate(comp_dil_factor = as.factor(comp_dil)) %>%
      dplyr::mutate(image_rep = as.factor(image_rep)) %>%
      dplyr::mutate(well_rep = as.factor(well_rep))
      })
  
  output$numberUnfilteredRecords <- renderText({
                                paste("Number of unfiltered records is:", nrow(dataMerge()))
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
  
  output$checkboxImage <- renderUI({
    typeImage <- reactive({
      dataImage <- unique(dataMerge()$id)
    })
    
    checkboxGroupInput(inputId = "filterImageId", label = "",
                       choices = typeImage(),
                       selected = typeImage())
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
                                  image_rep %in% input$filterImageRep,
                                  id %in% input$filterImageId)    #%>% # %in% matching operator --> filtered voor alle elementen in x (match) --> de == operator werkt niet omdattie dan opzoek gaat naar cellen die voldoen aan alle eigenschappen in input$strain
                                  #sample_frac(0.1)
           })
  
  output$numberFilteredRecords <- renderText({
                                paste("Number of filtered records is:", nrow(dataMergeFiltered()))
                  })
  
  
  #generate tables
  
  sumImageRep <- reactive({
    dataMergeFiltered() %>%
      group_by(od, comp_dil, compound, well_rep, image_rep) %>%
      summarize( 
        count = n(),
        meanArea = mean(Area, na.rm=TRUE),
        sdArea = sd(Area, na.rm=TRUE)
        )
  })
  
  sumWellRep <- reactive ({
    sumImageRep() %>%
      group_by(od, comp_dil, compound, well_rep) %>%
      summarize( 
        count = n(),
        meanImage = mean(meanArea, na.rm=TRUE),
        sdImage = sd(meanArea, na.rm=TRUE),
        semImage = sd(meanArea, na.rm=TRUE)/sqrt(n())
        )
  })
  
  sumExp <- reactive ({
    sumWellRep() %>%
      group_by(od, comp_dil, compound) %>%
      summarize( 
        count = n(),
        meanWell = mean(meanImage, na.rm=TRUE),
        sdWell = sd(meanImage, na.rm=TRUE),
        semWell = sd(meanImage, na.rm=TRUE)/sqrt(n())
        )
  })
  
  output$resultsTbl <- DT::renderDataTable(dataResults(), 
                                           options = list(scrollX = TRUE,
                                                          pageLength = 5,
                                                          searching = FALSE))
  
  output$parametersTbl <- DT::renderDataTable(dataParameters(), 
                                              options = list(scrollX = TRUE,
                                                            pageLength = 5,
                                                            searching = FALSE))
  
  output$dataMergeFilteredTbl <- DT::renderDataTable(dataMergeFiltered(), 
                                                    options = list(scrollX = TRUE,
                                                                  pageLength = 5,
                                                                  searching = FALSE))
  
  output$sumExpTbl <- DT::renderDataTable(sumExp(),
                                          options = list(scrollX = TRUE,
                                                        pageLength = 5,
                                                        searching = FALSE))
  
  output$downloadSumExpTbl <- downloadHandler(
    filename = function(){
      paste("sumExp", "csv", sep = ".")
    },
    content = function(file){ 
      write.csv(sumExp(), file, row.names = FALSE)
      }
    )
  
  # My_Theme = theme(
  #   plot.title = element_text(size=12, face = "bold"),
  #   axis.title.x = element_text(size = 10),
  #   axis.text.x = element_text(size = 10),
  #   axis.title.y = element_text(size = 10),
  #   axis.text.y = element_text(size = 10),
  #   legend.title = element_text(size = 10),
  #   legend.text = element_text(size=10))
  
  #generate plots
  
  plotCount <- reactive({
    plot1 <- ggplot(data = sumImageRep(), mapping = aes_string(x = input$axisX, y = "count")) + #aes_string to be able to refer to input$axisX. reference to column count needs to be in between ""
      geom_line(
        mapping = aes(color = image_rep)) +
      ggtitle("Particle Count") 
   
    if(input$log10transformCount == TRUE)
      plot1 <- plot1 + scale_x_continuous(trans = 'log10')
    
    facetsCount <- paste(input$facetRowCount, '~', input$facetColCount)
    if (facetsCount != '. ~ .')
      plot1 <- plot1 + facet_grid(facetsCount)
  })
  
  plotDensity <- reactive({
    plot2 <- ggplot(data = dataMergeFiltered(), mapping = aes_string(x = "log10(Area)", y = input$facetRowDensity, fill = input$fillDensity)) +
      geom_density_ridges(
        mapping = aes(height=(stat(density))),
        bandwidth = 0.05,
        alpha = 0.5
      ) +
      ggtitle ("Area Distribution (Density)") +
     # theme_ridges(center_axis_labels = TRUE) +
      #coord_fixed(ratio = 1) +
      scale_x_continuous(expand = c(0.01, 0)) +
      scale_y_discrete(expand = c(0.01, 0)) 

    facetsDensity <- paste('~', input$facetColDensity)
    if (facetsDensity != '~.')
      plot2 <- plot2 + facet_grid(facetsDensity)
  })
  
  plotMeanArea <- reactive({
    plot4 <- ggplot(data = sumImageRep(), mapping = aes_string(x = input$axisX, y = "log10(meanArea)", color = "image_rep")) +
                ggtitle("Intrawell Reproducibility")
      
      
                if (input$plotSeries == TRUE){
                  plot4 <- plot4 + geom_line() 
                } else {
                  plot4 <- plot4 + geom_boxplot(mapping = aes(x = compound))
                }
                
                if (input$log10transformMeanArea == TRUE)
                plot4 <- plot4 + scale_x_continuous(trans = 'log10')
      
                facetsArea <- paste(input$facetRowArea, '~', input$facetColArea)
                if (facetsArea != '.~.') 
                  plot4 <- plot4 + facet_grid(facetsArea)
    })
  
  plotMeanImage <- reactive({
    plot5 <- ggplot()+ 
                ggtitle("Interwell Reproducibility") 
      
                if (input$plotSeries == TRUE){
                  plot5 <- plot5 + geom_ribbon(data = sumWellRep(), mapping = aes_string(x = input$axisX, ymin = "log10(meanImage - semImage)", ymax = "log10(meanImage + semImage)", fill = "well_rep", alpha=0.1)) +
                  geom_line(data = sumWellRep(), mapping = aes_string(x = input$axisX, y = "log10(meanImage)", color = "well_rep"))
                } else {
                  plot5 <- plot5 + geom_boxplot(data= sumImageRep(), mapping = aes_string(x = "compound", y = "log10(meanArea)", color = "well_rep"))
                }
              
                if (input$log10transformMeanArea == TRUE)
                  plot5 <- plot5 + scale_x_continuous(trans = 'log10')
    
                facetsArea <- paste('~', input$facetColArea)
                if (facetsArea != '~.')
                  plot5 <- plot5 + facet_grid(facetsArea)
    })
  
  plotMeanExp <- reactive({
    plot6 <- ggplot() + 
                ggtitle("Experiment")
      
                if (input$plotSeries == TRUE){
                  plot6 <- plot6 + geom_ribbon(data = sumExp(), mapping = aes_string(x = input$axisX, ymin = "log10(meanWell - semWell)", ymax = "log10(meanWell + semWell)", fill = "compound", alpha=0.1)) +
                  geom_line(data = sumExp(), mapping = aes_string(x = input$axisX, y = "log10(meanWell)", color = "compound"))
                } else {
                  plot6 <- plot6 + geom_boxplot(data = sumWellRep(), mapping = aes_string(x = "compound", y="log10(meanImage)"))
                }
                
                if (input$log10transformMeanArea == TRUE)
                  plot6 <- plot6 + scale_x_continuous(trans = 'log10')
    
                facetsArea <- paste('~', input$facetColArea)
                 if (facetsArea != '~.') 
                   plot6 <- plot6 + facet_grid(facetsArea)
    })    
 
  
  output$countPlot <- renderPlot({
    print(plotCount())
    })
  
  output$densityPlot <- renderPlot({
    print(plotDensity())
    })
  
  output$meanAreaPlot <- renderPlot({
    print(plotMeanArea())
  })  
  
  output$meanImagePlot <- renderPlot({
    print(plotMeanImage())
  })
  
  output$meanExpPlot <- renderPlot({
    print(plotMeanExp())
  })  
  
  
  output$downloadCountPlot <- downloadHandler(
    filename = function(){
      paste("countPlot", "png", sep = ".")
    },
    content = function(file) {
      ggsave(file,plotCount())
    }
    )
  
  output$downloadDensityPlot <- downloadHandler(
    filename = function(){
      paste("densityPlot", "png", sep = ".")
    },
    content = function(file) {
      ggsave(file,plotDensity())
    }
    )
  
  output$downloadMeanAreaPlot <- downloadHandler(
    filename = function(){
      paste("meanAreaPlot", "png", sep = ".")
    },
    content = function(file) {
      ggsave(file,plotMeanArea())
    }
    )
  
  output$downloadMeanImagePlot <- downloadHandler(
    filename = function(){
      paste("meanImagePlot", "png", sep = ".")
    },
    content = function(file) {
      ggsave(file, plotMeanImage())
    }
    )
  
  output$downloadMeanExpPlot <- downloadHandler(
    filename = function(){
      paste("meanExpPlot", "png", sep = ".")
    },
    content = function(file) {
      ggsave(file, plotMeanExp())
    }
  )
  
  
  output$allGraphs = downloadHandler(
    filename= function(){
      paste("report", "png", sep = ".")
    },
    content = function(file) {
      panel <- cowplot::plot_grid(plotCount(), plotDensity(), plotMeanArea(), plotMeanImage(), plotMeanExp(), labels = c("A", "B", "C", "D", "E"), rel_widths = c(1,1), nrow = 3, ncol =2) #https://cran.r-project.org/web/packages/cowplot/cowplot.pdf
      ggsave(file, panel)
    }  
    )
  
  
  

  # output$particlePlot <- renderPlot({
  #     
  #     plot3 <- ggplot(data = dataMergeFiltered(), mapping = aes_string(x = input$axisX, y = "log10(Area)")) +
  #       geom_point(
  #         mapping = aes_string(color = "well_rep", group = input$axisX),
  #         alpha = 0.1,
  #         position = "jitter" ) +
  #       ggtitle("Particle Size (log)")
  #     
  #     if (input$log10transformParticle == TRUE)
  #       plot3 <- plot3 + scale_x_continuous(trans = 'log10')
  #     
  #     facetsParticle <- paste(input$facetRowParticle, '~', input$facetColParticle)
  #     if (facetsParticle != '. ~ .') 
  #       plot3 <- plot3 + facet_grid(facetsParticle)
  #       
  #     print(plot3)
  #   })
 

    
  
  #https://stackoverflow.com/questions/49977969/using-a-download-handler-to-save-ggplot-images-in-shiny
  #https://www.staringatr.com/3-the-grammar-of-graphics/multi-plots/2_savingmultiplefigures/
  #https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
}

# Run the application 
shinyApp(ui = ui, server = server)
