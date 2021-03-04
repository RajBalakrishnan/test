library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyalert)
library(shinydashboard)
library(shinycssloaders)
library(rhandsontable)
library(data.table)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(janitor)
library(readxl)
library(WriteXLS)
library(shinyWidgets)

str_right <- function(string, n) {substr(string, nchar(string) - (n - 1), nchar(string))}
PlateDef <- read_csv("PlateDef.csv")
hibit_cv <-  read_csv("hibit_cv.csv")
PlateDetails <- read_csv("PlateDetails.csv")

colnames(hibit_cv) <- gsub(" ","_", colnames(hibit_cv))
RowNames <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P")
pholder <- as.data.frame(matrix(data = "", nrow = 16, ncol = 24, byrow = FALSE, dimnames = NULL))
colnames(pholder) <- paste0("Col-",seq(1:24))

unfactorize <- function(df){for(i in which(sapply(df, class) == "factor")) 
                                {df[[i]] = as.character(df[[i]])}
                            return(df)}

PlateDef <- unfactorize(PlateDef)
hibit_cv <- unfactorize(hibit_cv)
PlateDetails <- unfactorize(PlateDetails)

MMark <- function(label) { tagList( label, span("*", style = "color:red; font-size: 12px;"))}

server <-  function(input, output, session) {  disable("DownloadData")

values <- reactiveValues(hbData = pholder , hbCmpd = pholder , 
                           hbConc = pholder, hbBatch = pholder, 
                           PlateDetails = PlateDetails, nop = NULL)

observeEvent(input$Upload, {  if(is.null(input$hibitDataFile)) {return(NULL)}
    inFile <- input$hibitDataFile
    hbd <- as.data.frame(read_csv(inFile$datapath, col_names = FALSE, skip = 10))
    hbd2 <- hbd[rowSums(is.na(hbd)) == 0,]
    hbd2[,1] <- NULL
    values$hbData <- hbd2
    colnames(values$hbData) <- paste0("Col-",seq(1:24))
    for (x in 1:nrow(values$hbData)) {row.names(values$hbData)[x] <- x}
    values$hbCmpd <- as.data.frame(matrix(data = "", nrow = nrow(values$hbData), 
                                          ncol = 24, byrow = FALSE, dimnames = NULL))
    colnames(values$hbCmpd) <- colnames(values$hbData)
    values$hbConc <- values$hbCmpd
    values$hbBatch <- values$hbCmpd
    NoP <- as.numeric(nrow(values$hbData)/16)
    values$nop <- NoP
    updateSelectInput(session, "Plates", choices = seq(1:NoP))
    values$PlateDetails <- PlateDetails %>% filter(PlateDetails$PLATE_NUMBER <= NoP)
})
  
observeEvent(input$Parse, {if(is.null(input$hibitDataFile)) {return(NULL)}  
  
  if (as.numeric(input$Plates) == 1) {st <- 1; st1 <- 1} 
      else {st <- 16 * (as.numeric(input$Plates)-1) + 1
            st1 <- 15 * (as.numeric(input$Plates)-1) + 1}
  sp <- st +15; sp1  <- st1 + 14
  values$PlateDetails[st1:sp1,] <- hot_to_r(input$pDetail)
  pDetail2  <- hot_to_r(input$pDetail)
  PlateDef2 <- PlateDef
  
    for (j in 1:384) {for (k in 1:15)
          {if (PlateDef2$Object[j] == pDetail2$OBJECT[k])
            {PlateDef2$Rescue_Compound[j] <-  pDetail2$RESCUE_CMPD[k]
             PlateDef2$Concentration[j] <-    pDetail2$RESCUE_CMPD_CONCN[k]}
          }
    }
  #write.csv(PlateDef2, "PlateDef21.csv")
  cnt = 1;  sta <- st -1
    for (j in 1:16) 
      {for (k in 1:24)
          {values$hbCmpd[sta+j,k] <- PlateDef2$Rescue_Compound[cnt]
           values$hbConc[sta+j,k] <- PlateDef2$Concentration[cnt]
           cnt = cnt + 1
          }
    }
  }
)


output$pDetail <- renderRHandsontable({pdTbl()})

pdTbl <- reactive({
  if (as.numeric(input$Plates) == 1) {st <- 1} else {st <- 15 * (as.numeric(input$Plates)-1) + 1}
  sp <- st +14
  cv <- rhandsontable(values$PlateDetails[st:sp,], height = 390, readOnly = F)  %>% 
  hot_cols(columnSorting = FALSE, colWidths = 150, manualColumnResize =TRUE) %>%
  hot_col("PLATE_NUMBER", format = "0a") %>%
  hot_col("OBJECT") %>%
  hot_col("RESCUE_CMPD", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("RESCUE_CMPD_CONCN", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Cmpd_Conc)))
  return(cv)
})

output$hibitData <- renderRHandsontable({hbTbl()})

hbTbl <- reactive({
  if (as.numeric(input$Plates) == 1) {st <- 1} else {st <- 16 * (as.numeric(input$Plates)-1) + 1}
  sp <- st +15
  cv <- rhandsontable(values$hbData[st:sp,],height = 420, readOnly = F, rowHeaders = RowNames)  %>% 
  hot_cols(columnSorting = FALSE, colWidths = 125, manualColumnResize =TRUE)
  return(cv)
})

output$hibitCmpd <- renderRHandsontable({hbTbl2()})

hbTbl2 <- reactive({
  if (as.numeric(input$Plates) == 1) {st <- 1} else {st <- 16 * (as.numeric(input$Plates)-1) + 1}
  sp <- st +15
  cv <- rhandsontable(values$hbCmpd[st:sp,],height = 420, readOnly = F, rowHeaders = RowNames)  %>%  
  hot_col("Col-1", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-2", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-3", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-4", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-5", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-6", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-7", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-8", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-9", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-10", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-11", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-12", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-13", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-14", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-15", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-16", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-17", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-18", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-19", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-20", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-21", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-22", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-23", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_col("Col-24", type = "dropdown", source = sort(unique(hibit_cv$Rescue_Compound))) %>%
  hot_cols(columnSorting = FALSE, colWidths = 125, manualColumnResize =TRUE)
  return(cv)
})

output$hibitConc <- renderRHandsontable({hbTbl3()})

hbTbl3 <- reactive({
if (as.numeric(input$Plates) == 1) {st <- 1} else {st <- 16 * (as.numeric(input$Plates)-1) + 1}
  sp <- st +15
  cv <- rhandsontable(values$hbConc[st:sp,],height = 420, readOnly = F, rowHeaders = RowNames)  %>% 
  hot_col("Col-1", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-2", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-3", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-4", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-5", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-6", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-7", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-8", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-9", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-10", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-11", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-12", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-13", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-14", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-15", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-16", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-17", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-18", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-19", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-20", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-21", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-22", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-23", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_col("Col-24", type = "dropdown", source = unique(hibit_cv$Rescue_Cmpd_Conc)) %>%
  hot_cols(columnSorting = FALSE, colWidths = 125, manualColumnResize =TRUE)
  return(cv)
})

observeEvent(input$Update, { if(is.null(input$hibitDataFile)) {return(NULL)}
if (as.numeric(input$Plates) == 1) {st <- 1; st1 <- 1} 
  else {st <- 16 * (as.numeric(input$Plates)-1) + 1
        st1 <- 15 * (as.numeric(input$Plates)-1) + 1}
  sp <- st +15; sp1  <- st1 + 14
  values$PlateDetails[st1:sp1,] <- hot_to_r(input$pDetail)
  values$hbCmpd[st:sp,] <- hot_to_r(input$hibitCmpd)
  values$hbConc[st:sp,] <- hot_to_r(input$hibitConc)
})


observeEvent(input$SaveParData, {if(is.null(input$hibitDataFile)) {return(NULL)}
  sink("Parsed_HitBiT_Data.csv")
  for (i in 1:values$nop)
    { if (i == 1) {st <- 1} else {st <- 16 * (i-1) + 1}
      sp <- st +15
      cat(paste("Plate -",i, "[Raw Data]")); cat('\n');
      write.csv(values$hbData[st:sp,])
      cat('\n');  cat(paste("Plat - ",i, "[Rescue Compounds]")); cat('\n')
      write.csv(values$hbCmpd[st:sp,])
      cat('\n');  cat(paste("Plat - ",i, "[Rescue Compounds Concn.]")); cat('\n')
      write.csv(values$hbConc[st:sp,])
      cat('\n'); cat('\n')
    }
  sink()  
  shinyalert(title = "Parsed Data Generated", type = "success")
  enable("DownloadData")
})

output$plateMapA <- renderUI({p(br(),tags$img(src = input$PlateMap, height = "400px",width = "900px"))})
output$DownloadData <- downloadHandler(
filename <- function() {paste0("Parsed_HitBiT_Data",".csv")}, 
content <- function(file) {file.copy("Parsed_HitBiT_Data.csv", file)},
contentType = "text/csv")

}

ui <- dashboardPage(
title = "Data Pre-Processing Tool",
dashboardHeader(title = "Data Pre Process",
                tags$li(a(href = '', img(src= 'Nurix.jpg', title = 'Nurix Informatics', height = "28px")), class = 'dropdown')
                ),
dashboardSidebar(useShinyjs(),useShinyalert(),
         sidebarMenu(menuItem("Data Parsing for Activity Base", tabName = "RawData" , badgeColor = "blue")),
         sidebarMenu(menuItem("About", tabName = "About" , badgeColor = "blue"))
        ),

dashboardBody(tags$head(tags$script(type = "text/javascript", src = js)), #tags$script(jscode),
    tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}",
            ".btn {border-radius: 15px; font-family:'Verdana';font-size: 15px ;color: #fff; background-color: #337ab7; cursor: pointer;
            text-align: center;  text-decoration: none;  display: inline-block;font-size: 10px;font-stretch: expanded;
            transition-duration: 0.7s;border: 1px solid;border-color: #337ab7; padding: 10px;box-shadow: 5px 10px 8px rgba(0, 0, 0, 0.4)}"))),
    tabItems(
        tabItem(tabName = "RawData",
                fluidRow(
                    box(width = 2,
                        selectInput("PlateMap", "Select Plate Map", choices = c("11PT_15C_NC_BC_1CCR.png", "16PT_10CMPD_NC_BC.png", "10PT_3CMPDS_NC_BC_1CCR_TT.png" ), 
                                    selected = "11PT_15C_NC_BC_1CCR.png"),
                        fileInput("hibitDataFile",MMark("Select Data Input File"), multiple = FALSE, 
                                  accept = c("csv", "comma-separated-values", ".csv"),
                                  width = NULL, buttonLabel = "Browse...", placeholder = "No file selected"),
                        actionButton("Upload", "Upload Data"),br(),br(),
                        selectInput("Plates", "Select Plate", choices = "1", selected = "1"),br(),
                        actionButton("Parse", "Parse Data"),br(),br(),
                        actionButton("SaveParData", "Save Parsed Data"),br(),br(),
                        downloadButton("DownloadData", "Download Parsed Data"),
                        br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                        br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                        br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                        br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                        br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                        br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                        br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                        br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), 
                        br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                    ),
                    #conditionalPanel(condition = "input.ExpType == 'CTG'",
                    
                    column(10, box(width = 12,
                                   tabPanel("RawData",
                                            uiOutput('plateMapA', width = "850px", height = "400px"),
                                            style = "overflow-y:scroll; max-height: 425px"),
                                   solidHeader = T, collapsible = T, collapsed = F,
                                   title = "Plate Map",status = "primary")),
                    column(10, box(width = 12,
                                   tabPanel("RawData",
                                  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
                                                tags$script(type = "text/javascript", src = "busy.js"),
                                            rHandsontableOutput("hibitData", width = "auto", height = "auto"),
                                  div(class = "busy", h4(strong(span("Loading & Processing data....!",
                                        style = "color:blue"))), img(src = "Processing.gif")),
                                            style = "overflow-y:scroll; max-height: 500px"),
                                   solidHeader = T, collapsible = T, collapsed = F,
                                   title = "Raw Data",status = "primary")),
                    column(10, box(width = 12,
                                   tabPanel("RawData",
                                            rHandsontableOutput("pDetail", width = "auto", height = "auto"),
                                            style = "overflow-y:scroll; max-height: 500px"),
                                   solidHeader = T, collapsible = T, collapsed = F,
                                   title = "Plate Details",status = "primary")),
                    column(10, box(width = 12,
                                   tabPanel("RawData",
                                            rHandsontableOutput("hibitCmpd", width = "auto", height = "auto"),
                                            style = "overflow-y:scroll; max-height: 500px"),
                                   solidHeader = T, collapsible = T, collapsed = F,
                                   title = "Compound Names",status = "primary")),
                    column(10, box(width = 12,
                                   tabPanel("RawData",
                                           rHandsontableOutput("hibitConc", width = "auto", height = "auto"),
                                            style = "overflow-y:scroll; max-height: 500px"),
                                   solidHeader = T, collapsible = T, collapsed = F,
                                   title = "Compound Concentrations",status = "primary"))
                )),
        tabItem(tabName = "About",
                box(width = 12,
                    tabPanel("About",
                             h2(p(span("Data Pre-Processing Tool", style = "font-family: 'Arial Black';color: DarkMagenta"))),
                             h4(p(span("Metadata pre-processing is required for those assays where Well-Level Conditions (e.g. Rescue Compound, 
                                       Rescue Compound Concentration) vary across a single assay plate, for one or more Object titrations (NRX # titrations).  
                                       Normally, Well-Level Conditions are input directly into an ABase Template via the Configurable Properties feature, 
                                       however, this is only possible when the Well-Level Conditions are uniform across the assay plate (i.e. across all 
                                       wells, all Object titrations).  The solution needed was to have a way to input this Well-Level Condition information
                                       as 'data', having the user append the information (using Controlled Vocabulary) to the instrument raw data file itself.  
                                       This pre-processing tool would be used to achieve this.  Currently, the need for this pre-processing is for the 
                                       ABase HiBiT Competition-Rescue Template.", 
                                       style = "font-family: 'Arial';color:black"))),
                             HTML('<center><img src="wf.png"></center>'),br(),br(),br(),
                             HTML('<left><img src="author.png"></left>')
                             
                    ),
                    solidHeader = T, collapsible = T, title = "About",  status = "primary"))
    ) #tabItems
) # dashboardBody
) # dashboardPage

shinyApp(ui = ui, server = server)
