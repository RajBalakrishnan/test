# packages ----------------------------------
# .libPaths(new="/home/rstudio/R/R_3.6/")        ## this is for Tao, please don't delete it.
# Packages <- c("shiny","plyr","dplyr", "tidyr","ggplot2", "DT", "limma","pheatmap","httr","jsonlite","shinyBS","DBI","RMariaDB","stringr","igraph",
#               "plotly","RColorBrewer","reshape2","htmltools","psych","shinythemes","survival","survminer","bigmemory","gridExtra","cowplot",
#               "shinycssloaders", "RPostgreSQL", "data.table", "shinyjs")
# invisible(lapply(Packages, library, character.only = TRUE))

##############
library(shiny)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(limma)
library(pheatmap)
library(httr)
library(jsonlite)
library(shinyBS)
library(DBI)
library(RMariaDB)
library(stringr)
library(igraph)
library(plotly)
library(RColorBrewer)
library(reshape2)
library(htmltools)
library(psych)
library(shinythemes)
library(survival)
library(survminer)
library(bigmemory)
library(gridExtra)
library(cowplot)
library(shinycssloaders)
library(RPostgreSQL)
library(data.table)
library(shinyjs)
##############

# helper functions ----------------------------
source('utility/basicInfo.R')
source('utility/common.R')
source('utility/dataCon.R')
source('utility/geneLookup.R')
source('utility/queryBuilder.R')
source('utility/mCOPA.R')

# mimic ggplot default color scheme -----------
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

options(shiny.maxRequestSize = 30*1024^2)
table2_columns_selected <- 0

# load data needed for UI ---------------------
data.dir <- config::get("data.dir")
load(file.path(data.dir, "datForShiny2.RData"))
dat.pantcga <- attach.big.matrix(file.path(data.dir, "dat.pantcga.desc"))
anno.type <- read.delim(file.path(data.dir, "TCGA_cancer_types.tsv"),stringsAsFactors = F)
JIS.anno.type <- anno.type[anno.type$InSockOriginal == "yes",]
JIS.anno.type <- JIS.anno.type[which(JIS.anno.type$display_name != "Breast"),]
css <- "
#large .selectize-input { line-height: 30px; }
#large .selectize-dropdown { line-height: 30px; }"

# UI ------------------------------------------ 
js <- HTML("var changes_done = false;
           Shiny.addCustomMessageHandler('changes_done', function(bool_ch) {
           console.log('Want to leave the Oncology Target Portal?');
           console.log(bool_ch);changes_done = bool_ch;});

           function goodbye(e) {
             if (changes_done === true) {
               if(!e) e = window.event;
               e.cancelBubble = true;
               e.returnValue = 'Are you sure you want to leave the Oncology Target Portal?';
               if (e.stopPropagation) {e.stopPropagation();e.preventDefault();}
             }
           }
           window.onbeforeunload = goodbye;"
          )

ui <- function(request) {
  tagList(tags$head(tags$script(js)),
    tags$head(tags$script(type="text/javascript", src = "thumbnail.js")),
    tags$style(type='text/css', css),
    
    navbarPage(title="Oncology Target Portal", id="nav", theme=shinytheme("cerulean"), #selected = "CMTarget",
               
               tabPanel(title="Gene Search", value="GeneSearch", #tags$img(src = 'caution.png', width = "330px", height = "30px"),
                        
                        div(id = "large",
                            fluidRow(column(2,selectizeInput("gene", label = "Enter a Gene Symbol:", choices = c("Choose a gene" = "", rownames(dat.pantcga)))),
                                     conditionalPanel(condition="input.table2_columns_selected > 1", 
                                                      column(1, align="left", br(),
                                                             div(style="display: inline-block;",
                                                                 actionButton("GoBack", uiOutput("img"), 
                                                                              style="float:left; border: none;background: none;colr: #30a5e7; background-color: white; border-color: white"),
                                                                 bsTooltip("GoBack", "Go Back to CM Targets Tab", placement="bottom", trigger="hover"))
                                                             )
                                      )
                            )
                        ),
                        tags$hr(style="height:3px;border:none;color:#2fa4e7;background-color:#2fa4e7;border-color: #2fa4e7;"),
                        includeCSS(file.path(config::get("www.dir"),"JIS_error.CSS")),
                        #includeCSS(file.path("www","styles.css")),
                        
                        tabsetPanel(id = "inTabset", type = "tabs",
                           source(file.path("ui", "geneQueryTab.R"), local=TRUE)$value,
                           source(file.path("ui", "geneInfoTab.R"), local=TRUE)$value,
                           source(file.path("ui", "gxpTab.R"), local=TRUE)$value,
                           source(file.path("ui", "mutnewTab.R"), local=TRUE)$value,
                           source(file.path("ui", "cnvTab.R"), local=TRUE)$value,
                           source(file.path("ui", "proteinactivityTab.R"), local=TRUE)$value,
                           source(file.path("ui", "essentialityTab.R"), local=TRUE)$value,
                           #source(file.path("ui", "yeastTab.R"), local=TRUE)$value,
                           source(file.path("ui", "survivalTab.R"), local=TRUE)$value,
                           source(file.path("ui", "diseaseRelevance.R"), local=TRUE)$value
                           #,source(file.path("ui", "usefulLinksTab.R"), local=TRUE)$value
                         )
                       ),
   
               navbarMenu(title="Target Prioritization",   
                          source(file.path("ui", "cellsurfaceTab.R"), local=TRUE)$value,
                          source(file.path("ui", "CMTargetPortalTab.R"), local=TRUE)$value
                          ),
               
               source(file.path("ui", "helpTab.R"), local=TRUE)$value
    )
  )
}

# SERVER ---------------------------- 
server <- function(input, output, session) { 
  shinyjs::hide("LegendDisplay")
  shinyjs::hide("geneinfo")
  shinyjs::hide("LegendDisplay")
  shinyjs::hide("Collapse")
  shinyjs::hide("Expand")
  shinyjs::hide("GoBack")
  
  # updateSelectizeInput(session, inputId = "gene", choices = c('Choose a gene'="", rownames(dat.pantcga)), server = TRUE) 
  updateSelectizeInput(session, inputId = "TAAbsAbs1", choices=c('Choose a gene'="",res[[1]]$gene), server=TRUE)
  
  observe({query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query$url)) {
      url <- strsplit(query$url,"\"")[[1]][2]
      updateTabsetPanel(session, 'nav', url)
    }
  })
  
  observeEvent(input$gene, { gene1 <- input$gene
    if (nchar(gene1) <= 0) 
    {
       shinyjs::hide("Expand")
       shinyjs::hide("Collapse")
       shinyjs::hide("Legend")
    }
  }) 
                            
  observeEvent(input$Expand, {
    updateTextInput(session, "LegendDisplay", value = "F")
    shinyjs::hide("Expand")
    shinyjs::show("Collapse")
  })
  
  observeEvent(input$Collapse, {
    updateTextInput(session, "LegendDisplay", value = "P")
    shinyjs::show("Expand")
    shinyjs::hide("Collapse")
  })
    
  observeEvent(input$Expand2, {
    updateTextInput(session, "LegendDisplay", value = "F")
    shinyjs::hide("Expand2")
    shinyjs::show("Collapse2")
  })
  
  observeEvent(input$Collapse2, {
    updateTextInput(session, "LegendDisplay", value = "P")
    shinyjs::show("Expand2")
    shinyjs::hide("Collapse2")
  })
  
  observeEvent(input$table2_columns_selected, {
    colNo <- input$table2_columns_selected
    # updateActionButton(session,"GoBack", label = "Back to CM Targets")
    #updateActionButton(session,"GoBack", img(src = "back.png"))
    output$img <- renderUI({tags$img(src = "back.png", float="left", width = "40px", height = "30px")}) 
    show("GoBack")
    #updateActionButton(session,"GoBack", img(src = "back.png"))
    
    if (colNo == 2) 
      {updateNavbarPage(session, "nav", selected = "GeneSearch")
       updateTabsetPanel(session, "inTabset", selected = "CNVpanel")}
    
    if (colNo == 3) 
    {updateNavbarPage(session, "nav", selected = "GeneSearch")
      updateTabsetPanel(session, "inTabset", selected = "MutationPanel")}
    
    if (colNo == 4) 
    {updateNavbarPage(session, "nav", selected = "GeneSearch")
      updateTabsetPanel(session, "inTabset", selected = "gxp")}
    
    if (colNo == 5) 
    {updateNavbarPage(session, "nav", selected = "GeneSearch")
      updateTabsetPanel(session, "inTabset", selected = "FuncDep")}
    
    if (colNo == 6) 
    {updateNavbarPage(session, "nav", selected = "GeneSearch")
      updateTabsetPanel(session, "inTabset", selected = "Prognosis")}
  })
  
  observeEvent(input$GoBack, { 
      hide("GoBack")
      updateNavbarPage(session, "nav", selected = "CMTargetPortal")
      updateTabsetPanel(session, "tabselected3", selected = "evidence")
  })
  
  observeEvent(input$table1_rows_selected, {
    #  data <- results_table()
    if(is.null(my_sum$totalwts)){
      data <- results_table_default()
    } else if (!is.null(my_sum$totalwts)) {
      data <- results_table_reactive()
    }
    rowNo <- input$table1_rows_selected
    mygene <- row.names(data)[rowNo]
    #updateSelectizeInput(session, "gene", choices = c('Choose a gene'="", rownames(dat.pantcga)), server = TRUE) 
    updateSelectizeInput(session, inputId ="gene", choices = c('Choose a gene'="", rownames(dat.pantcga)), selected = mygene, server=TRUE)
  })

  
  # load data -----------------------------------
  source(file.path("server", "loadData.R"))
  
  source(file.path("server", "geneQueryEventHandlers.R"       ), local=TRUE)$value
  source(file.path("server", 'geneInfoEventHandlers.R'        ), local=TRUE)$value
  source(file.path("server", "gxpEventHandlers.R"             ), local=TRUE)$value
  source(file.path("server", "mutationnewEventHandlers.R"     ), local=TRUE)$value
  source(file.path("server", "cnvEventHandlers.R"             ), local=TRUE)$value
  source(file.path("server", "proteinActivityEventHandlers.R" ), local=TRUE)$value
  source(file.path("server", "essentialityEventHandlers.R"    ), local=TRUE)$value
  source(file.path("server", "yeastEventHandlers.R"           ), local=TRUE)$value
  source(file.path("server", "survivalEventHandlers.R"        ), local=TRUE)$value
  source(file.path("server", "cellsurfaceEventHandlers.R"     ), local=TRUE)$value
  source(file.path("server", "CMTarget2EventHandlers.R"        ), local=TRUE)$value
  #source(file.path("server", "CMTargetEventHandlers.R"        ), local=TRUE)$value
  source(file.path("server", "usefulLinksEventHandlers.R"     ), local=TRUE)$value
  source(file.path("server", "diseaseRelevanceEventHandlers.R"), local=TRUE)$value
}


shinyApp(ui = ui, server = server, enableBookmarking = "url")
