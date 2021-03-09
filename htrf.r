for (package in c('shiny','DT')) {
  if (!require(package, character.only = T, quietly = T)) {
    install.packages(package)
    library(package, character.only = T)
  }
}

cAMPStd <- data.frame(cbind(Values = c(95.8, 95.3,   99.8,   123.3,  202.9,  619.8,  1214.2, 1519.1, 1509.2, 1523.3, 1595.2, 1625.1),
                 Concn = c(1000, 300, 100, 30, 10, 3, 1, 0.3, 0.1, .03,  0.01, 0)))
ui <- shinyUI(fluidPage(
  title = "HTRF Assay Analysis",
  titlePanel(tags$img(src = 'HTRF_Title.png', height = 70, width = 500)),
  sidebarLayout(sidebarPanel(
    textInput("text", h5(strong(span("Enter Data Folder Path", style = "color:blue"),span("*", style = "color:red"))), value = ""),
    uiOutput("select.folder"),
    fileInput("fileRD1", "Upload NECA titration data file"),
    fileInput("fileRD2", "Upload Schild Data file"),
    fluidRow(column(6, textInput("bi32",label = "Row 1 Mean", value = "")),
             column(6,textInput("bx29", label = "Row 23 Mean", value = ""))),
    fileInput("fileRD3", "Upload cAMP titration data file"),
    actionButton("exclude_reset", "Reset Std Curve Plot", class="btn-primary"),
    tags$br(),tags$br(),
    #h6(strong(span("Select Plate Map after plotting cAMP Std Curve.", style = "color:blue"))),
    fileInput("fileRD4", "Upload Plate Map file"),
    selectInput(inputId = "filter1", label = "Select Compound",multiple = FALSE, choices = list('Nil'), selected = 'Nil'),
    tags$br(),
    h6(strong(span("By:", style = "color:red"))),
    h5(strong(span("Raj Balakrishnan, Ph.D.", style = "color:blue"))),
    h6(strong(span("Scientific Computing Group, RWC.", style = "color:brown"))),width = 2),

  mainPanel(wellPanel(tabsetPanel(type = "tab",
      tabPanel("Data Input and Std Curve", uiOutput("Tab1")),
      tabPanel("HTRF Plots", uiOutput("Tab2")),
      tabPanel("HTRF Data Analysis", uiOutput("Tab3")),
      tabPanel("User Manual", uiOutput("pdfview"),style='overflow-y:scroll;overflow-x:scroll;')),
       style='width: 1200px; height: 840px')))))

server <-   function(input,session, output)
  {  session$onSessionEnded(stopApp)

  NonScientific <- function(l) {l <- format(l, scientific = FALSE); parse(text=l)}

  fpath <- reactive({
    validate(
      need(input$text != "", "Please Enter Data Folder Path")
    )
    fpathtext <- gsub("\\\\", "/", input$text);  paste(fpathtext,"/",sep = "")})

  output$Tab1 <- renderUI({
    tabsetPanel(id = "Data Sources",
      tabPanel("NECA titration data",DT::dataTableOutput("NECA"),style = "overflow-y:scroll; max-height: 900px"),
      tabPanel("Schild titration data",DT::dataTableOutput("SchildData",height = 700, width = "100%"),style = "overflow-y:scroll; max-height: 700px"),
      tabPanel("cAMP titration data",DT::dataTableOutput("cAMPR", width = 250),
               style = "overflow-y:scroll; max-height: 700px"),
      tabPanel("cAMP Std Curve",plotOutput("cAMPStdPlot", height = 450, width = 800,
               click = "plot1_click", brush = brushOpts(id = "plot1_brush")),
               DT::dataTableOutput("fitData", width = 800),
               style = "overflow-y:scroll; 'width: 900px; max-height: 840px"),
      tabPanel("Plate Map",DT::dataTableOutput("PlateMap"),
               style = "overflow-y:scroll; max-height: 840px"))
  })

  output$Tab2 <- renderUI({
    tabsetPanel(id = "Data Plots",
      tabPanel("HTRF Plot Conc-1",
               fluidRow(column(8,fluidRow(h5(strong(span(input$filter1, style = "color:darkviolet"))),
                                       plotOutput("HTRFPlot1", height = 500,  width = 770)),
                               fluidRow(verbatimTextOutput("fitData1"), style = "overflow-y:scroll; max-height: 700px")),
                        column(4, DT::dataTableOutput("ExcludData1"),style = "overflow-y:scroll; max-height: 700px"))),
      tabPanel("HTRF Plot Conc-2", fluidRow(column(8,fluidRow(h5(strong(span(input$filter1, style = "color:darkviolet"))),
                                          plotOutput("HTRFPlot2", height = 500,  width = 770)),
                               fluidRow(verbatimTextOutput("fitData2"), style = "overflow-y:scroll; max-height: 700px")),
                        column(4, DT::dataTableOutput("ExcludData2"),style = "overflow-y:scroll; max-height: 700px"))),
      tabPanel("HTRF Plot Conc-3", fluidRow(column(8,fluidRow(h5(strong(span(input$filter1, style = "color:darkviolet"))),
                                          plotOutput("HTRFPlot3", height = 500,  width = 770)),
                               fluidRow(verbatimTextOutput("fitData3"), style = "overflow-y:scroll; max-height: 700px")),
                        column(4, DT::dataTableOutput("ExcludData3"),style = "overflow-y:scroll; max-height: 700px"))),
      tabPanel("HTRF Plot Conc-4", fluidRow(column(8,fluidRow(h5(strong(span(input$filter1, style = "color:darkviolet"))),
                                          plotOutput("HTRFPlot4", height = 500,  width = 770)),
                               fluidRow(verbatimTextOutput("fitData4"), style = "overflow-y:scroll; max-height: 700px")),
                        column(4, DT::dataTableOutput("ExcludData4"),style = "overflow-y:scroll; max-height: 700px"))),
      tabPanel("HTRF Plot Conc-0", fluidRow(column(8,fluidRow(h5(strong(span(input$filter1, style = "color:darkviolet"))),
                                plotOutput("HTRFPlot5", height = 500,  width = 750)),
                               fluidRow(verbatimTextOutput("fitData5"), style = "overflow-y:scroll; max-height: 700px")),
                        column(4, DT::dataTableOutput("ExcludData5"),style = "overflow-y:scroll; max-height: 700px"))))
  })

  output$Tab3 <- renderUI({
    tabsetPanel(id = "Data Analysis",
                tabPanel("Dose Response Curve Fit Data",DT::dataTableOutput("Parameters"),style = "overflow-y:scroll; max-height: 840px"),
                tabPanel("Linear Fit", column(8, plotOutput("LinFitPlot", height = 450,  width = 800)),
                         column(8,  DT::dataTableOutput("LinFitData", width = 800))),
                tabPanel("Global Fit Data",DT::dataTableOutput("GlobalParameters"),style = "overflow-y:scroll; max-height: 840px"),
                tabPanel("Gobal Fit", column(8, plotOutput("GlobalFitPlot", height = 500,  width = 850)),
                         column(8,  DT::dataTableOutput("GlobalFitData", width = 850))))
  })

  output$NECA <- DT::renderDataTable({
    file1 <- input$fileRD1 ;   if (is.null(file1)) {return()}
    NECA<- read.delim(file1$datapath, header = T, stringsAsFactors = FALSE, skip = 1)
    NECA <- NECA[(1:6),];NECA <- NECA[,-26]; rownames(NECA) <- NECA[,1];NECA <- NECA[,-1];NECA <- NECA[-(1:5),];NECA <- NECA * 1000
    write.csv(NECA,paste("NECAData.csv", sep = ""),na = "0")
    DT::datatable(NECA,  options = list(pageLength = 100, dom = 't'))
  })

  output$SchildData <- DT::renderDataTable({
    file2 <- input$fileRD2
    if (is.null(file2)) {return()}
    Schild <- read.delim(file2$datapath, header = T, stringsAsFactors = FALSE, skip = 1)
    Schild <- Schild[(1:16),];Schild <- Schild[,-26]; rownames(Schild) <- Schild[,1];
    Schild <- Schild[,-1];Schild <- Schild * 1000
    write.csv(Schild,paste("SchildData.csv", sep = ""),na = "0")
    ydataf<- read.csv(paste("SchildData.csv", sep = ""))   #SchildData input
    ydataT <- t(ydataf); colnames(ydataT) <- ydataT[1,1:16]; ydataT <- ydataT[-1,]
    bx29 <- mean(as.numeric(ydataT[23,]));   bi32 <- mean(as.numeric(ydataT[1,]))
    updateTextInput(session, "bi32", value = bi32 )
    updateTextInput(session, "bx29", value = bx29)
    DT::datatable(ydataT,  options = list(pageLength = 100, dom = 't'))
  })

  output$cAMPR <- DT::renderDataTable({ #renderPlot({
    file3 <- input$fileRD3;if (is.null(file3)) {return()}
    CAMP <- read.delim(file3$datapath, header = T, stringsAsFactors = FALSE, skip = 1)
    cAMPStd <- as.data.frame(as.matrix(c(1.00E+03,3.00E+02,1.00E+02,3.00E+01,1.00E+01,3.00E+00,
                                         1.00E+00,3.00E-01,1.00E-01,3.00E-02,1.00E-02,0)))
    names(cAMPStd) <- "Concn"
    CAMP <- CAMP[(1:16),];CAMP <- CAMP[,-26]; rownames(CAMP) <- CAMP[,1];
    CAMP <- CAMP[,-1];CAMP <- CAMP[-(1:15),];CAMP <- format(CAMP * 1000,digits = 4, nsmall = 4)
    j <- 1; tmp <- 0; for (i in seq(1, ncol(CAMP), 2))
    {tmp[j] =(mean(CAMP[1,i]:CAMP[1,i+1])); j <- j+1}
    CAMPResponse <- as.data.frame(tmp); tmp <- NULL
    if (length(cAMPStd) == length(CAMPResponse))
     {cAMPStd["Values"] <- format(CAMPResponse$tmp,digits = 4, nsmall = 4)}
    else {stop("Number of Std Concns do not match with Std titration data")}
    write.csv(cAMPStd,paste("cAMPStdCurveData.csv", sep = ""),na = "0")
    write.csv(cAMPStd,paste("cAMPData.csv", sep = ""),na = "0")
    DT::datatable(cAMPStd,  options = list(pageLength = 100, dom = 'rtip'))
  })

  output$PlateMap <- DT::renderDataTable({
    file4 <- input$fileRD4
    if (is.null(file4)) {return()}
    pmap <- read.delim(file4$datapath, header = T, stringsAsFactors = FALSE)
    posn <- as.data.frame(which(pmap == "StarConcentration", arr.ind=TRUE))
    pmap2 <- pmap;  pmap2 <- pmap[-(1:posn[1,1]-1),];  pmap2 <- pmap2[,1:3]
    colnames(pmap2) <- pmap2[1,]; pmap2 <- pmap2[-1,]; pmap <- pmap[(1:posn[1,1]-1),]
    compoundNames <- as.data.frame(pmap$BatchID, quote = F )
    colnames(compoundNames) <- "compoundNames"
    pmap$Concentration1 <- as.numeric(pmap$Concentration1)
    pmap$Concentration2 <- as.numeric(pmap$Concentration2)
    pmap$Concentration3 <- as.numeric(pmap$Concentration3)
    pmap$Concentration4 <- as.numeric(pmap$Concentration4)
    updateSelectInput(session, "filter1",label = "Select Compound BatchID", choices = pmap$BatchID)
    write.csv(pmap, paste("PlateMap.csv", sep = ""))
    ydataf<- read.csv(paste("SchildData.csv", sep = ""))   #SchildData input
    ydataT <- t(ydataf); colnames(ydataT) <- ydataT[1,1:16]; ydataT <- ydataT[-1,]
    necaf <- read.csv(paste("NECAData.csv", sep = ""))     #NECAData input
    necaf <- necaf[,-1]; necafT <- t(necaf)
    pmap <- read.csv(paste("PlateMap.csv", sep = ""))      #PlateMap input
    row.names(pmap) <-pmap[,1];   pmap <- pmap[,-1]

    bi29 <- mean(as.numeric(necafT[21:22,]))
    bj32 <- mean(as.numeric(necafT[1:2,]))

    bi32 = as.numeric(input$bi32)
    bx29 = as.numeric(input$bx29)

    necafT <- necafT[-23:-24];  ydataT <- ydataT[-23:-24,]

    HTRF1 <- ydataT[,1:4]; HTRF1 <- cbind(HTRF1,necafT)
    HTRF2 <- ydataT[,5:8]; HTRF2 <- cbind(HTRF2,necafT)
    HTRF3 <- ydataT[,9:12]; HTRF3 <- cbind(HTRF3,necafT) ;
    HTRF4 <- ydataT[,13:16]; HTRF4 <- cbind(HTRF4,necafT)

    cAMPAcm1 <- HTRF1; cAMPAcm2 <- HTRF2; cAMPAcm3 <- HTRF3; cAMPAcm4 <- HTRF4
    PctEfct1 <- HTRF1; PctEfct2 <- HTRF2; PctEfct3 <- HTRF3; PctEfct4 <- HTRF4

    p <- read.csv(paste("cAMP_Fit.csv", sep = ""));  p <- p[,-1]   #cAMP_Fit Data input
    ymax <- p$ymax ;    ymin <- p$ymin;    ec50 <- p$ec50

    for (j in 1:5){
      for (i in 1:length(HTRF1[,1])){
        cAMPAcm1[i,j]  <- as.numeric((ec50*(-as.numeric(HTRF1[i,j]) + ymin))/((as.numeric(HTRF1[i,j])-ymin)+ abs(ymax))/1000)
        cAMPAcm2[i,j]  <- as.numeric((ec50*(-as.numeric(HTRF2[i,j]) + ymin))/((as.numeric(HTRF2[i,j])-ymin)+ abs(ymax))/1000)
        cAMPAcm3[i,j]  <- as.numeric((ec50*(-as.numeric(HTRF3[i,j]) + ymin))/((as.numeric(HTRF3[i,j])-ymin)+ abs(ymax))/1000)
        cAMPAcm4[i,j]  <- as.numeric((ec50*(-as.numeric(HTRF4[i,j]) + ymin))/((as.numeric(HTRF4[i,j])-ymin)+ abs(ymax))/1000)}}

    bi30CalcValue <- as.numeric((ec50*(-as.numeric(bi29) + ymin))/((as.numeric(bi29)-ymin)+ abs(ymax))/1000)
    bi33CalcValue <- as.numeric((ec50*(-as.numeric(bi32) + ymin))/((as.numeric(bi32)-ymin)+ abs(ymax))/1000)
    bx30CalcValue <- as.numeric((ec50*(-as.numeric(bx29) + ymin))/((as.numeric(bx29)-ymin)+ abs(ymax))/1000)
    bj33CalcValue <- as.numeric((ec50*(-as.numeric(bj32) + ymin))/((as.numeric(bj32)-ymin)+ abs(ymax))/1000)

    for (j in 1:5){
      for (i in 1:length(HTRF1[,1])){
        PctEfct1[i,j]  <- format(as.numeric((100 *(as.numeric(cAMPAcm1[i,j])-bx30CalcValue))/(bi33CalcValue-bx30CalcValue)),digits=4, nsmall=4)
        PctEfct2[i,j]  <- format(as.numeric((100 *(as.numeric(cAMPAcm2[i,j])-bx30CalcValue))/(bi33CalcValue-bx30CalcValue)),digits=4, nsmall=4)
        PctEfct3[i,j]  <- format(as.numeric((100 *(as.numeric(cAMPAcm3[i,j])-bx30CalcValue))/(bi33CalcValue-bx30CalcValue)),digits=4, nsmall=4)
        PctEfct4[i,j]  <- format(as.numeric((100 *(as.numeric(cAMPAcm4[i,j])-bx30CalcValue))/(bi33CalcValue-bx30CalcValue)),digits=4, nsmall=4)}}

    xdata <-  c(-5.000E+00,	-5.301E+00,	-5.602E+00,	-5.903E+00,	-6.204E+00,	-6.505E+00,	-6.806E+00,
                -7.107E+00,	-7.408E+00,	-7.709E+00,	-8.010E+00,	-8.311E+00,	-8.612E+00,	-8.913E+00,
                -9.214E+00, -9.515E+00,	-9.816E+00,	-1.012E+01,	-1.042E+01,	-1.072E+01,	-1.102E+01,	-1.132E+01)

    PctEfct1 <- cbind(xdata,PctEfct1)
    PctEfct2 <- cbind(xdata,PctEfct2)
    PctEfct3 <- cbind(xdata,PctEfct3)
    PctEfct4 <- cbind(xdata,PctEfct4)

    write.csv(PctEfct1, paste(gsub(" ", "",pmap$BatchID[1]), ".csv", sep = ""))
    write.csv(PctEfct2, paste(gsub(" ", "",pmap$BatchID[2]), ".csv", sep = ""))
    write.csv(PctEfct3, paste(gsub(" ", "",pmap$BatchID[3]), ".csv", sep = ""))
    write.csv(PctEfct4, paste(gsub(" ", "",pmap$BatchID[4]), ".csv", sep = ""))


    params <- matrix(data = NA, nrow = 20, ncol = 6); l=1
    for (i in 1:4)
      for (j in 1:5){params[l,1] <- gsub(" ", "",pmap$BatchID[i])
        params[l,2] <- pmap[i,j+1]
        params[l,3] <- "0";  params[l,4] <- "0"
        params[l,5] <- "0";  params[l,6] <- "0"; l = l+1}
    colnames(params) <- c("BatchID","Concn","Top", "Bottom", "HillSlope", "logEC50")
    write.csv(params,paste("Params.csv", sep = ""))



    Globals <- matrix(data = NA, nrow = 4, ncol = 11)
    for (i in 1:4) {Globals[i,1] <- gsub(" ", "",pmap$BatchID[i])
      Globals[i,2] <- "0"; Globals[i,3] <- "0"
      Globals[i,4] <- "0"; Globals[i,5] <- "0";Globals[i,6] <- "0"
      Globals[i,7] <- gsub(" ", "",pmap$Concentration1[i])
      Globals[i,8] <- gsub(" ", "",pmap$Concentration2[i])
      Globals[i,9] <- gsub(" ", "",pmap$Concentration3[i])
      Globals[i,10] <- gsub(" ", "",pmap$Concentration4[i])
      Globals[i,11] <- gsub(" ", "",pmap$Concentration5[i])}
    colnames(Globals) <- c("BatchID","Top", "Bottom", "pa2", "EC50", "LogEC50","conc1","conc2","conc3","conc4","conc5")
    for (i in 1:4) {Globals[i,4] <- format(-log10(mean(as.numeric(Globals[i,7:11]))), digits = 5)}
    Globals[, c('BatchID','Top','Bottom','pa2','EC50','LogEC50','conc1','conc2','conc3','conc4','conc5')]
    write.table(Globals,paste("Globals.csv", sep = ""), quote = F, col.names = T, sep = ",")
    minx <- matrix(data = NA, nrow = 4, ncol = 6); maxx <- matrix(data = NA, nrow = 4, ncol = 6)
    for (i in 1:4) {minx[i,1] <- gsub(" ", "",pmap$BatchID[i])}
    for (i in 1:4) {maxx[i,1] <- gsub(" ", "",pmap$BatchID[i])}
    colnames(minx) <- c("BatchID","Conc1","Conc2","Conc3","Conc4","Conc5")
    colnames(maxx) <- c("BatchID","Conc1","Conc2","Conc3","Conc4","Conc5")
    write.csv(maxx,paste("Top5.csv", sep = ""))
    write.csv(minx,paste("Bot5.csv", sep = ""))

    DT::datatable(pmap,  options = list(pageLength = 10, dom = 't'))
  })

########################## REACTIVE VALUES ############################################

  vals <- reactiveValues(keeprows = rep(TRUE, nrow(cAMPStd)))
  pmapval <- reactive({
    pmapv <- read.csv(paste("PlateMap.csv", sep = ""))
    pmapv <- pmapv[,-1] })

#### HTRF Plot 1  ############  HTRF Plot 1  ########################################
  output$ExcludData1 <- DT::renderDataTable({
    if (input$filter1 =='Nil') {return()}
    COMPD1 <- as.data.frame(read.csv(paste(input$filter1,".csv", sep = "")))
    COMPD1 <- COMPD1[,-1]; COMPD1 <- COMPD1[c(1,2)]
    names(COMPD1)[1] <- "Log[NECA]/(M)"
    rnum <- which(grepl(input$filter1, pmapval()$BatchID))
    names(COMPD1)[2] <- pmapval()$Concentration1[rnum]
    DT::datatable(COMPD1,options = list(pageLength = 22, dom = 'tip'))
  })
  output$fitData1 <- renderPrint({
    if (input$filter1 =='Nil') {return()}
    COMPD1 <- as.data.frame(read.csv(paste(input$filter1,".csv", sep = "")))
    COMPD1 <- COMPD1[,-1]
    names(COMPD1) <- c("xdata","A","B","C","D","necafT")
    s = input$ExcludData1_rows_selected
    if (length(s)) {
      keep    <- COMPD1[-s, , drop = FALSE]
      exclude <- COMPD1[!s, , drop = FALSE]}
    else {
      s = rep(TRUE, nrow(COMPD1))
      keep    <- COMPD1[s, , drop = FALSE]
      exclude <- COMPD1[!s, , drop = FALSE]
    }
    maxy <- max(as.numeric(keep$A))
    miny <- min(as.numeric(keep$A))
    estimatedY50 = (maxy - miny)/2
    estimatedDistY50 = abs(as.numeric(keep$A)-estimatedY50)
    locationNearest = which.min(estimatedDistY50)
    estimatedX50 = keep$xdata[locationNearest]

    maxx <- as.data.frame(read.csv(paste("Top5.csv", sep = "")))
    minx <- as.data.frame(read.csv(paste("Bot5.csv", sep = "")))
    maxx <- maxx[,-1] ; minx <- minx[,-1]
    mxnum <- which(grepl(input$filter1, maxx$BatchID));
    maxx$Conc1[mxnum] = maxy
    minx$Conc1[mxnum] = miny
    write.csv(maxx,paste("Top5.csv", sep = ""));
    write.csv(minx,paste("Bot5.csv", sep = ""))
    fit1 <- nls(A ~ BOTTOM + (TOP-BOTTOM)/(1+10**((logEC50-xdata)*hillSlope)), data= keep,
     start=list(TOP = max(as.numeric(keep$A)), BOTTOM = min(as.numeric(keep$A)), hillSlope = 1, logEC50 = estimatedX50))
    rnum <- which(grepl(input$filter1, pmapval()$BatchID))
    concval <- pmapval()$Concentration1[rnum]
    sink(paste(input$filter1,"_",concval,".txt", sep = ""));  print(fit1);  sink()
    pm1 <- read.table(paste(input$filter1,"_",concval,".txt", sep = ""), skip = 3, nrows = 1, header = T)
    rnum <- which(grepl(input$filter1, pmapval()$BatchID))
    Concn <- pmapval()$Concentration1[rnum]
    BatchID <- input$filter1
    pm1 <- cbind(BatchID, Concn,pm1); pm1 <- as.data.frame(pm1)
    write.csv(pm1, paste(input$filter1,"_",concval,".csv", sep = ""))
    paramval <- read.csv(paste("Params.csv", sep = ""), header = T); paramval <- paramval[,-1]
    for (i in 1:20)
      if(paramval$BatchID[i] == gsub(" ", "", input$filter1) & paramval$Concn[i] == pmapval()$Concentration1[rnum] )
      {paramval$Top[i] <- pm1$TOP[1];      paramval$Bottom[i] <- pm1$BOTTOM[1]
      paramval$HillSlope[i] <- pm1$hillSlope[1];    paramval$logEC50[i] <- pm1$logEC50[1]}
    write.csv(paramval,paste("Params.csv", sep = ""))
    fit1
    })
    output$HTRFPlot1 <- renderPlot({
      if (input$filter1 =='Nil') {return()}
      COMPD1 <- as.data.frame(read.csv(paste(input$filter1,".csv", sep = "")))
      COMPD1 <- COMPD1[,-1]
      names(COMPD1) <- c("xdata","A","B","C","D","necafT")
      s = input$ExcludData1_rows_selected
      if (length(s)) {
        keep    <- COMPD1[-s, , drop = FALSE]
        exclude <- COMPD1[s, , drop = FALSE]}
        else {
          s = rep(TRUE, nrow(COMPD1))
          keep    <- COMPD1[s, , drop = FALSE]
          exclude <- COMPD1[!s, , drop = FALSE]
        }
      rnum <- which(grepl(input$filter1, pmapval()$BatchID))
      concval <- pmapval()$Concentration1[rnum]
      write.csv(keep, paste(input$filter1,"_",concval,"keep1.csv", sep = ""));   #write.csv(exclude, paste(input$filter1,"_",concval,"exclude1.csv", sep = ""))
      estimatedY50 = (max(as.numeric(keep$A))-min(as.numeric(keep$A)))/2
      estimatedDistY50 = abs(as.numeric(keep$A)-estimatedY50)
      locationNearest = which.min(estimatedDistY50)
      estimatedX50 = keep$xdata[locationNearest]
       fit1 <- nls(A ~ BOTTOM + (TOP-BOTTOM)/(1+10**((logEC50-xdata)*hillSlope)), data= keep,
                  start=list(TOP = max(as.numeric(keep$A)), BOTTOM = min(as.numeric(keep$A)), hillSlope = 1, logEC50 = estimatedX50))
       SmoothX <- seq(min(keep$xdata), max(keep$xdata), length = 100)
       Title =paste(input$filter1,"_",concval, sep = "")
       par(mar=c(5, 4, 4, 2) + 0.1)
       jpeg(paste(input$filter1,"_",concval,".jpg", sep = ""))
       plot(A ~ xdata , data = keep, col = 4, pch =21, lwd=2, main = Title,
            xlab="Log[NECA]/[M]", ylab="%Max[cAMP accumulation/cells")
       lines(SmoothX, predict(fit1,list(xdata=SmoothX)), col='red', lwd=2)
       dev.off()
       plot(A ~ xdata , data = keep, col = 4, pch =21, lwd=2, main = Title,
            xlab="Log[NECA]/[M]", ylab="%Max[cAMP accumulation/cells")
       lines(SmoothX, predict(fit1,list(xdata=SmoothX)), col='red', lwd=2)
    })

#### HTRF Plot 2  ############  HTRF Plot 2  ########################################
    output$ExcludData2 <- DT::renderDataTable({
      COMPD1 <- as.data.frame(read.csv(paste(input$filter1,".csv", sep = "")))
      COMPD1 <- COMPD1[,-1]; COMPD1 <- COMPD1[c(1,3)]
      names(COMPD1)[1] <- "Log[NECA]/(M)"
      rnum <- which(grepl(input$filter1, pmapval()$BatchID))
      names(COMPD1)[2] <- pmapval()$Concentration2[rnum]
      DT::datatable(COMPD1,options = list(pageLength = 22, dom = 'tip'))
    })
    output$fitData2 <- renderPrint({
      COMPD1 <- as.data.frame(read.csv(paste(input$filter1,".csv", sep = "")))
      COMPD1 <- COMPD1[,-1]
      names(COMPD1) <- c("xdata","A","B","C","D","necafT")
      s = input$ExcludData2_rows_selected
      if (length(s)) {
        keep    <- COMPD1[-s, , drop = FALSE]
        exclude <- COMPD1[!s, , drop = FALSE]}
      else {s = rep(TRUE, nrow(COMPD1))
      keep    <- COMPD1[s, , drop = FALSE]
      exclude <- COMPD1[!s, , drop = FALSE]}
      estimatedY50 = (max(as.numeric(keep$B))-min(as.numeric(keep$B)))/2
      estimatedDistY50 = abs(as.numeric(keep$B)-estimatedY50)
      locationNearest = which.min(estimatedDistY50)
      estimatedX50 = keep$xdata[locationNearest]

      maxx <- as.data.frame(read.csv(paste("Top5.csv", sep = ""))) ;
      minx <- as.data.frame(read.csv(paste("Bot5.csv", sep = "")))
      maxx <- maxx[,-1] ; minx <- minx[,-1]
      mxnum <- which(grepl(input$filter1, maxx$BatchID));
      maxx$Conc2[mxnum] = max(as.numeric(keep$B)); minx$Conc2[mxnum] = min(as.numeric(keep$B))
      write.csv(maxx,paste("Top5.csv", sep = ""))
      write.csv(minx,paste("Bot5.csv", sep = ""))
      fit2 <- nls(B ~ BOTTOM + (TOP-BOTTOM)/(1+10**((logEC50-xdata)*hillSlope)), data= keep,
      start=list(TOP = max(as.numeric(keep$B)), BOTTOM = min(as.numeric(keep$B)), hillSlope = 1, logEC50 = estimatedX50))
      rnum <- which(grepl(input$filter1, pmapval()$BatchID))
      concval <- pmapval()$Concentration2[rnum]
      sink(paste(input$filter1,"_",concval,".txt", sep = ""));  print(fit2);  sink()
      pm1 <- read.table(paste(input$filter1,"_",concval,".txt", sep = ""), skip = 3, nrows = 1, header = T)
      rnum <- which(grepl(input$filter1, pmapval()$BatchID))
      Concn <- pmapval()$Concentration2[rnum]
      BatchID <- input$filter1
      pm1 <- cbind(BatchID, Concn,pm1)
      pm1 <- as.data.frame(pm1)
      write.csv(pm1, paste(input$filter1,"_",concval,".csv", sep = ""))
      paramval <- read.csv(paste("Params.csv", sep = ""))
      paramval <- paramval[,-1]
      for (i in 1:20)
        if(paramval$BatchID[i] == gsub(" ", "", input$filter1) & paramval$Concn[i] == pmapval()$Concentration2[rnum] )
        {paramval$Top[i] <- pm1$TOP[1]
        paramval$Bottom[i] <- pm1$BOTTOM[1]
        paramval$HillSlope[i] <- pm1$hillSlope[1]
        paramval$logEC50[i] <- pm1$logEC50[1]}
      write.csv(paramval,paste("Params.csv", sep = ""))
      fit2
    })
    output$HTRFPlot2 <- renderPlot({
      COMPD1 <- as.data.frame(read.csv(paste(input$filter1,".csv", sep = "")))
      COMPD1 <- COMPD1[,-1]
      names(COMPD1) <- c("xdata","A","B","C","D","necafT")
      s = input$ExcludData2_rows_selected
      par(mar = c(4, 4, 1, .1))
      if (length(s)) {
        keep    <- COMPD1[-s, , drop = FALSE]
        exclude <- COMPD1[!s, , drop = FALSE]}
      else {
        s = rep(TRUE, nrow(COMPD1))
        keep    <- COMPD1[s, , drop = FALSE]
        exclude <- COMPD1[!s, , drop = FALSE]
      }
      rnum <- which(grepl(input$filter1, pmapval()$BatchID))
      concval <- pmapval()$Concentration2[rnum]
      write.csv(keep, paste(input$filter1,"_",concval,"keep2.csv", sep = ""));  #write.csv(exclude, paste(input$filter1,"_",concval,"exclude2.csv", sep = ""))
      estimatedY50 = (max(as.numeric(keep$B))-min(as.numeric(keep$B)))/2
      estimatedDistY50 = abs(as.numeric(keep$B)-estimatedY50)
      locationNearest = which.min(estimatedDistY50)
      estimatedX50 = keep$xdata[locationNearest]
      fit2 <- nls(B ~ BOTTOM + (TOP-BOTTOM)/(1+10**((logEC50-xdata)*hillSlope)), data= keep,
                  start=list(TOP = max(as.numeric(keep$B)), BOTTOM = min(as.numeric(keep$B)), hillSlope = 1, logEC50 = estimatedX50))
      SmoothX <- seq(min(keep$xdata), max(keep$xdata), length = 100)
      Title =paste(input$filter1,"_",concval, sep = "")
      par(mar=c(5, 4, 4, 2) + 0.1)
      jpeg(paste(input$filter1,"_",concval,".jpg", sep = ""))
      plot(B ~ xdata , data = keep, col = 4, pch =21, lwd=2, main = Title,
           xlab="Log[NECA]/[M]", ylab="%Max[cAMP accumulation/cells")
      lines(SmoothX, predict(fit2,list(xdata=SmoothX)), col='red', lwd=2)
      dev.off()
      plot(B ~ xdata , data = keep, col = 4, pch =21, lwd=2, main = Title,
           xlab="Log[NECA]/[M]", ylab="%Max[cAMP accumulation/cells")
      lines(SmoothX, predict(fit2,list(xdata=SmoothX)), col='red', lwd=2)
    })

#### HTRF Plot 3  ############  HTRF Plot 3  ########################################
    output$ExcludData3 <- DT::renderDataTable({
      COMPD1 <- as.data.frame(read.csv(paste(input$filter1,".csv", sep = "")))
      COMPD1 <- COMPD1[,-1]; COMPD1 <- COMPD1[c(1,4)]
      names(COMPD1)[1] <- "Log[NECA]/(M)"
      rnum <- which(grepl(input$filter1, pmapval()$BatchID))
      names(COMPD1)[2] <- pmapval()$Concentration3[rnum]
      DT::datatable(COMPD1,options = list(pageLength = 22, dom = 'tip'))
    })
    output$fitData3 <- renderPrint({
      COMPD1 <- as.data.frame(read.csv(paste(input$filter1,".csv", sep = "")))
      COMPD1 <- COMPD1[,-1]
      names(COMPD1) <- c("xdata","A","B","C","D","necafT")
      s = input$ExcludData3_rows_selected
      if (length(s)) {
        keep    <- COMPD1[-s, , drop = FALSE]
        exclude <- COMPD1[!s, , drop = FALSE]}
      else {s = rep(TRUE, nrow(COMPD1))
      keep    <- COMPD1[s, , drop = FALSE]
      exclude <- COMPD1[!s, , drop = FALSE]}
      estimatedY50 = (max(as.numeric(keep$C))-min(as.numeric(keep$C)))/2
      estimatedDistY50 = abs(as.numeric(keep$C)-estimatedY50)
      locationNearest = which.min(estimatedDistY50)
      estimatedX50 = keep$xdata[locationNearest]
      maxx <- as.data.frame(read.csv(paste("Top5.csv", sep = "")))
      minx <- as.data.frame(read.csv(paste("Bot5.csv", sep = "")))
      maxx <- maxx[,-1] ; minx <- minx[,-1]
      mxnum <- which(grepl(input$filter1, maxx$BatchID));
      maxx$Conc3[mxnum] = max(as.numeric(keep$C)); minx$Conc3[mxnum] = min(as.numeric(keep$C))
      write.csv(maxx,paste("Top5.csv", sep = ""));
      write.csv(minx,paste("Bot5.csv", sep = ""))

      fit3 <- nls(C ~ BOTTOM + (TOP-BOTTOM)/(1+10**((logEC50-xdata)*hillSlope)), data= keep,
       start=list(TOP = max(as.numeric(keep$C)), BOTTOM = min(as.numeric(keep$C)), hillSlope = 1, logEC50 = estimatedX50))
      rnum <- which(grepl(input$filter1, pmapval()$BatchID))
      concval <- pmapval()$Concentration3[rnum]
      sink(paste(input$filter1,"_",concval,".txt", sep = ""));  print(fit3);  sink()
      pm1 <- read.table(paste(input$filter1,"_",concval,".txt", sep = ""), skip = 3, nrows = 1, header = T)
      rnum <- which(grepl(input$filter1, pmapval()$BatchID))
      Concn <- pmapval()$Concentration3[rnum]
      BatchID <- input$filter1
      pm1 <- cbind(BatchID, Concn,pm1)
      pm1 <- as.data.frame(pm1)
      write.csv(pm1, paste(input$filter1,"_",concval,".csv", sep = ""))
      paramval <- read.csv(paste("Params.csv", sep = ""))
      paramval <- paramval[,-1]
      for (i in 1:20)
        if(paramval$BatchID[i] == gsub(" ", "", input$filter1) & paramval$Concn[i] == pmapval()$Concentration3[rnum] )
        {paramval$Top[i] <- pm1$TOP[1]
        paramval$Bottom[i] <- pm1$BOTTOM[1]
        paramval$HillSlope[i] <- pm1$hillSlope[1]
        paramval$logEC50[i] <- pm1$logEC50[1]}
      write.csv(paramval,paste("Params.csv", sep = ""))
      fit3
    })

    output$HTRFPlot3 <- renderPlot({
      COMPD1 <- as.data.frame(read.csv(paste(input$filter1,".csv", sep = "")))
      COMPD1 <- COMPD1[,-1]
      names(COMPD1) <- c("xdata","A","B","C","D","necafT")
      s = input$ExcludData3_rows_selected
      if (length(s)) {
        keep    <- COMPD1[-s, , drop = FALSE]
        exclude <- COMPD1[!s, , drop = FALSE]}
      else {
        s = rep(TRUE, nrow(COMPD1))
        keep    <- COMPD1[s, , drop = FALSE]
        exclude <- COMPD1[!s, , drop = FALSE]
      }
      rnum <- which(grepl(input$filter1, pmapval()$BatchID))
      concval <- pmapval()$Concentration3[rnum]
      write.csv(keep, paste(input$filter1,"_",concval,"keep3.csv", sep = ""));#write.csv(exclude, paste(input$filter1,"_",concval,"exclude3.csv", sep = ""))
      estimatedY50 = (max(as.numeric(keep$C))-min(as.numeric(keep$C)))/2
      estimatedDistY50 = abs(as.numeric(keep$C)-estimatedY50)
      locationNearest = which.min(estimatedDistY50)
      estimatedX50 = keep$xdata[locationNearest]
      fit3 <- nls(C ~ BOTTOM + (TOP-BOTTOM)/(1+10**((logEC50-xdata)*hillSlope)), data= keep,
                  start=list(TOP = max(as.numeric(keep$C)), BOTTOM = min(as.numeric(keep$C)), hillSlope = 1, logEC50 = estimatedX50))
      SmoothX <- seq(min(keep$xdata), max(keep$xdata), length = 100)
      Title =paste(input$filter1,"_",concval, sep = "")
      par(mar=c(5, 4, 4, 2) + 0.1)
      jpeg(paste(input$filter1,"_",concval,".jpg", sep = ""))
        plot(C ~ xdata , data = keep, col = 4, pch =21, lwd=2, main = Title,
             xlab="Log[NECA]/[M]", ylab="%Max[cAMP accumulation/cells")
        lines(SmoothX, predict(fit3,list(xdata=SmoothX)), col='red', lwd=2)
      dev.off()
      plot(C ~ xdata , data = keep, col = 4, pch =21, lwd=2, main = Title,
           xlab="Log[NECA]/[M]", ylab="%Max[cAMP accumulation/cells")
      lines(SmoothX, predict(fit3,list(xdata=SmoothX)), col='red', lwd=2)

    })

#### HTRF Plot 4  ############  HTRF Plot 4  ########################################
    output$ExcludData4 <- DT::renderDataTable({
      COMPD1 <- as.data.frame(read.csv(paste(input$filter1,".csv", sep = "")))
      COMPD1 <- COMPD1[,-1]; COMPD1 <- COMPD1[c(1,5)]
      names(COMPD1)[1] <- "Log[NECA]/(M)"
      rnum <- which(grepl(input$filter1, pmapval()$BatchID))
      names(COMPD1)[2] <- pmapval()$Concentration4[rnum]
      DT::datatable(COMPD1,options = list(pageLength = 22, dom = 'tip'))
    })
    output$fitData4 <- renderPrint({
      COMPD1 <- as.data.frame(read.csv(paste(input$filter1,".csv", sep = "")))
      COMPD1 <- COMPD1[,-1]
      names(COMPD1) <- c("xdata","A","B","C","D","necafT")
      s = input$ExcludData4_rows_selected
      if (length(s)) {
        keep    <- COMPD1[-s, , drop = FALSE]
        exclude <- COMPD1[!s, , drop = FALSE]}
      else {s = rep(TRUE, nrow(COMPD1))
      keep    <- COMPD1[s, , drop = FALSE]
      exclude <- COMPD1[!s, , drop = FALSE]}
      estimatedY50 = (max(as.numeric(keep$D))-min(as.numeric(keep$D)))/2
      estimatedDistY50 = abs(as.numeric(keep$D)-estimatedY50)
      maxx <- as.data.frame(read.csv(paste("Top5.csv", sep = "")))
      minx <- as.data.frame(read.csv(paste("Bot5.csv", sep = "")))
      maxx <- maxx[,-1] ; minx <- minx[,-1]
      mxnum <- which(grepl(input$filter1, maxx$BatchID));
      maxx$Conc4[mxnum] = max(as.numeric(keep$D)); minx$Conc4[mxnum] = min(as.numeric(keep$D))
      write.csv(maxx,paste("Top5.csv", sep = ""));
      write.csv(minx,paste("Bot5.csv", sep = ""))
      locationNearest = which.min(estimatedDistY50)
      estimatedX50 = keep$xdata[locationNearest]
      fit4 <- nls(D ~ BOTTOM + (TOP-BOTTOM)/(1+10**((logEC50-xdata)*hillSlope)), data= keep,
                  start=list(TOP = max(as.numeric(keep$D)), BOTTOM = min(as.numeric(keep$D)), hillSlope = 1, logEC50 = estimatedX50))
      rnum <- which(grepl(input$filter1, pmapval()$BatchID))
      concval <- pmapval()$Concentration4[rnum]
      sink(paste(input$filter1,"_",concval,".txt", sep = ""));  print(fit4);  sink()
      pm1 <- read.table(paste(input$filter1,"_",concval,".txt", sep = ""), skip = 3, nrows = 1, header = T)
      rnum <- which(grepl(input$filter1, pmapval()$BatchID))
      Concn <- pmapval()$Concentration4[rnum]
      BatchID <- input$filter1
      pm1 <- cbind(BatchID, Concn,pm1)
      pm1 <- as.data.frame(pm1)
      write.csv(pm1, paste(input$filter1,"_",concval,".csv", sep = ""))
      paramval <- read.csv(paste("Params.csv", sep = ""))
      paramval <- paramval[,-1]
      for (i in 1:20)
        if(paramval$BatchID[i] == gsub(" ", "", input$filter1) & paramval$Concn[i] == pmapval()$Concentration4[rnum] )
        {paramval$Top[i] <- pm1$TOP[1]
        paramval$Bottom[i] <- pm1$BOTTOM[1]
        paramval$HillSlope[i] <- pm1$hillSlope[1]
        paramval$logEC50[i] <- pm1$logEC50[1]}
      write.csv(paramval,paste("Params.csv", sep = ""))
      fit4
    })

    output$HTRFPlot4 <- renderPlot({
      COMPD1 <- as.data.frame(read.csv(paste(input$filter1,".csv", sep = "")))
      COMPD1 <- COMPD1[,-1]
      names(COMPD1) <- c("xdata","A","B","C","D","necafT")
      s = input$ExcludData4_rows_selected
      if (length(s)) {
        keep    <- COMPD1[-s, , drop = FALSE]
        exclude <- COMPD1[!s, , drop = FALSE]}
      else {
        s = rep(TRUE, nrow(COMPD1))
        keep    <- COMPD1[s, , drop = FALSE]
        exclude <- COMPD1[!s, , drop = FALSE]
      }
      rnum <- which(grepl(input$filter1, pmapval()$BatchID))
      concval <- pmapval()$Concentration4[rnum]
      write.csv(keep, paste(input$filter1,"_",concval,"keep4.csv", sep = ""));  ##write.csv(exclude, paste(input$filter1,"_",concval,"exclude3.csv", sep = ""))
      estimatedY50 = (max(as.numeric(keep$D))-min(as.numeric(keep$D)))/2
      estimatedDistY50 = abs(as.numeric(keep$D)-estimatedY50)
      locationNearest = which.min(estimatedDistY50)
      estimatedX50 = keep$xdata[locationNearest]
      fit4 <- nls(D ~ BOTTOM + (TOP-BOTTOM)/(1+10**((logEC50-xdata)*hillSlope)), data= keep,
                  start=list(TOP = max(as.numeric(keep$D)), BOTTOM = min(as.numeric(keep$D)), hillSlope = 1, logEC50 = estimatedX50))
      SmoothX <- seq(min(keep$xdata), max(keep$xdata), length = 100)
      Title =paste(input$filter1,"_",concval, sep = "")
      par(mar=c(5, 4, 4, 2) + 0.1)
      jpeg(paste(input$filter1,"_",concval,".jpg", sep = ""))
      plot(D ~ xdata , data = keep, col = 4, pch =21, lwd=2, main = Title,
           xlab="Log[NECA]/[M]", ylab="%Max[cAMP accumulation/cells")
      lines(SmoothX, predict(fit4,list(xdata=SmoothX)), col='red', lwd=2)
      dev.off()
      plot(D ~ xdata , data = keep, col = 4, pch =21, lwd=2, main = Title,
           xlab="Log[NECA]/[M]", ylab="%Max[cAMP accumulation/cells")
      lines(SmoothX, predict(fit4,list(xdata=SmoothX)), col='red', lwd=2)
    })


#### HTRF Plot 5  ############  HTRF Plot 5  ########################################
    output$ExcludData5 <- DT::renderDataTable({
      COMPD1 <- as.data.frame(read.csv(paste(input$filter1,".csv", sep = "")))
      COMPD1 <- COMPD1[,-1]; COMPD1 <- COMPD1[c(1,6)]
      names(COMPD1)[1] <- "Log[NECA]/(M)"
      rnum <- which(grepl(input$filter1, pmapval()$BatchID))
      names(COMPD1)[2] <- pmapval()$Concentration5[rnum]
      DT::datatable(COMPD1,options = list(pageLength = 22, dom = 'tip'))
    })
    output$fitData5 <- renderPrint({
      COMPD1 <- as.data.frame(read.csv(paste(input$filter1,".csv", sep = "")))
      COMPD1 <- COMPD1[,-1]
      names(COMPD1) <- c("xdata","A","B","C","D","necafT")
      s = input$ExcludData5_rows_selected
      if (length(s)) {
        keep    <- COMPD1[-s, , drop = FALSE]
        exclude <- COMPD1[!s, , drop = FALSE]}
      else {s = rep(TRUE, nrow(COMPD1))
        keep    <- COMPD1[s, , drop = FALSE]
        exclude <- COMPD1[!s, , drop = FALSE]}
      estimatedY50 = (max(as.numeric(keep$necafT))-min(as.numeric(keep$necafT)))/2
      estimatedDistY50 = abs(as.numeric(keep$necafT)-estimatedY50)
      locationNearest = which.min(estimatedDistY50)
      estimatedX50 = keep$xdata[locationNearest]
      maxx <- as.data.frame(read.csv(paste("Top5.csv", sep = "")))
      minx <- as.data.frame(read.csv(paste("Bot5.csv", sep = "")))
      maxx <- maxx[,-1] ; minx <- minx[,-1]
      mxnum <- which(grepl(input$filter1, maxx$BatchID));
      maxx$Conc5[mxnum] = max(as.numeric(keep$necafT)); minx$Conc5[mxnum] = min(as.numeric(keep$necafT))
      write.csv(maxx,paste("Top5.csv", sep = ""))
      write.csv(minx,paste("Bot5.csv", sep = ""))
      fit5 <- nls(necafT ~ BOTTOM + (TOP-BOTTOM)/(1+10**((logEC50-xdata)*hillSlope)), data= keep,
                  start=list(TOP = max(as.numeric(keep$necafT)), BOTTOM = min(as.numeric(keep$necafT)), hillSlope = 1, logEC50 = estimatedX50))
      rnum <- which(grepl(input$filter1, pmapval()$BatchID))
      concval <- pmapval()$Concentration5[rnum]
      sink(paste(input$filter1,"_",concval,".txt", sep = ""));  print(fit5);  sink()
      pm1 <- read.table(paste(input$filter1,"_",concval,".txt", sep = ""), skip = 3, nrows = 1, header = T)
      rnum <- which(grepl(input$filter1, pmapval()$BatchID))
      Concn <- pmapval()$Concentration5[rnum]
      BatchID <- input$filter1
      pm1 <- cbind(BatchID, Concn,pm1)
      pm1 <- as.data.frame(pm1)
      write.csv(pm1, paste(input$filter1,"_",concval,".csv", sep = ""))
      paramval <- read.csv(paste("Params.csv", sep = ""));    paramval <- paramval[,-1]
      for (i in 1:20)
        if(paramval$BatchID[i] == gsub(" ", "", input$filter1) & paramval$Concn[i] == pmapval()$Concentration5[rnum] )
        {paramval$Top[i] <- pm1$TOP[1]
         paramval$Bottom[i] <- pm1$BOTTOM[1]
         paramval$HillSlope[i] <- pm1$hillSlope[1]
         paramval$logEC50[i] <- pm1$logEC50[1]}
      write.csv(paramval,paste("Params.csv", sep = ""))
      fit5
    })
    output$HTRFPlot5 <- renderPlot({
      COMPD1 <- as.data.frame(read.csv(paste(input$filter1,".csv", sep = "")))
      COMPD1 <- COMPD1[,-1]
      names(COMPD1) <- c("xdata","A","B","C","D","necafT")
      s = input$ExcludData5_rows_selected
      if (length(s)) {
        keep    <- COMPD1[-s, , drop = FALSE]
        exclude <- COMPD1[!s, , drop = FALSE]}
      else {
        s = rep(TRUE, nrow(COMPD1))
        keep    <- COMPD1[s, , drop = FALSE]
        exclude <- COMPD1[!s, , drop = FALSE]
      }
      rnum <- which(grepl(input$filter1, pmapval()$BatchID))
      concval <- pmapval()$Concentration5[rnum]
      write.csv(keep, paste(input$filter1,"_",concval,"keep5.csv", sep = ""));   ##write.csv(exclude, paste(input$filter1,"_",concval,"exclude3.csv", sep = ""))
      estimatedY50 = (max(as.numeric(keep$necafT))-min(as.numeric(keep$necafT)))/2
      estimatedDistY50 = abs(as.numeric(keep$necafT)-estimatedY50)
      locationNearest = which.min(estimatedDistY50)
      estimatedX50 = keep$xdata[locationNearest]
      fit5 <- nls(necafT ~ BOTTOM + (TOP-BOTTOM)/(1+10**((logEC50-xdata)*hillSlope)), data= keep,
                  start=list(TOP = max(as.numeric(keep$necafT)), BOTTOM = min(as.numeric(keep$necafT)), hillSlope = 1, logEC50 = estimatedX50))
      SmoothX <- seq(min(keep$xdata), max(keep$xdata), length = 100)
      Title =paste(input$filter1,"_",concval, sep = "")
      par(mar=c(5, 4, 4, 2) + 0.1)
      jpeg(paste(input$filter1,"_",concval,".jpg", sep = ""))
      plot(necafT ~ xdata , data = keep, col = 4, pch =21, lwd=2, main = Title,
           xlab="Log[NECA]/[M]", ylab="%Max[cAMP accumulation/cells")
      lines(SmoothX, predict(fit5,list(xdata=SmoothX)), col='red', lwd=2)
      dev.off()
      plot(necafT ~ xdata , data = keep, col = 4, pch =21, lwd=2, main = Title,
           xlab="Log[NECA]/[M]", ylab="%Max[cAMP accumulation/cells")
      lines(SmoothX, predict(fit5,list(xdata=SmoothX)), col='red', lwd=2)

    })

#### Linear Fit1  ############  Linear Fit ########################################
    output$Parameters <- DT::renderDataTable({
      Parameter <- as.data.frame(read.csv(paste("Params.csv", sep = ""), header = T))
      Parameter <- Parameter[-1]
      Parameter <- subset(Parameter,Parameter$BatchID==gsub(" ", "",input$filter1))
      write.csv(Parameter, paste(gsub(" ", "",input$filter1), "_Params.csv", sep = ""))
      DT::datatable(Parameter, rownames = F, options = list(dom = 't',columnDefs = list(list(className = 'dt-center'))))
    })
    output$LinFitData <- DT::renderDataTable({
      xx <- read.csv(paste(input$filter1, "_LinearFit.csv", sep = ""))
      DT::datatable(xx,options = list(dom = 't',columnDefs = list(list(className = 'dt-center', targets = 0:3))))
    })
    output$LinFitPlot <- renderPlot({
      pms1 = read.csv(paste(input$filter1, "_Params.csv", sep = ""))
      pms1 <- pms1[order(pms1$Concn, decreasing = F),]; pms1 <- pms1[,-1]
      pms1$LogY <- log10(as.numeric(pms1$Concn));
      pms1$EC50 <- 10^(as.numeric(pms1$logEC50))
      for (i in 1:5){pms1$LogX[i] <- log10((as.numeric(pms1$EC50[i]) / as.numeric(pms1$EC50[1]))-1)}
      lf1 = as.data.frame(cbind(pms1$LogX[-1], pms1$LogY[-1]))
      colnames(lf1) <- c("X", "Y")
      mod <- lm(formula = lf1$Y ~ lf1$X)
      jpeg(paste(input$filter1,"_LinearFit.jpg", sep = ""))
      plot(Y ~ X , data = lf1, col = 4, pch =21, lwd=2, main = input$filter1,
           ylab="Log Concn", xlab="LogEC50")
      lines(lf1$X,mod$fitted.values, col='red', lwd=2)
      dev.off()
      xx <- as.data.frame(mod$coefficients)
      rownames(xx)[1] <- "X_Intercept"
      rownames(xx)[2] <- "Slope"
      xx <- as.data.frame(t(xx))
      xx$Y_Intercept <- format(as.numeric(-(xx$Slope * xx$X_Intercept)),digits=6, nsmall=2)
      xx$Calc_kb <- format(as.numeric(10^xx$X_Intercept),digits=6, nsmall=2)
      rownames(xx)[1] <- "Linear_Fit_Values"
      xx$X_Intercept <- format(as.numeric(xx$X_Intercept),digits=6, nsmall=2)
      xx$Slope <- format(as.numeric(xx$Slope),digits=4, nsmall=2)
      xx$SlopeInverse <- format(1/as.numeric(xx$Slope),digits=4, nsmall=2)
      write.csv(xx,paste(gsub(" ", "",input$filter1), "_LinearFit.csv", sep = ""))
      plot(Y ~ X , data = lf1, col = 4, pch =21, lwd=2, main = input$filter1,
           ylab="Log Concn", xlab="LogEC50")
      lines(lf1$X,mod$fitted.values, col='red', lwd=2)
    })


#### Global Fit  ############  Global Fit  ########################################

    output$GlobalParameters <- DT::renderDataTable({
      Parameter <- as.data.frame(read.csv(paste("Params.csv", sep = ""), header = T))
      Globals <- as.data.frame(read.csv(paste("Globals.csv", sep = ""), header = T))
      Parameter <- Parameter[-1]
      Parameter <- subset(Parameter,Parameter$Concn == 0)
      for (i in 1:4) {
        if (Parameter$BatchID[i] == Globals$BatchID[i]) {
          Globals$EC50[i] = format(10^as.numeric(Parameter$logEC50[i]), digits = 4, nsmall = 4)
          Globals$LogEC50[i]= format(as.numeric(Parameter$logEC50[i]), digits = 4, nsmall= 4)
        }}

      maxx <- as.data.frame(read.csv(paste("Top5.csv", sep = "")))
      minx <- as.data.frame(read.csv(paste("Bot5.csv", sep = "")))
      maxx <- maxx[,-1] ; minx <- minx[,-1]
      for (i in 1:4) {
        if (Globals$BatchID[i] == maxx$BatchID[i] & Globals$BatchID[i] == minx$BatchID[i])
        { Globals$Top[i] = mean(as.numeric(maxx[i,2:6]))
        Globals$Bottom[i] = mean(as.numeric(minx[i,2:6]))
        }}

      Globals[, c('BatchID','Top','Bottom','pa2','EC50', 'LogEC50', 'conc1','conc2','conc3','conc4','conc5')]
      write.table(Globals,paste("Globals.csv", sep = ""), quote = F, col.names = T, sep = ",")
      Globals <- subset(Globals,Globals$BatchID==gsub(" ", "",input$filter1))
      DT::datatable(Globals[, c('BatchID','Top','Bottom','pa2','EC50','LogEC50','conc1','conc2','conc3','conc4','conc5')], rownames = F, options = list(dom = 't',columnDefs = list(list(className = 'dt-center'))))
    })

    output$GlobalFitPlot <- renderPlot({

      Globals <- as.data.frame(read.csv(paste("Globals.csv", sep = ""), header = T))
      Globals <- Globals[c('BatchID','Top','Bottom','pa2','EC50','LogEC50','conc1','conc2','conc3','conc4','conc5')]
      Globals <- subset(Globals,Globals$BatchID == input$filter1)

      COMPD1 <- data.frame(read.csv(paste(input$filter1,"_",Globals$conc1[1],"keep1.csv", sep = "")))
      COMPD2 <- data.frame(read.csv(paste(input$filter1,"_",Globals$conc2[1],"keep2.csv", sep = "")))
      COMPD3 <- data.frame(read.csv(paste(input$filter1,"_",Globals$conc3[1],"keep3.csv", sep = "")))
      COMPD4 <- data.frame(read.csv(paste(input$filter1,"_",Globals$conc4[1],"keep4.csv", sep = "")))
      COMPD5 <- data.frame(read.csv(paste(input$filter1,"_",Globals$conc5[1],"keep5.csv", sep = "")))

      COMPD1 <- COMPD1[,-1]; COMPD2 <- COMPD2[,-1];COMPD3 <- COMPD3[,-1]; COMPD4 <- COMPD4[,-1]; COMPD5 <- COMPD5[,-1]

      ec50_1 = as.numeric(Globals$EC50[1]) ;    conc1 = as.numeric(Globals$conc1[1])
      antag1 = 1+(conc1/(10**(-1*Globals$pa2[1])))**1 ;  logEC1 = log10(ec50_1*antag1)
      TOP1 = as.numeric(Globals$Top[1]);  BOTTOM1 = as.numeric(Globals$Bottom[1]);  hillSlope = 1

      fit1 <- nls(A ~ BOTTOM + (TOP-BOTTOM)/(1+10**((logEC-xdata)*hillSlope)), data= COMPD1,
                  start=list(TOP = TOP1, BOTTOM = BOTTOM1, hillSlope = 1, logEC = logEC1))
      SmoothXA <- seq(min(COMPD1$xdata), max(COMPD1$xdata), length = 100)

      fit2 <- nls(B ~ BOTTOM + (TOP-BOTTOM)/(1+10**((logEC-xdata)*hillSlope)), data= COMPD2,
                  start=list(TOP = TOP1, BOTTOM = BOTTOM1, hillSlope = 1, logEC = logEC1))
      SmoothXB <- seq(min(COMPD2$xdata), max(COMPD2$xdata), length = 100)

      fit3 <- nls(C ~ BOTTOM + (TOP-BOTTOM)/(1+10**((logEC-xdata)*hillSlope)), data= COMPD3,
                  start=list(TOP = TOP1, BOTTOM = BOTTOM1, hillSlope = 1, logEC = logEC1))
      SmoothXC <- seq(min(COMPD3$xdata), max(COMPD3$xdata), length = 100)

      fit4 <- nls(D ~ BOTTOM + (TOP-BOTTOM)/(1+10**((logEC-xdata)*hillSlope)), data= COMPD4,
                  start=list(TOP = TOP1, BOTTOM = BOTTOM1, hillSlope = 1, logEC = logEC1))
      SmoothXD <- seq(min(COMPD4$xdata), max(COMPD4$xdata), length = 100)

      fit5 <- nls(necafT ~ BOTTOM + (TOP-BOTTOM)/(1+10**((logEC-xdata)*hillSlope)), data= COMPD5,
                  start=list(TOP = TOP1, BOTTOM = BOTTOM1, hillSlope = 1, logEC = logEC1))
      SmoothXE <- seq(min(COMPD5$xdata), max(COMPD5$xdata), length = 100)

      globalFit <- cbind(coef(fit1),coef(fit2),coef(fit3),coef(fit4),coef(fit5))
      write.csv(globalFit,paste("gf.csv", sep = ""))
      par(mar=c(5, 4, 4, 2) + 0.1)
      Title = paste("Stimulation of hA2aR by", input$filter1, "in hA2aT cells (Global Fit)", sep = " ")
      jpeg(paste(input$filter1,"_GlobalFit.jpg", sep = ""))
          plot(A ~ xdata , data = COMPD1, col = 4,  ylim=c(0,150), xlim=c(-12,-4),
            pch =21, lwd=2, main = Title, xlab="Log[NECA]/[M]", ylab="%Max[cAMP accumulation/cells")
          lines(SmoothXA, predict(fit1,list(xdata=SmoothXA)), col='black', lwd=2)
          points(B ~ xdata , data = COMPD2, col = 2, pch =22, lwd=2)
          lines(SmoothXB, predict(fit2,list(xdata=SmoothXB)), col='black', lwd=2)
          points(C ~ xdata , data = COMPD3, col = 81, pch =23, lwd=2)
          lines(SmoothXC, predict(fit3,list(xdata=SmoothXC)), col='black', lwd=2)
          points(D ~ xdata , data = COMPD4, col = 94, pch =24, lwd=2)
          lines(SmoothXD, predict(fit4,list(xdata=SmoothXD)), col='black', lwd=2)
          points(necafT ~ xdata , data = COMPD5, col = 84, pch =25, lwd=2)
          lines(SmoothXE, predict(fit5,list(xdata=SmoothXE)), col='black', lwd=2)
          legend(-12,150,title = input$filter1, Globals[1,6:10], col=c(4, 2, 81,94,84),pch=21:25, lwd = 2, lty =0)
      dev.off()
      plot(A ~ xdata , data =  COMPD2, col = 4,  ylim=c(0,150), xlim=c(-12,-4),
           pch =21, lwd=2, main = Title, xlab="Log[NECA]/[M]", ylab="%Max[cAMP accumulation/cells")
      lines(SmoothXA, predict(fit1,list(xdata=SmoothXA)), col='black', lwd=2)
      points(B ~ xdata , data = COMPD2, col = 2, pch =22, lwd=2)
      lines(SmoothXB, predict(fit2,list(xdata=SmoothXB)), col='black', lwd=2)
      points(C ~ xdata , data = COMPD3, col = 81, pch =23, lwd=2)
      lines(SmoothXC, predict(fit3,list(xdata=SmoothXC)), col='black', lwd=2)
      points(D ~ xdata , data = COMPD4, col = 94, pch =24, lwd=2)
      lines(SmoothXD, predict(fit4,list(xdata=SmoothXD)), col='black', lwd=2)
      points(necafT ~ xdata , data = COMPD5, col = 84, pch =25, lwd=2)
      lines(SmoothXE, predict(fit5,list(xdata=SmoothXE)), col='black', lwd=2)
      legend(-12,150,title = input$filter1, Globals[1,6:10], col=c(4, 2, 81,94,84),pch=21:25, lwd = 2, lty =0)
    })

    output$GlobalFitData <- DT::renderDataTable({
      Globals <- as.data.frame(read.csv(paste("Globals.csv", sep = ""), header = T))
      gf <- data.frame(read.csv(paste("gf.csv", sep = "")))
      rownames(gf) <-gf[,1]; gf <- gf[,-1]
      colnames(gf) <- c(Globals$conc1[1],Globals$conc2[1],Globals$conc3[1],Globals$conc4[1],"[0]")
      write.csv(gf, paste(input$filter1,"_GlobalFitData.csv", sep = ""))
      DT::datatable(gf,options = list(dom = 't',columnDefs = list(list(className = 'dt-center'))))%>% formatRound(1:5, 4)
    })

#### cAMP STANDARD CURVE  ############ cAMP STANDARD CURVE  #########################
  output$cAMPStdPlot <- renderPlot({
    file3 <- input$fileRD3
    if (is.null(file3)) {return()}
    cAMPStd <- as.data.frame(read.csv(paste("cAMPData.csv", sep = "")))
    cAMPStd <- cAMPStd[,-1]
    keep    <- cAMPStd[ vals$keeprows, , drop = FALSE]
    exclude <- cAMPStd[!vals$keeprows, , drop = FALSE]
    fo <- Values ~ (ymax* Concn / (ec50 + Concn)) + Ns * Concn + ymin
    st <- list(ymax=max(keep$Values), ymin = min(keep$Values), ec50 = 3, Ns = 0.2045514)
    nls.fit <- nls(fo, data = keep, start = st)
    plot(Values ~ Concn, keep, subset = Concn > 0, type = 'p', col = 4,pch = 16,cex = 2, axes = FALSE, frame.plot = TRUE,log = "x")
    title(main = "Interactive Std curve")
    logRange <- with(keep, log(range(Concn[Concn > 0])))
    x <- exp(seq(logRange[1], logRange[2], length = 250))
    lines(x, predict(nls.fit, new = list(Concn = x)),col = 2, cex = 2)
    points(Values ~ Concn, exclude, subset = Concn > 0, col = 1, cex = 2)
    my.at <- 10^(-2:3)
    axis(1, at = my.at, labels = formatC(my.at, format = "fg"))
    axis(2)

    jpeg(paste("cAMP_StdCurve.jpg", sep = ""),width = 650, height = 400, units = "px", pointsize = 12,
         quality = 75, bg = "white", res = NA)
    plot(Values ~ Concn, keep, subset = Concn > 0, type = 'p',pch = 16,cex = 2, axes = FALSE, frame.plot = TRUE,log = "x")
    title(main = "Interactive Std curve")
    logRange <- with(keep, log(range(Concn[Concn > 0])))
    x <- exp(seq(logRange[1], logRange[2], length = 250))
    lines(x, predict(nls.fit, new = list(Concn = x)),col = 1)
    points(Values ~ Concn, exclude, subset = Concn > 0, col = 1, cex = 2)
    my.at <- 10^(-2:3)
    axis(1, at = my.at, labels = formatC(my.at, format = "fg"))
    axis(2)
    dev.off()
  })

  output$fitData <- DT::renderDataTable({
    file3 <- input$fileRD3
    if (is.null(file3)) {return()}
      cAMPStd <- as.data.frame(read.csv(paste("cAMPData.csv", sep = "")))
      cAMPStd <- cAMPStd[,-1]
      cAMPStdData  <- cAMPStd[vals$keeprows, , drop = FALSE]
      exclude <- cAMPStd[!vals$keeprows, , drop = FALSE]
      nls.fit <- nls(Values ~ (ymax* cAMPStdData$Concn/(ec50 + cAMPStdData$Concn))+Ns*cAMPStdData$Concn + ymin, data=cAMPStdData,
                     start=list(ymax=max(cAMPStdData$Values), ymin = min(cAMPStdData$Values), ec50 = 3, Ns = 0.2045514))
      sink(paste("cAMPFit.txt", sep = ""));  print(nls.fit);    sink()
      pm <- read.delim(paste("cAMPFit.txt", sep = ""), header = T, stringsAsFactors = T, skip =3, sep = " ")
      data =   read.table(paste("cAMPFit.txt", sep = ""),skip = 3, nrows = 1, header = T)
      write.csv(data, paste("cAMP_Fit.csv", sep = ""))
      DT::datatable(data, rownames = F, options = list(dom = 't',columnDefs = list(list(className = 'dt-center'))))
  })


  observeEvent(input$plot1_click, {
    res <- nearPoints(cAMPStd, input$plot1_click, xvar="Concn", yvar="Values", allRows = TRUE)
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })

  observeEvent(input$exclude_reset, {
    vals$keeprows <- rep(TRUE, nrow(cAMPStd))
  })

  #### cAMP STANDARD CURVE  ############ cAMP STANDARD CURVE  #########################
  output$pdfview <- renderUI({
    tags$iframe(style="height:600px; width:100%", src="HTRFAssayDataAnalysisToolManual.pdf")
  })

}

shinyApp(ui, server)
