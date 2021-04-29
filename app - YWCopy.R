library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyBS)
library(shinyalert)
library(shinydashboard)
library(shinycssloaders)
library(rhandsontable)
library(DT)
library(data.table)
library(dplyr)
library(tidyr)
library(readr)
library(drc)
library(stringr)
library(plotly)
library(d3heatmap)
library(janitor)
library(readxl)
library(WriteXLS)
library(shinyWidgets)
library(pzfx)
library(Polychrome)
options(scipen = 999)
Glasbey = glasbey.colors(16) 
xd <- as.data.frame(read_csv("pdyw.csv"))
str_right <- function(string, n) {substr(string, nchar(string) - (n - 1), nchar(string))}
PlateDetails <- read_csv("PlateDetails.csv")

RowNames <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P")
pholder <- as.data.frame(matrix(data = "", nrow = 16, ncol = 24, byrow = FALSE, dimnames = NULL))
colnames(pholder) <- paste0("Col-",seq(1:24))

unfactorize <- function(df){for(i in which(sapply(df, class) == "factor")) 
                                {df[[i]] = as.character(df[[i]])}
                            return(df)}

PlateDetails <- unfactorize(PlateDetails)

MMark <- function(label) { tagList( label, span("*", style = "color:red; font-size: 12px;"))}

server <-  function(input, output, session) {  
  
hide("cmpnum"); disable("DownloadData");  disable("DownloadData2"); 
disable("DownloadData3");  disable("DownloadData4"); disable("Process");  disable("Update")

values <- reactiveValues(hbData1 = pholder, vbData1 = pholder,hbData2 = pholder, vbData2 = pholder, cmpnumber = 1,
                         hbData = pholder, vbData = pholder,  PlateDetails = PlateDetails, prismdata = NULL,
                         hmpa1 = NULL, hmpa2 = NULL, hibit1 = NULL, hibit2 = NULL, hb1 = NULL, vb1 = NULL,
                         hmpb1 = NULL, hmpb2 = NULL, viab1 = NULL, viab2 = NULL, hb2 = NULL, vb2 = NULL)

observeEvent(input$loadFiles, { 
  if(is.null(input$hibitDataFile) | is.null(input$viabDataFile) ) 
{shinyalert("Oops!", "Please select a 384 well plate Data file to process.", type = "error")
return(NULL)}
    inFile <- input$hibitDataFile
    inFile2 <- input$viabDataFile
    for (fz in 1:2) {
          xd$Compound <- gsub("Compound01",values$PlateDetails$Compound_Name[1],xd$Compound)
          xd$Compound <- gsub("Compound02",values$PlateDetails$Compound_Name[2],xd$Compound)
          xd$Compound <- gsub("Compound03",values$PlateDetails$Compound_Name[3],xd$Compound)
          xd$Compound <- gsub("Compound04",values$PlateDetails$Compound_Name[4],xd$Compound)
          xd$Compound <- gsub("Compound05",values$PlateDetails$Compound_Name[5],xd$Compound)
          xd$Compound <- gsub("Compound06",values$PlateDetails$Compound_Name[6],xd$Compound)
          xd$Compound <- gsub("Compound07",values$PlateDetails$Compound_Name[7],xd$Compound)
          xd$Compound <- gsub("Compound08",values$PlateDetails$Compound_Name[8],xd$Compound)
          xd$Compound <- gsub("Compound09",values$PlateDetails$Compound_Name[9],xd$Compound)
          xd$Compound <- gsub("Compound10",values$PlateDetails$Compound_Name[10],xd$Compound)
          xd$Compound <- gsub("Compound11",values$PlateDetails$Compound_Name[11],xd$Compound)
          xd$Compound <- gsub("Compound12",values$PlateDetails$Compound_Name[12],xd$Compound)
          xd$Compound <- gsub("Compound13",values$PlateDetails$Compound_Name[13],xd$Compound)
          xd$Compound <- gsub("Compound14",values$PlateDetails$Compound_Name[14],xd$Compound)
          xd$Compound <- gsub("Compound15",values$PlateDetails$Compound_Name[15],xd$Compound)
          xd$Compound <- gsub("Compound16",values$PlateDetails$Compound_Name[16],xd$Compound)         
        pd <- xd;  pdv <- xd
       
        hbd <- as.data.frame(read_csv(inFile$datapath[fz], col_names = FALSE, skip = 10))
        vbd <- as.data.frame(read_csv(inFile2$datapath[fz], col_names = FALSE, skip = 10))
        hbd <- hbd[,1:25]
        vbd <- vbd[,1:25]
        colnames(hbd) <- paste0("Col",seq(1:25))
        colnames(vbd) <- paste0("Col",seq(1:25))
        print("Test 0")
        vc <- c("A","B","C","D", "E", "F", "G", "H", "I", "J", "K","L", "M", "N", "O", "P")
        hbd <- hbd[hbd$Col1 %in% vc,]
        vbd <- vbd[vbd$Col1 %in% vc,]
        hbd$Col1 <- NULL
        vbd$Col1 <- NULL
        colnames(hbd) <- paste0("Col",seq(1:24))
        colnames(vbd) <- paste0("Col",seq(1:24))
        hbd <- as.data.frame(hbd)
        hbd <- hbd[hbd$Col1 >=0, ]
        hbd <- hbd %>% select(paste0("Col",seq(1:24)))
        vbd <- as.data.frame(vbd)
        vbd <- vbd[vbd$Col1 >=0, ]
        vbd <- vbd %>% select(paste0("Col",seq(1:24)))
    
        if (fz == 1) {values$hbData1 <- hbd; values$vbData1 <- vbd }
        if (fz == 2) {values$hbData2 <- hbd; values$vbData2 <- vbd }
    }
        enable("Update")
        shinyalert("Step-1", "Update the Compound Names in 'Plate Details' Section and click on [Update Compound Names] button ", type = "info")
  })

observeEvent(input$Process, {  
if(is.null(input$hibitDataFile) | is.null(input$viabDataFile) ) 
{shinyalert("Oops!", "Please select a 384 well plate Data file to process.", type = "error")
return(NULL)}

    for (fz in 1:2)
    {
          xd$Compound <- gsub("Compound01",values$PlateDetails$Compound_Name[1],xd$Compound)
          xd$Compound <- gsub("Compound02",values$PlateDetails$Compound_Name[2],xd$Compound)
          xd$Compound <- gsub("Compound03",values$PlateDetails$Compound_Name[3],xd$Compound)
          xd$Compound <- gsub("Compound04",values$PlateDetails$Compound_Name[4],xd$Compound)
          xd$Compound <- gsub("Compound05",values$PlateDetails$Compound_Name[5],xd$Compound)
          xd$Compound <- gsub("Compound06",values$PlateDetails$Compound_Name[6],xd$Compound)
          xd$Compound <- gsub("Compound07",values$PlateDetails$Compound_Name[7],xd$Compound)
          xd$Compound <- gsub("Compound08",values$PlateDetails$Compound_Name[8],xd$Compound)
          xd$Compound <- gsub("Compound09",values$PlateDetails$Compound_Name[9],xd$Compound)
          xd$Compound <- gsub("Compound10",values$PlateDetails$Compound_Name[10],xd$Compound)
          xd$Compound <- gsub("Compound11",values$PlateDetails$Compound_Name[11],xd$Compound)
          xd$Compound <- gsub("Compound12",values$PlateDetails$Compound_Name[12],xd$Compound)
          xd$Compound <- gsub("Compound13",values$PlateDetails$Compound_Name[13],xd$Compound)
          xd$Compound <- gsub("Compound14",values$PlateDetails$Compound_Name[14],xd$Compound)
          xd$Compound <- gsub("Compound15",values$PlateDetails$Compound_Name[15],xd$Compound)
          xd$Compound <- gsub("Compound16",values$PlateDetails$Compound_Name[16],xd$Compound)         
        pd <- xd;  pdv <- xd
        
        print("Test 02a")
        # if (input$hibitData) {print(hot_to_r(input$hibitData))}
        # #hot_to_r(input$viabData)
        # 

             
        if (fz == 1) { 
            values$hbData1 <- hot_to_r(input$hibitData1) 
            hbd <- values$hbData1;  
            values$vbData1 <- hot_to_r(input$viabData1)
            vbd <- values$vbData1}
          
        if (fz == 2) { 
            values$hbData2 <- hot_to_r(input$hibitData2) 
            hbd <- values$hbData2;  
            values$vbData2 <- hot_to_r(input$viabData2)
            vbd <- values$vbData2}
          
        
        hbd3 <- hbd ; vbd3 <- vbd
        
        print("Test 02b")

        cnt = 1
        for (i in 1:16) { for (j in 1:24) 
          {pd$Measurement[cnt] <- hbd[i,j] 
           pdv$Measurement[cnt] <- vbd[i,j] 
           cnt = cnt +1
          }
        }
        
        print("Test 1")
        
        NC <- pd  %>% filter(Compound == "NC" & Measurement > 0)
        NCV <- pdv  %>% filter(Compound == "NC"& Measurement > 0)
        avgNCV  <- mean(as.numeric(NCV$Measurement))
      
        AC <- pd  %>% filter(Compound == "AC" & Measurement > 0)
       
        avgNC <- mean(as.numeric(NC$Measurement))
        avgAC <- mean(as.numeric(AC$Measurement))
        sdNC <- sd(as.numeric(NC$Measurement))
        sdAC <- sd(as.numeric(AC$Measurement))
        S2N <- avgNC/avgAC
        numa <- (3 * (sdNC+sdAC))
        denom <- (avgNC-avgAC)
        ZPrime <- 1-(numa/denom)
        
        print(paste("avgNc", avgNC))
        print(paste("avgAc", avgAC))
        
        print("Test 2")

        pda <- pd
        pdb <- pdv
        cnt = 0
        
        for (i in 1:16) {for (j in 1:24) {cnt = cnt +1
          pda$Measurement[cnt] <- round(100 *((as.numeric(hbd[i,j])-avgAC)/(avgNC-avgAC)),3)
          hbd3[i,j] <- round(100 *((as.numeric(hbd[i,j])-avgAC)/(avgNC-avgAC)),3)
          pdb$Measurement[cnt] <- round(100 *((as.numeric(vbd[i,j]))/avgNCV),3)
          vbd3[i,j] <- round(100 *((as.numeric(vbd[i,j]))/avgNCV),3)
          }
        }
        
         if (fz == 1) {
              values$hmpa1 <- as.matrix(sapply(hbd3, as.numeric))
              values$hmpb1 <- as.matrix(sapply(vbd3, as.numeric)) 
         }
        
         if (fz == 2) {
              values$hmpa2 <- as.matrix(sapply(hbd3, as.numeric))
              values$hmpb2 <- as.matrix(sapply(vbd3, as.numeric)) 
         }
        
        colnames(pda)
        pda <- data.frame(pda)
        pdb <- data.frame(pdb)
        pda <- pda[1:384,]
        pdb <- pdb[1:384,]
        pd0 <- pda %>% dplyr::select(Compound, Condition,  Replicate, Concentration, Measurement) %>% filter(!is.na(Condition)) %>% as.data.frame()
        print(nrow(pda))
        pd0$Compound <-  paste0(pd0$Compound,"+",pd0$Condition)
        pd0$Compound<- gsub("\\+Nil","", pd0$Compound)
        pd0$Condition <- NULL
        pdv0 <- pdb %>% dplyr::select(Compound, Condition,  Replicate, Concentration, Measurement) %>% filter(!is.na(Condition)) %>% as.data.frame()
        pdv0$Compound <-  paste0(pdv0$Compound,"+",pdv0$Condition)
        pdv0$Compound<- gsub("\\+Nil","", pdv0$Compound)
        pdv0$Condition <- NULL


         if (fz == 1) {values$hb1 <- pd0; values$vb1 <- pdv0}
         if (fz == 2) {values$hb2 <- pd0; values$vb2 <- pdv0
         values$hb2$Replicate <- 2; values$vb2$Replicate <- 2;}

        print("Test 3")
    ###  HiBiT pivot    
        if (fz == 1) {
          pdx1 <- pda %>% dplyr::select(Compound, Condition,  Concentration, Measurement) %>% filter (!Compound %in% c('NC', 'NC2','AC')) 
          pdx1$Repeat <- "1"
          }
        if (fz == 2) { 
          pdx2 <- pda %>% dplyr::select(Compound, Condition,  Concentration, Measurement) %>% filter (!Compound %in% c('NC', 'NC2','AC') ); 
          pdx2$Repeat <- "2";}
       
        
        pd0 <- pda %>% dplyr::select(Compound, Condition,  Concentration, Measurement) %>% 
        pivot_wider(names_from = Compound,  values_from = Measurement, values_fn = list) %>%  filter(!is.na(Condition)) %>% as.data.frame()
        
        print("colnames(pd0)")
        #print(colnames(pd0))
        
        pd3 <- pd0 %>% filter(Condition == "Nil") 
        pd4 <- pd0 %>% filter(Condition != "Nil") 
        
        pd3$Condition <- NULL;    pd3$exptype <- NULL;    pd3$AC <- NULL;    pd3$NC <- NULL
        pd4$Condition <- NULL;    pd4$exptype <- NULL;    pd4$AC <- NULL;    pd4$NC <- NULL
        
        colnames(pd4) <- paste0(colnames(pd4), "_MEK")
        print("colnames(pd4)")
       # print(colnames(pd4))
        pd6 <- cbind(pd3,pd4)
        pd6 <- pd6[ , order(names(pd6))]
        colnames(pd6)
        
        pd6$"Concentration_MEK" <- NULL; pd6$NC2  <- NULL; pd6$"NC2_MEK" <- NULL; pd6$Replicate <- NULL; pd6$"Replicate_MEK" <- NULL    
        
        columnNumber <- which(colnames(pd6)=="Concentration")
        pd6 <- pd6[,c(columnNumber,1:ncol(pd6))]
        print("Prism")
       # print(colnames(pd6))
        pd6 <- as.matrix(sapply(pd6, as.numeric))
        print("Prism End")
    
        pd6 <- as.data.frame(pd6)
        if (fz == 1) {values$hibit1 <- pd6}
        if (fz == 2) {values$hibit2 <- pd6}
        print(paste("Test 4 -", fz))
         print(head(pd6))
        print("Test 4")
    ###  Viability pivot
    
        if (fz == 1) {
          pdvx1 <- pdb %>% dplyr::select(Compound, Condition,  Concentration, Measurement) %>% filter (!Compound %in% c('NC', 'NC2', 'AC')) 
          pdvx1$Repeat <- "1"
          }
        if (fz == 2) { 
          pdvx2 <- pdb %>% dplyr::select(Compound, Condition,  Concentration, Measurement) %>% filter (!Compound %in% c('NC', 'NC2','AC') ); 
          pdvx2$Repeat <- "2";}
       
        
        pdv0 <- pdb %>% dplyr::select(Compound, Condition,  Concentration, Measurement) %>% 
        pivot_wider(names_from = Compound,  values_from = Measurement, values_fn = list) %>%  filter(!is.na(Condition)) %>% as.data.frame()
        
        pdv3 <- pdv0 %>% filter(Condition == "Nil") 
        pdv4 <- pdv0 %>% filter(Condition != "Nil") 
        
        pdv3$Condition <- NULL;    pdv3$exptype <- NULL;    pdv3$AC <- NULL;    pdv3$NC <- NULL
        pdv4$Condition <- NULL;    pdv4$exptype <- NULL;    pdv4$AC <- NULL;    pdv4$NC <- NULL
        
        colnames(pdv4) <- paste0(colnames(pdv4), "_MEK")
        pdv5 <- cbind(pdv3,pdv4)
        pdv6 <- pdv5[ , order(names(pdv5))]
        colnames(pdv6)
        
        pdv6$"Concentration_MEK" <- NULL; pdv6$NC2  <- NULL; pdv6$"NC2_MEK" <- NULL; 
        pdv6$Replicate <- NULL; pdv6$"Replicate_MEK" <- NULL    
    
        columnNumber <- which(colnames(pdv6)=="Concentration")
        pdv6 <- pdv6[,c(columnNumber,1:ncol(pdv6))]
        print("Prism")
        pdv6 <- as.matrix(sapply(pdv6, as.numeric))
         print("Prism End")
    
        pdv6 <- as.data.frame(pdv6)
        if (fz == 1) {values$viab1 <- pdv6}
        if (fz == 2) {values$viab2 <- pdv6}
    }
       print("Test 5")
       
        # print(head(values$hibit1))
        # print(head(values$hibit2))
       pd6cc <-  cbind(values$hibit1,values$hibit2) 
       pd6cc <- pd6cc[ , order(names(pd6cc))]
       columnNumber <- which(colnames(pd6cc)=="Concentration")
       pd6cc <- pd6cc[,c(columnNumber,1:ncol(pd6cc))]
       pd6cc$Concentration.1 <- NULL
       pd6cc$Concentration.3 <- NULL
       pd6cc$Concentration.2 <- NULL
       pd6cc$Concentration.1.1 <- NULL
       colnames(pd6cc)  <- sub("\\.1$", "",colnames(pd6cc))
       fwrite(pd6cc, "HiBiT_Data.csv")
       
       pd6vcc <- cbind(values$viab1,values$viab2) 
       pd6vcc <- pd6vcc[ , order(names(pd6vcc))]
       columnNumber <- which(colnames(pd6vcc)=="Concentration")
       pd6vcc <- pd6vcc[,c(columnNumber,1:ncol(pd6vcc))]
       pd6vcc$Concentration.1 <- NULL
       pd6vcc$Concentration.3 <- NULL
       pd6vcc$Concentration.2 <- NULL
       pd6vcc$Concentration.1.1 <- NULL
       colnames(pd6vcc)  <- sub("\\.1$", "",colnames(pd6vcc))
       fwrite(pd6vcc, "Viability_Data.csv")
       
       hb <- rbind(values$hb1,values$hb2)
       vb <- rbind(values$vb1,values$vb2)
        Summary <- as.data.frame(rbind(pdx1,pdx2))
        Summary$Run <- ""
        Summary$Valid  <- ""
        Summary$Label  <- ""
        Summary$'Detail Type' <- "" 
        Summary$Analyte  <- ""
        Summary$'Background Subtraction'  <- ""
        Summary$'Cell Line'  <- ""
        Summary$Clone  <- ""
        Summary$Terminus  <- ""
        Summary$'Treatment Time'  <- ""
        Summary$'M Detail Type'  <- ""
        Summary$'M Concentration'  <- ""
        Summary$'M Measurement'   <- ""
        Summary$Condition <- gsub("Nil", "",Summary$Condition)
        Summary$Condition <- gsub("Inhibitor", "MEK",Summary$Condition)
       fwrite(Summary, "Summary.csv")
       
        Summary2 <- as.data.frame(rbind(pdvx1,pdvx2))
        Summary2$Run <- ""
        Summary2$Valid  <- ""
        Summary2$Label  <- ""
        Summary$'Detail Type' <- "" 
        Summary2$Analyte  <- ""
        Summary2$'Background Subtraction'  <- ""
        Summary2$'Cell Line'  <- ""
        Summary2$Clone  <- ""
        Summary2$Terminus  <- ""
        Summary2$'Treatment Time'  <- ""
        Summary2$'M Detail Type'  <- ""
        Summary2$'M Concentration'  <- ""
        Summary2$'M Measurement'   <- "" 
        Summary2$Condition <- gsub("Nil", "",Summary2$Condition)
        Summary2$Condition <- gsub("Inhibitor", "MEK",Summary2$Condition)
       fwrite(Summary2, "Summary2.csv")
       disable("loadFiles")
       enable("DownloadData");  enable("DownloadData2");enable("DownloadData3");  enable("DownloadData4")
})

observeEvent(input$Update, {  

   values$PlateDetails <- hot_to_r(input$pDetail)
   #values$vbData <- hot_to_r(input$viabData)
    updateSelectInput(session, "cmpdname",  choices = c(sort(values$PlateDetails$Compound_Name)), 
                    selected =values$PlateDetails$Compound_Name[1] )
    enable("Process"); disable("loadFiles")
    shinyalert("Step-2", "Click on [Process Data] button to continue.", type = "info")
})
  
output$drcPlot <- renderPlot({
    if (!is.null(values$hibit1) & !is.null(values$hibit2))
      
    { 
        if (input$hvplates == "Plate Set-1" ) {pd6 <- values$hibit1}
        if (input$hvplates == "Plate Set-2" ) {pd6 <- values$hibit2}
     
      pd6$Concentration.1 <- NULL
    
      keep <- c("Concentration",input$cmpdname)
      pd6a <- pd6 %>% select (starts_with(keep))
      print("head(pd6a)")
      
      #write.csv(pd6a,"pd6a.csv")
     
      fit1 <- drm(pd6a[,2] ~ pd6a$Concentration,  fct = LL2.4())
      fit2 <- drm(pd6a[,3] ~ pd6a$Concentration,  fct = LL2.4())
                  coef1 <- t(data.table(fit1$coefficients))
      coef1 <- as.data.frame(t(data.table(fit1$coefficients)))
      coef2 <- as.data.frame(t(data.table(fit2$coefficients)))
      colnames(coef1) <-  c("Hill slope", "Y-Min", "Y-Max","logIC50")
      colnames(coef2) <-  c("Hill slope", "Y-Min", "Y-Max","logIC50")
      coef <- as.data.frame(rbind(coef1, coef2))
       coef$IC50 <- exp(coef$logIC50)
       print(coef)
       row.names(coef) <- c(input$cmpdname, paste0(input$cmpdname,"+MEK"))
       
      output$coefTableA <- renderDataTable({
          datatable(coef, rownames = T,options = list(
          initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#3399CC', 
                            'color': '#fff', 'fontSize' : '100%'});","}"), 
          dom = 't',columnDefs = list(list(className = 'dt-center', width = '120px', targets = "_all")))) %>%
          formatRound("Hill slope", 3)  %>% formatRound("Y-Min", 2) %>%   formatRound("logIC50", 3) %>% #formatRound("AUC", 2) %>%
          formatRound("Y-Max", 2) %>% formatRound("IC50", 3) %>% formatStyle(c(1:5), fontWeight = 'bold', fontSize = '100%')
      })
      
      plot(fit1, ylim = c( -2,130),bty="n",col="blue",pch=16,axes = F,  text.title = input$cmpdname,
           xlab = "Concentration [uM]", col.lab="blue",cex.lab=1.2,
           ylab = "% B-Raf remaining", 
           font.sub = 2,col.sub = "#be2bc5", cex.sub=1.2)
      title(main = input$cmpdname,font.main= 3, col.main= "blue",cex.main=2)
      axis(1);    axis(2);  par(new=TRUE)
      plot(fit2,pch=16, col="red", ylim = c( -2,130),axes = F, xlab = "", ylab = "")
      legend("bottomleft", legend=c(input$cmpdname, paste0(input$cmpdname,"+MEK")),
       col=c("blue", "red"), lty=1, cex=0.8)
    }
  })

output$drcPlot2 <- renderPlot({
    if (!is.null(values$viab1) & !is.null(values$viab2))
      
    { 
        if (input$hvplates == "Plate Set-1" ) {pdv6 <- values$viab1}
        if (input$hvplates == "Plate Set-2" ) {pdv6 <- values$viab2}
      
      pdv6$Concentration.1 <- NULL
   
      keep <- c("Concentration",input$cmpdname)
      pdv6a <- pdv6 %>% select (starts_with(keep))
      
      fit1 <- drm(pdv6a[,2] ~ pdv6a$Concentration,  fct = LL2.4())
      fit2 <- drm(pdv6a[,3] ~ pdv6a$Concentration,  fct = LL2.4())

      coef1 <- as.data.frame(t(data.table(fit1$coefficients)))
      coef2 <- as.data.frame(t(data.table(fit2$coefficients)))
      colnames(coef1) <-  c("Hill slope", "Y-Min", "Y-Max","logIC50")
       colnames(coef2) <-  c("Hill slope", "Y-Min", "Y-Max","logIC50")
       coef <- as.data.frame(rbind(coef1, coef2))
       coef$IC50 <- exp(coef$logIC50)
       print(coef)
       row.names(coef) <- c(input$cmpdname, paste0(input$cmpdname,"+MEK"))
       
    output$coefTableB <- renderDataTable({
          datatable(coef, rownames = T,options = list(
          initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#3399CC', 
                            'color': '#fff', 'fontSize' : '100%'});","}"), 
          dom = 't',columnDefs = list(list(className = 'dt-center', width = '120px', targets = "_all")))) %>%
          formatRound("Hill slope", 3)  %>% formatRound("Y-Min", 2) %>%   formatRound("logIC50", 3) %>% #formatRound("AUC", 2) %>%
          formatRound("Y-Max", 2) %>% formatRound("IC50", 3) %>% formatStyle(c(1:5), fontWeight = 'bold', fontSize = '100%')
    })
      
      plot(fit1, ylim = c( -2,130),bty="n",col="blue",pch=16,axes = F,            xlab = "Concentration [uM]", col.lab="blue",cex.lab=1.2,
           ylab = "% B-Raf remaining", 
           font.sub = 2,col.sub = "#be2bc5", cex.sub=1.2)
      title(main = input$cmpdname,font.main= 3, col.main= "blue",cex.main=2)
      axis(1);    axis(2);  par(new=TRUE)
      plot(fit2,pch=16, col="red", ylim =c( -2,130),axes = F, xlab = "", ylab = "")
            legend("bottomleft", legend=c(input$cmpdname, paste0(input$cmpdname,"+MEK")),
       col=c("blue", "red"), lty=1, cex=0.8)  
    }
  })

output$DRHMPlot <- renderD3heatmap({
    if (!is.null(values$hmpa1) & !is.null(values$hmpa2))
    { 
       if (input$hvplates == "Plate Set-1" ) {hmp <- values$hmpa1}
        if (input$hvplates == "Plate Set-2" ){hmp <- values$hmpa2}
     rownames(hmp) <- values$PlateDetails$Compound_Name
      d3heatmap(hmp[,1:22], dendrogram = "none",show_grid = T, colors = "RdYlGn")
    }
  })

output$DRHMPlot2 <- renderD3heatmap({
    if (!is.null(values$hmpb1) & !is.null(values$hmpb2))
    { 
       if (input$hvplates == "Plate Set-1" ) {hmp2 <- values$hmpb1}
        if (input$hvplates == "Plate Set-2" ){hmp2 <- values$hmpb2}
      rownames(hmp2) <- values$PlateDetails$Compound_Name
      d3heatmap(hmp2[,1:22], dendrogram = "none",show_grid = T, colors = "RdYlGn")
    }
  })
    
output$pDetail <- renderRHandsontable({pdTbl()})

pdTbl <- reactive({
  cv <- rhandsontable(values$PlateDetails, height = 410, readOnly = F)  %>% 
  hot_cols(columnSorting = FALSE, colWidths = 150, manualColumnResize =TRUE) %>%
  hot_col("Compound_Number", readOnly = TRUE, format = "0a") %>%
  hot_col("Compound_Name", readOnly = F) %>%
  return(cv)
})


output$hibitData1 <- renderRHandsontable({hbTbl1()})

hbTbl1 <- reactive({
  cv <- rhandsontable(values$hbData1,height = 420, readOnly = F, rowHeaders = RowNames)  %>% 
  hot_cols(columnSorting = FALSE, colWidths = 75, manualColumnResize =TRUE) %>% hot_heatmap(color_scale = c("#ED6D47", "#17F556"))
  return(cv)
})

output$viabData1 <- renderRHandsontable({vbTbl1()})

vbTbl1 <- reactive({
  if (input$hvplates == "Plate Set-1" ) {vbData <- values$vbData1}
  if (input$hvplates == "Plate Set-2" ) {vbData <- values$vbData2}
  cv <- rhandsontable(values$vbData1,height = 420, readOnly = F, rowHeaders = RowNames)  %>% 
  hot_cols(columnSorting = FALSE, colWidths = 75, manualColumnResize =TRUE) %>% hot_heatmap(color_scale = c("#ED6D47", "#17F556"))
  return(cv)
})

output$hibitData2 <- renderRHandsontable({hbTbl2()})

hbTbl2 <- reactive({
  cv <- rhandsontable(values$hbData2,height = 420, readOnly = F, rowHeaders = RowNames)  %>% 
  hot_cols(columnSorting = FALSE, colWidths = 75, manualColumnResize =TRUE) %>% hot_heatmap(color_scale = c("#ED6D47", "#17F556"))
  return(cv)
})

output$viabData2 <- renderRHandsontable({vbTbl2()})

vbTbl2 <- reactive({
  cv <- rhandsontable(values$vbData2,height = 420, readOnly = F, rowHeaders = RowNames)  %>% 
  hot_cols(columnSorting = FALSE, colWidths = 75, manualColumnResize =TRUE) %>% hot_heatmap(color_scale = c("#ED6D47", "#17F556"))
  return(cv)
})


observeEvent(input$nextC,{ values$cmpnumber <- input$cmpnum
    if (values$cmpnumber  >= 1  & values$cmpnumber <= 15)    { 
      values$cmpnumber  <- values$cmpnumber + 1
      updateNumericInput(session, "cmpnum", value = values$cmpnumber,min = 1, max = 16)
      updateSelectInput(session, "cmpdname",  choices = c(sort(values$PlateDetails$Compound_Name)), 
                    selected =values$PlateDetails$Compound_Name[values$cmpnumber] )
      }
  })
observeEvent(input$prevC,{ values$cmpnumber <- input$cmpnum
    if (values$cmpnumber > 1  & values$cmpnumber <= 16)    { 
      values$cmpnumber <- values$cmpnumber - 1
      updateNumericInput(session, "cmpnum", value = values$cmpnumber,min = 1, max = 16)
      updateSelectInput(session, "cmpdname",  choices = c(sort(values$PlateDetails$Compound_Name)), 
                    selected = values$PlateDetails$Compound_Name[values$cmpnumber] )

      }
  })
 
output$plateMapA <- renderUI({p(br(),tags$img(src = input$PlateMap, height = "300px",width = "800px"))})

output$DownloadData <- downloadHandler(
  filename <- function() {paste0("HiBiT_Data",".csv")}, 
  content <- function(file) {file.copy("HiBiT_Data.csv", file)},
  contentType = "csv")

output$DownloadData2 <- downloadHandler(
  filename <- function() {paste0("Viability_Data",".csv")}, 
  content <- function(file) {file.copy("Viability_Data.csv", file)},
  contentType = "csv")
output$DownloadData3 <- downloadHandler(
  filename <- function() {paste0("HiBiT_Data_Summary",".csv")}, 
  content <- function(file) {file.copy("Summary.csv", file)},
  contentType = "csv")
output$DownloadData4 <- downloadHandler(
  filename <- function() {paste0("Viability_Data_Summary",".csv")}, 
  content <- function(file) {file.copy("Summary2.csv", file)},
  contentType = "csv")


}

ui <- dashboardPage(
title = "HiBiT Data Processing Tool",
dashboardHeader(title = "HiBiT Data Parse",
                tags$li(a(href = '', img(src= 'Nurix.jpg', title = 'Nurix Informatics', height = "28px")), class = 'dropdown')
                ),
dashboardSidebar(useShinyjs(),useShinyalert(),
         sidebarMenu(menuItem("Data Parsing for HiBit Data", tabName = "RawData" , badgeColor = "blue")),
         sidebarMenu(menuItem("How To?", tabName = "About" , badgeColor = "blue"))
        
        ),

dashboardBody(tags$head(tags$script(type = "text/javascript", src = js)), #tags$script(jscode),
    tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}",
            ".btn {border-radius: 12px; font-family:'Verdana';font-size: 20px ;color: #fff; background-color: #337ab7; cursor: pointer;
            text-align: center;  text-decoration: none;  display: inline-block;font-size: 10px;font-stretch: expanded;
            transition-duration: 0.7s;border: 1px solid;border-color: #337ab7; padding: 10px;box-shadow: 5px 10px 8px rgba(0, 0, 0, 0.4)}"))),
    tabItems(
        tabItem(tabName = "RawData",
                fluidRow(
                    box(width = 2,
                        selectInput("PlateMap", "Select Plate Map", 
                        choices = c("11PT_16C_MEK_NC_AC_.png", "16PT_10CMPD_NC_BC.png", "10PT_3CMPDS_NC_BC_1CCR_TT.png" ), 
                                    selected = "11PT_15C_NC_BC_1CCR.png"),
                        fileInput("hibitDataFile",MMark("Select HiBiT Raw Data Files"), multiple = TRUE, 
                                  accept = c("csv", "comma-separated-values", ".csv"),
                                  width = NULL, buttonLabel = "Browse...", placeholder = "No file selected"),
                        fileInput("viabDataFile",MMark("Select Viability Data Files"), multiple = TRUE, 
                                  accept = c("csv", "comma-separated-values", ".csv"),
                                  width = NULL, buttonLabel = "Browse...", placeholder = "No file selected"),
                        actionButton("loadFiles", "Upload Data Files"),br(),br(),
                         h5(span("Note:", style = "font-family: 'Arial Black';color: DarkMagenta"),HTML('&nbsp;'),
                           span("Click [Upload Data Files] Button to upload data to initiate data processing.", 
                           style = "font-family: 'Verdana';color: blue")),br(),
                        actionButton("Process", "Process Data"),br(),br(),
                        h5(span("Note:", style = "font-family: 'Arial Black';color: DarkMagenta"),HTML('&nbsp;'),
                           span("Click [Process Data] Button after selecting the 384 well data input file in csv format. The data will populate 'Raw Data' tables.", 
                           style = "font-family: 'Verdana';color: blue")),br(),
                        selectInput("hvplates", "HiBiT/CTG Plateset to Review", choices = c("Plate Set-1", "Plate Set-2"), 
                                    selected = "Plate Set-1"),
                        selectInput("cmpdname", "Select Compound Name for DRC", choices = c(sort(PlateDetails$Compound_Name)),
                                    selected = PlateDetails$Compound_Name[1]),
                         downloadButton("DownloadData3", "Download   HiBiT   Summary"),br(),br(),
                         bsTooltip("DownloadData3", "Click to Download Summary File", "top", 
                                   options = list(container = "body")),
                         downloadButton("DownloadData4", "Download Viability Summary"),br(),br(),
                         bsTooltip("DownloadData4", "Click to Download Summary File", "top", 
                                   options = list(container = "body")),
                        downloadButton("DownloadData",  "Download  HiBiT  Data  for  Prism"),br(),br(),
                         bsTooltip("DownloadData", "Click to Download HiBit processed Data File", "top", 
                                   options = list(container = "body")),
                        downloadButton("DownloadData2", "Download Viability Data for Prism"),br(),
                         bsTooltip("DownloadData2", "Click to Download Viability processed data File", "top", 
                                   options = list(container = "body")),
                        h5(span("Note:", style = "font-family: 'Arial Black';color: DarkMagenta"),HTML('&nbsp;'),
                           span("Click on above buttons to download the Processed data files for use in PRISM.", 
                           style = "font-family: 'Verdana';color: blue")),br(),br(),
                        numericInput("cmpnum","cn", value = 1, min = 1, max = 16),
                        h4(span("This app best viewed at web browser zoom level of 50% - 70% on laptops, and at 75% - 100% on larger monitors.", 
                           style = "font-family: 'Arial Black';color: DarkMagenta")),br(), br(),hr(),
                            img(src= 'author.png', title = 'Nurix Informatics', width = "280px"),hr(),
                            br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),  
                            br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), 
                            br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                            br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),  
                            br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                            br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),  
                            br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), 
                            br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), 
                            br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),  
                            br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),  
                            br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), 
 
                    ),
                    #conditionalPanel(condition = "input.ExpType == 'CTG'",
                    
                    column(10, box(width = 12,
                                   tabPanel("",
                                            fluidRow(column(4,rHandsontableOutput("pDetail", width = "auto", height = "auto")),
                                                     column(8, uiOutput('plateMapA'),br(),actionButton("Update", "Update Compound Names"))),
                                            style = "overflow-y:scroll; max-height: 550px"),
                                   solidHeader = T, collapsible = T, collapsed = F,
                                   title = "Plate Map",status = "primary")),

                    column(10, box(width = 12,
                                   tabPanel("",
                                            rHandsontableOutput("hibitData1", width = "auto", height = "auto"),br(),
				    rHandsontableOutput("hibitData2", width = "auto", height = "auto"),
                                    style = "overflow-y:scroll; max-height: 500px"),
                                   solidHeader = T, collapsible = T, collapsed = F,
                                   title = "HiBiT Raw Data Plates 1 & 2",status = "primary")),
                    column(10, box(width = 12,
                                   tabPanel("",
                                            rHandsontableOutput("viabData1", width = "auto", height = "auto"),br(),
                                            rHandsontableOutput("viabData2", width = "auto", height = "auto"),
                                            style = "overflow-y:scroll; max-height: 500px"),
                                   solidHeader = T, collapsible = T, collapsed = F,
                                   title = "Viability [CTG] Raw Data Plates 1 & 2",status = "primary")),
                    column(10, box(width = 12,
                                   tabPanel("",
                                            fluidRow(column(6,h3(strong("HiBiT Data"))), 
                                             column(6,h3(strong("Viability [CTG] Data")))),
                                             fluidRow(column(6,d3heatmapOutput("DRHMPlot")), 
                                             column(6,d3heatmapOutput("DRHMPlot2")), 
                                            style = "overflow-y:scroll; max-height: 500px")),
                                   solidHeader = T, collapsible = T, collapsed = T,
                                   title = "Heat Maps",status = "primary")),
                    column(10, box(width = 12,
                                   tabPanel("",tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
                                            tags$script(type = "text/javascript", src = "busy.js"),
                                            fluidRow(column(6,h3(strong("HiBiT Data"))), 
                                             column(6,h3(strong("Viability [CTG] Data")))),
                                            fluidRow(column(1,actionButton("prevC", "<< Prev", width = '90px')),
                                            column(1,actionButton("nextC", "Next >>" ,width = '90px'))),
                                           fluidRow(column(6,plotOutput("drcPlot",  width = "100%", height = "500px")), 
                                           column(6,plotOutput("drcPlot2",  width = "100%", height = "500px"))),
                                           fluidRow(column(6,DT::dataTableOutput('coefTableA')), 
                                                    column(6,DT::dataTableOutput('coefTableB'))),
                                           div(class = "busy", p(span(h4(strong("DR Curves being plotted. Please wait...!"), 
                                               style = "color:brown"))), img(src = "Processing.gif"))
                                           ,style = "overflow-y:scroll; max-height: 800px"),
                                   solidHeader = T, collapsible = T, collapsed = T,
                                   title = "Dose Response Curves",status = "primary"))
                )),
        tabItem(tabName = "About",
                box(width = 12,
                    tabPanel("How To?",
                             h2(p(span("Data Pre-Processing Tool", style = "font-family: 'Arial Black';color: DarkMagenta"))),

                             HTML('<center><img src="workflow.png"></center>'),
                    ),
                    solidHeader = T, collapsible = T, title = "How To",  status = "primary"))
    ) #tabItems
) # dashboardBody
) # dashboardPage

shinyApp(ui = ui, server = server)
