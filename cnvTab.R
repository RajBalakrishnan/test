tabPanel("DNA Copy Number", value = "CNVpanel",
         #mainPanel(
         br(),
         fluidPage(
           tabsetPanel(
             tabPanel("CN Frequency", 
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          width=2,
                          selectInput("choiceFreqCN", "Sort the plot by:", choices=c("Amplification","Deletion","Amp+Del"))
                          #actionButton("updateFreqCN", "Plot")
                        ), 
                        mainPanel(
                          withSpinner(plotlyOutput("freqPlotCN",width="1200px", height="600px"), type=5),
                          withSpinner(plotlyOutput("qPlotCN",   width="1100px", height="600px"), type=5),
                          helpText("* log10(q-values) from GISTIC2 results for assessing the abbrent genomic region where the gene is mapped to in terms of their CN changes within each TCGA cohort are plotted.  Only those with q-values < 0.25 are shown."),
                          helpText("** About GISTIC: The GISTIC identifies regions of the genome that are significantly amplified or deleted across a set of samples.",
                                   "Each aberration is assigned a G-score that considers the amplitude of the aberration as well as the frequency of its occurrence across samples.", 
                                   "False Discovery Rate q-values are then calculated for the aberrant regions, and regions with q-values below a user-defined threshold are considered significant.",  
                                   "For each significant region, a “peak region” is identified, which is the part of the aberrant region with greatest amplitude and frequency of alteration.",  
                                   "In addition, a “wide peak” is determined using a leave-one-out algorithm to allow for errors in the boundaries in a single sample.", 
                                   "The “wide peak” boundaries are more robust for identifying the most likely gene targets in the region.", 
                                   "Each significantly aberrant region is also tested to determine whether it results primarily from broad events (longer than half a chromosome arm), focal events, or significant levels of both.",  
                                   "The GISTIC reports the genomic locations and calculated q-values for the aberrant regions.",  
                                   "It identifies the samples that exhibit each significant amplification or deletion, and it lists genes found in each “wide peak” region.",
                                   "For more details, please see: ",
                                   a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3218867/", target="_blank", "Mermel C. et al. Genome Bio. 2011"),
                                   ", ",
                                   a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2826709/", target="_blank", "Beroukhim R. et al. Nature 2010"),
                                   ", and ",
                                   a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2148413/", target="_blank", "Beroukhim R. et al. PNAS 2007")),
                          
                          br(),
                          h3("Genes that co-reside in the significant peaks according to GISTIC calls at .99 confidence."),
                          DT::dataTableOutput("coPeakGenesCN", width="100%")
                        )
                      )
             ), 
             tabPanel("Gene Expression vs CN",
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          width=2,
                          selectInput("datasetGxpCN", "Choose a tumor type:", choices=c("All", sort(c(setdiff(unique(sampleinfo.TCGA$TUMOR_TYPE),"FPPP"),c("BRCA-Basal","BRCA-Her2","BRCA-Luminal","HNSC-HPVneg","HNSC-HPVpos")))))
                          #actionButton("updateGxpCN", "Plot")
                        ),
                        mainPanel(
                          withSpinner(plotlyOutput("plotGxpVsCN",width="850px", height="800px"), type=5)
                        )
                      )
             ),
                    
            tabPanel("Copy Number in Cell Lines", htmlOutput("frameCNR")
	   )
                          
           )
         )
)
  
