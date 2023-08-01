library(cyjShiny)
library(later)
library(shinyjs)
library(DT)

df_list<- readRDS("demoDfList.rds")

srcContent<- readRDS("srcContent.rds")
allNodeNames<- readRDS("NodeNames.rds")
#data<-srcContent[["nci60"]][["molPharmData"]][["exp"]] 

#data<- readRDS("GeneTempData.rds")


#old Choices choices=c("","WNT","TP53","MYC","RTK-RAS","NOTCH","TGF-Beta","PI3K","NRF","HIPPO","CTNNB1")

#dataSmall <- data[1:1000,1:60]
#saveRDS(dataSmall,'Data.rds')


ui = shinyUI(fluidPage(
  useShinyjs(), #enable shinyjs
  tags$head(tags$style("#cyjShiny{height:95vh !important;}")),
  titlePanel(title="Example"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cellLineSet","Cell line Set",c("","nci60","ccle")),
      selectInput("selectPathwayType","Select Pathway",c("","Upload Pathway","Select using Gene")),
      conditionalPanel(
        condition = "input.selectPathwayType=='Select using Gene'",
        selectInput("selectGene","Select Gene: ",choices=c("",allNodeNames))
      ),
      uiOutput("fileInputUI"),
      hidden(selectInput("selectPathway","Select Pathway: ",choices =c(""))),
      hidden(selectInput("options","Select Cell Line or Tissue",c("","Cell line","Tissue"))),
      
      conditionalPanel(
        condition = "input.options=='Cell line'",
        selectizeInput("selectCellLine","Select Cell Line",choices=NULL),
      ),
      conditionalPanel(
        condition = "input.options=='Tissue'",
        selectizeInput("selectTissue","Select Tissue",c("")),
      ),
      hidden(uiOutput("rangeSlider")),
      hidden(plotOutput("colorPlot",height = "50px",width="auto")),
      
      selectInput("selectNode", "Select Node by ID:", choices = c("")),
    
      textOutput("status"),
      actionButton("fit", "Fit Graph"),
      actionButton("fitSelected", "Fit Selected"),
      #actionButton("getSelectedNodes", "Get Selected Nodes"), HTML("<br><br>"),
      #htmlOutput("selectedNodesDisplay"),
      dataTableOutput("table"),
      width=3
    ),
    mainPanel(cyjShinyOutput('cyjShiny', height=400),
              width=9)
  ) # sidebar Layout
))
#----------------------------------------------------------------------------------------------------
server = function(input, output, session)
{
  observeEvent(input$fit, ignoreInit=TRUE, {
    fit(session, 80)
  })
  
  reactiveData<-reactiveVal()
  reactiveAverage<- reactiveVal()
  reactiveDfList<-reactiveVal(df_list)
  
  output$fileInputUI <- renderUI({
    if (input$selectPathwayType == "Upload Pathway") {
      fileInput("file", "Choose a file")
    } else {
      NULL
    }
  })
  
  observeEvent(input$cellLineSet,ignoreInit = TRUE,{
    
    data<- srcContent[[input$cellLineSet]][["molPharmData"]][["exp"]]
    
    reactiveData(data)
    tissueToSampleMap<<- srcContent[[input$cellLineSet]][["tissueToSamplesMap"]]
    tissueNames<- names(tissueToSampleMap)
    cellLines<- as.list(colnames(data))
    
    updateSelectizeInput(session,"selectCellLine",choices = c("",cellLines))
    updateSelectizeInput(session,"selectTissue",choices = c("",tissueNames))
  })

  observeEvent(input$selectGene,  ignoreInit=TRUE,{
    if(input$selectGene != ""){
      show("selectPathway")
      gene<- input$selectGene
      df_list<-reactiveDfList()
      extrapathwayNames <- names(df_list)[grep(input$selectGene, names(df_list))]
      pathwayChoices <- extrapathwayNames
        
      for (pathway_name in names(df_list)) {
        pathway <- df_list[[pathway_name]][[1]]
        genes <- pathway[["NodeName"]]
        if (gene %in% genes) {
          pathwayChoices <- c(pathwayChoices, pathway_name)
        }
      }
      pathwayChoices<-unique(pathwayChoices)
      updateSelectInput(session, "selectPathway",choices = pathwayChoices)
      if(length(pathwayChoices)==0){
        updateSelectInput(session,"selectPathway",choices="No Pathway exists")
      }
      updateSelectInput(session,"selectGene",selected = input$selectGene)
    }
  })
  
  observeEvent(input$selectPathway,ignoreInit = TRUE,{
    
    if(input$selectPathway!="" & input$selectPathway!="No Pathway exists"){
      show("options")
      df_list<-reactiveDfList()
      forNodes<- df_list[[input$selectPathway]][[1]]
      forEdges<- df_list[[input$selectPathway]][[2]]
      namesOfNodes <- forNodes[["NodeName"]]
      
      updateSelectInput(session,"selectPathway",selected = input$selectPathway)
      updateSelectInput(session,"selectNode",choices = c("",namesOfNodes))
    }
  })
  
  displayGraph<- function(averageValues,minVal,maxVal){
    df_list<-reactiveDfList()

    forNodes<- df_list[[input$selectPathway]][[1]]
    forEdges<- df_list[[input$selectPathway]][[2]]
    namesOfNodes <- forNodes[["NodeName"]]
    if(minVal==0)minVal=-0.0001
    if(maxVal==0)maxVal=0.0001
    rescaledAverage <- ifelse(is.na(averageValues), NA,
                               ifelse(averageValues >= 0,
                                      averageValues * 10 / maxVal,
                                      averageValues * -10 / minVal ))
    
    print(rescaledAverage)
    tbl.nodes <- data.frame(id=forNodes[["NodeID"]],
                            name=forNodes[["NodeName"]],
                            x=forNodes[["PosX"]],
                            y=forNodes[["PosY"]],
                            avgValues=rescaledAverage,
                            parent=forNodes[["ParentId"]],
                            nodeType=forNodes[["NodeType"]],
                            stringsAsFactors=FALSE)
    
    #family: e0e0ff, compartment: ffe0e0, complex: white
    
    tbl.edges <- data.frame(source=forEdges[["Source"]],
                            target=forEdges[["Target"]],
                            interaction=forEdges[["EdgeType"]],
                            stringsAsFactors=FALSE)
    
    graph.json <- dataFramesToJSON(tbl.edges, tbl.nodes)

    output$cyjShiny <- renderCyjShiny({
      cyjShiny(graph=graph.json, layoutName="preset",styleFile ="basicStyle.js")
    })
    
    displayColorPlot(maxVal,minVal)
  }
  
  
  displayTable <-function(selectedCells){
    data<- reactiveData()
    cellLineData<-data[,selectedCells,drop=FALSE]
    
    #print(cellLineData)
    df_list<-reactiveDfList()
    namesOfNodes <- df_list[[input$selectPathway]][[1]][["NodeName"]]
    names <- as.list(row.names(data))
    tableValuesAverages<- c()
    tableValuesMedians<-c()
    
    for (nodeName in namesOfNodes) {
      # Construct the name of the gene in the "exp" data frame
      nameInData <- paste0("exp", nodeName)
     # print(nameInData )
      if (nameInData %in% names) {
      #  print(nameInData)
        # Calculate the average value for the gene  
        cellVal <- mean(as.numeric(cellLineData[nameInData,]),na.rm=TRUE)
        cellMed <- median(as.numeric(cellLineData[nameInData,],na.rm=TRUE))
        
        cellVal <- round(cellVal, 3) 
        cellMed <- round(cellMed, 3)
        
        tableValuesAverages <- c(tableValuesAverages, cellVal)
        tableValuesMedians <- c(tableValuesMedians,cellMed)
        
      } else {
        tableValuesAverages <- c(tableValuesAverages, NA)
        tableValuesMedians <- c(tableValuesMedians,NA)
      }
    } 
    
    reactiveAverage(tableValuesAverages)
    maxVal<- max(c(tableValuesAverages,0.0001),na.rm = TRUE)
    minVal<- min(c(tableValuesAverages,-0.0001),na.rm=TRUE)
    
    if(length(selectedCells)>1){
    tableValuesDataFrame <- data.frame(Name = namesOfNodes, Average=tableValuesAverages, Median=tableValuesMedians)
    }
    else {
      tableValuesDataFrame <- data.frame(Name = namesOfNodes, Value=tableValuesAverages)
    }
    
    tableValuesDataFrame <- tableValuesDataFrame[!is.na(tableValuesDataFrame$Name) & tableValuesDataFrame$Name != "",]
    
    #print(tableValuesDataFrame)
    
    
    
    # Display the data table using DT package
    output$table <- renderDataTable({
      datatable(tableValuesDataFrame)
    })
   
    if(length(tableValuesAverages)>1){
      show("rangeSlider")
      output$rangeSlider<- renderUI({
        sliderInput("rangeSlider", "Value Range", 
        min = minVal, max = maxVal, value=c(minVal,maxVal))
      })
    }
    return (tableValuesAverages)
  }
  displayColorPlot<-function(maxVal,minVal){
    show("colorPlot")
    output$colorPlot<- renderPlot({
      par(mar = c(0, 0, 0, 0)) # Set all margins to 0
      plot(NULL, xaxt='n', yaxt='n', bty='n', ylab='', xlab='', xlim=0:1, ylim=0:1)
      legend("center", legend =c(maxVal,'0.0', minVal),pt.cex=3, cex=1.5,bty='n',
             fill = c('blue', 'white', 'red'), horiz=TRUE)
      
    })
  } 
  
  observeEvent(input$rangeSlider,{
    sliderValues<- input$rangeSlider
    
    #setting condition that first slider cannot go beyond zero and second cannot come below zero
    if(sliderValues[1]>0){
      updateSliderInput(session,"rangeSlider",value=c(-0.001,sliderValues[2]))
    }
    if(sliderValues[2]<0){
      updateSliderInput(session,"rangeSlider",value=c(sliderValues[1],0.001))
    }
    averageValues<-reactiveAverage()
    displayGraph(averageValues,sliderValues[1],sliderValues[2])
    })
  
  observeEvent(input$selectCellLine,ignoreInit = TRUE,{
    if(input$selectCellLine!=""){
    selectedCellLine<- input$selectCellLine
    #print(length(reactiveDfList()))
    #print(is.vector(selectedCellLine))
    if(input$selectPathway!="No Pathway exists"){
      #print(input$selectPathway)
      averages<- displayTable(selectedCellLine)
      maxVal<- max(c(averages,0.0),na.rm = TRUE)
      minVal<- min(c(averages,0.0),na.rm=TRUE)
      
      displayGraph(averages,maxVal,minVal)
    }
    }
  })
  
  # convertToDot <- function(vec){
  #   return (gsub("[-\\:\\ ]", ".", vec))
  # }
  
  observeEvent(input$selectTissue,ignoreInit = TRUE,{
    if(input$selectTissue!=""){
      selectedTissue<- input$selectTissue
      selectedCellLines <-tissueToSampleMap[selectedTissue]
      
      #print(selectedCellLines)
      
      #selectedCellLines <- lapply(selectedCellLines,convertToDot)
      
      selectedCellLines <- unlist(selectedCellLines,use.names=FALSE)
      #print(selectedCellLines)
      
      averages<-displayTable(selectedCellLines)
      reactiveAverage(averages)
      maxVal<- max(c(averages,0.0),na.rm = TRUE)
      minVal<- min(c(averages,0.0),na.rm=TRUE)
      displayGraph(averages,minVal,maxVal)
    }
  })
  #DONE
  
  # Event handler for file upload
  observeEvent(input$file, {
    # Check if a file is uploaded
    if (!is.null(input$file)) {
      # Read the file
      
      data <- readLines(input$file$datapath)
      
      # Initialize empty vectors for nodes and edges
      node_lines <- c()
      edge_lines <- c()
      edgeLineStart<-0
      nodeLineStart<-0
      # Find the lines containing node and edge information
      for (i in 1:length(data)) {
        line <- data[i]
        
        if (grepl("--NODE_NAME", line))
        {nodeLineStart<- i
        }
        else if(grepl("--EDGE_ID",line)){
          edgeLineStart<- i
          break
        }
      }
      node_lines<- data[(nodeLineStart+1):(edgeLineStart-1)]
      edge_lines<- data[(edgeLineStart+1):length(data)]
      
      nodeData<- paste(node_lines, collapse = "\t")
      edgeData<- paste(edge_lines,collapse = "\t")
      
      nodefields <- strsplit(nodeData, "\t")
      edgesfields <- strsplit(edgeData ,"\t")
      
      node_df_pathway <- data.frame(matrix(unlist(nodefields), ncol = 8, byrow=TRUE))
      edge_df_pathway <- data.frame(matrix(unlist(edgesfields), ncol = 8, byrow=TRUE))
      
      colnames(node_df_pathway) <- c("NodeName", "NodeID", "NodeType", "ParentId", "PosX", "PosY","Width","Height")
      # Convert PosX and PosY columns to numeric data type: this was needed in later part
      node_df_pathway$PosX <- as.double(node_df_pathway$PosX)
      node_df_pathway$PosY <- as.double(node_df_pathway$PosY)
      
      
      colnames(edge_df_pathway) <- c("EdgeID", "Source", "Target", "EdgeType","Interaction","EdgeName","Bend","Curve")
      df_list<-reactiveDfList()
      fileName<-input$file$name 
      df_list[[fileName]] <- list(node_df_pathway, edge_df_pathway)
      reactiveDfList(df_list)
      #print(fileName)
      #print(df_list[[fileName]])
      show("selectPathway")
      updateSelectInput(session,"selectPathway",choices=c(fileName),selected = fileName)
      print(input$selectPathway)
      show("options")
      
      # 
      # tbl.nodes <- data.frame(id=node_df_pathway[["NodeID"]],
      #                         x=node_df_pathway[["PosX"]],
      #                         y=node_df_pathway[["PosY"]],
      #                         name=node_df_pathway[["NodeName"]],
      #                         parent=node_df_pathway[["ParentId"]],
      #                         nodeType=node_df_pathway[["NodeType"]],
      #                         stringsAsFactors=FALSE)
      # 
      # 
      # 
      # tbl.edges <- data.frame(source=edge_df_pathway[["Source"]],
      #                         target=edge_df_pathway[["Target"]],
      #                         interaction=edge_df_pathway[["EdgeType"]],
      #                         stringsAsFactors=FALSE)
      # 
      # updateSelectInput(session,"selectNode",choices = c("",node_df_pathway[["NodeName"]]))
      # removeGraph(session)
      # graph.json <- dataFramesToJSON(tbl.edges, tbl.nodes)
      # output$cyjShiny <- renderCyjShiny({
      #   cyjShiny(graph=graph.json, layoutName="preset",styleFile ="basicStyle.js")
      # })

    }
  })
  
  
  #DONE
  observeEvent(input$selectNode,  ignoreInit=TRUE,{
    forNodes<- df_list[[input$selectPathway]][[1]]
    selectedNodeID <- forNodes$NodeID[forNodes$NodeName == input$selectNode]
    selectNodes(session, selectedNodeID)
  })
  
  #DONE
  observeEvent(input$fitSelected,  ignoreInit=TRUE,{
    fitSelected(session, 100)
  })
  
  #DONE
  observeEvent(input$getSelectedNodes, ignoreInit=TRUE, {
    output$selectedNodesDisplay <- renderText({" "})
    getSelectedNodes(session)
  })
  
  output$value <- renderPrint({ input$action })
  
  
  
} # server
#----------------------------------------------------------------------------------------------------
app <- shinyApp(ui = ui, server = server)
