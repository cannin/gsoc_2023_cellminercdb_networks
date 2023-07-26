

library(jsonlite)
# Read the JSON file
pathways <- jsonlite::fromJSON("pathways.json")

# Initialize data frame for node and edges fields
node_df <- data.frame()
edge_df <- data.frame()
df_list <- list()


# Initialize a vector to store all node names
allNodeNames <- c()

#for demo pathways
#demoPathways <-c("Cell Cycle","HIPPO","MYC","NOTCH","WNT","TP53","NRF2","PI3K","RTK-RAS","TGF-Beta") 
# Loop through each pathway
for (pathway in pathways) {
  # Get the nodes and edges
  
  #For demo pathways
  #if(!(pathway[1] %in% demoPathways)){
  # next
  #}
  
  node1 <- pathway[grep("--NODE_NAME", pathway):grep("--EDGE_ID", pathway)-1]
  edges <- pathway[grep("--EDGE_ID", pathway):length(pathway)]
  
  # Split nodes and edges on "\t" delimiter
  nodefields <- strsplit(node1, "\t")
  edgesfields <- strsplit(edges ,"\t")
  
  node_df_pathway <- data.frame(matrix(unlist(nodefields), ncol = 6, byrow=TRUE))
  edge_df_pathway <- data.frame(matrix(unlist(edgesfields), ncol = 4, byrow=TRUE))
  
  colnames(node_df_pathway) <- c("NodeName", "NodeID", "NodeType", "ParentId", "PosX", "PosY")
  # Convert PosX and PosY columns to numeric data type:: this was needed in later part( In cyjshiny) 
  node_df_pathway$PosX <- as.double(node_df_pathway$PosX)
  node_df_pathway$PosY <- as.double(node_df_pathway$PosY)
  
  
  
  colnames(edge_df_pathway) <- c("EdgeID", "Source", "Target", "EdgeType")
  
  
  
  # Remove empty rows
  node_df <- node_df[rowSums(node_df == "") != ncol(node_df),]
  edge_df <- edge_df[rowSums(edge_df == "") != ncol(edge_df),]
  
  node_df_pathway<- node_df_pathway[-1,]
  edge_df_pathway<- edge_df_pathway[-1,]
  
  df_list[[pathway[1]]] <- list(node_df_pathway, edge_df_pathway)
  
  allNodeNames <- c(allNodeNames, node_df_pathway$NodeName)
}


# Remove duplicates from the vector
allNodeNames <- unique(allNodeNames)


saveRDS(allNodeNames,file="NodeNames.rds")

saveRDS(df_list, file = "df_list.rds")




