# png("colors.png",width=450,height=200)
# plot(NULL, xaxt='n', yaxt='n', bty='n', ylab='', xlab='', xlim=0:1, ylim=0:1)
# legend("topleft", legend =c('Max Val', 'Mid Val', 'Min Val'),pt.cex=3, cex=1.5,bty='n',
#        fill = c('blue', 'white', 'red'), horiz=TRUE)
# mtext("Values", at=0.2, cex=2)
# dev.off()



# if(input$cellLineSet =="NCI-60"){
#   
#   data<<- read.csv("GeneTempData.csv",row.names = 1)
#   cellLines<- as.list(colnames(data))
#   #print(data)
#   
#   
#    
# }
# else{
#   #data<<- molData@eSetList[["exp"]]@assayData[["exprs"]]
#   data<<- read.csv("CCLE-AllGenesData.csv",row.names = 1,check.names = FALSE)
#   cellLines<- molData@sampleData@samples[["Name"]]
#   tissuesMapping<- molData@sampleData@samples[["OncoTree1"]]
#   
#   tissueToSampleMap<<- split(cellLines,tissuesMapping)
#   
#   tissueNames<- names(tissueToSampleMap)
# }