
rawdatacondition <- c("orig","delay","artn","delart")

for (rawdatain in 1:length(rawdatacondition)){
  eval(parse(text=paste("gnet_recalldata_matricesarray <- gnet_recalldata_matricesarray_",rawdatacondition[rawdatain],sep="")))
  eval(parse(text=paste("gnet_recalldata_matricesarray_stimulusID <- gnet_recalldata_matricesarray_stimulusID_",rawdatacondition[rawdatain],sep="")))
  eval(parse(text=paste("gnet_recalldata_matricesarray_pairs <- gnet_recalldata_matricesarray_pairs_",rawdatacondition[rawdatain],sep="")))
  
  source('06-autocomparator_for_recalltest.R')
  
  eval(parse(text=paste("final_recollection_data_",rawdatacondition[rawdatain]," <- final_recollection_data",sep="")))
  rm(final_recollection_data)
}

