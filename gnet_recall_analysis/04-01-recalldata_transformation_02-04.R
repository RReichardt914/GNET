library(tidyverse)
library(abind)
library(stringr)

rawdatafiles <- c("gnet4s_orig_recall.csv","gnet4s_delay_recall.csv","gnet4s_artn_recall.csv","gnet4s_delart_recall.csv")
rawdatacondition <- c("orig","delay","artn","delart")

for (rawdatain in 1:length(rawdatafiles)){
  gnet4s_recalldataRAW <- read.csv(rawdatafiles[rawdatain])
  # we create gnet_recalldata_matricesarray & gnet_recalldata_matricesarray_stimulusID
  source('02-recalldata_matrix_transformation.R')
  # generate gnet_recalldata_matricesarray_pairs
  source('04-calculate_relative_position_recall.R')
  
  eval(parse(text=paste("gnet_recalldata_matricesarray_",rawdatacondition[rawdatain]," <- gnet_recalldata_matricesarray",sep="")))
  eval(parse(text=paste("gnet_recalldata_matricesarray_stimulusID_",rawdatacondition[rawdatain]," <- gnet_recalldata_matricesarray_stimulusID",sep="")))
  eval(parse(text=paste("gnet_recalldata_matricesarray_pairs_",rawdatacondition[rawdatain]," <- gnet_recalldata_matricesarray_pairs",sep="")))
  
  rm(gnet_recalldata_matricesarray,gnet_recalldata_matricesarray_stimulusID,gnet_recalldata_matricesarray_pairs)
}





