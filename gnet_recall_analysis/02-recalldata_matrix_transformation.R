library(tidyverse)
library(abind)
library(stringr)
# this is the RAW recall test data from the no narrative, no delay group
# gnet4s_recalldataRAW <- read.csv("gnet4s_orig_recall.csv")
gnet4s_recalldataRAW <- read.csv("delay_rcll_revised.csv")

# this is the name of the final array - gnet_recalldata_matricesarray
gnet_recalldata_matricesarray_stimulusID <- c()

for (i in 1:nrow(gnet4s_recalldataRAW)){
  temp <- as_tibble(matrix(nrow = 5, ncol = 4), .name_repair = ~ c("shape","color","yrow","xcol"))
  
  gnet_recalldata_matricesarray_stimulusID <- c(gnet_recalldata_matricesarray_stimulusID,gnet4s_recalldataRAW[i,"multichar_knowresp"])
  
  for (j in 1:5){
    # szín kinyerése
    temp[j,"color"] <- gnet4s_recalldataRAW[i,colnames(gnet4s_recalldataRAW)[str_detect(colnames(gnet4s_recalldataRAW), paste(text="plc",j,sep=""))][2]]
    # alakzat kinyerése
    temp_shape <- gnet4s_recalldataRAW[i,colnames(gnet4s_recalldataRAW)[str_detect(colnames(gnet4s_recalldataRAW), paste(text="plc",j,sep=""))][4]]
    if (temp_shape == "sqa"){
      temp[j,"shape"] <- 1
    } else if (temp_shape == "tri"){
      temp[j,"shape"] <- 2
    } else if (temp_shape == "cir"){
      temp[j,"shape"] <- 3
    }
    # y koord kinyerése
    temp_yrow <- gnet4s_recalldataRAW[i,colnames(gnet4s_recalldataRAW)[str_detect(colnames(gnet4s_recalldataRAW), paste(text="plc",j,sep=""))][6]]
    if (temp_yrow == -120){
      temp[j,"yrow"] <- 1
    } else if (temp_yrow == 0){
      temp[j,"yrow"] <- 2
    } else if (temp_yrow == 120){
      temp[j,"yrow"] <- 3
    }
    # x koord kinyerése
    temp_xcol <- gnet4s_recalldataRAW[i,colnames(gnet4s_recalldataRAW)[str_detect(colnames(gnet4s_recalldataRAW), paste(text="plc",j,sep=""))][5]]
    if (temp_xcol == -120){
      temp[j,"xcol"] <- 1
    } else if (temp_xcol == 0){
      temp[j,"xcol"] <- 2
    } else if (temp_xcol == 120){
      temp[j,"xcol"] <- 3
    }
  }
  if (!exists("gnet_recalldata_matricesarray")){
    gnet_recalldata_matricesarray <- temp
  } else if (i == 2){
    gnet_recalldata_matricesarray <- abind(gnet_recalldata_matricesarray, temp, along=0)
  } else {
    gnet_recalldata_matricesarray <- abind(gnet_recalldata_matricesarray, temp, along=1)
  }
}

rm(temp,i,j,k,temp_shape,temp_yrow,temp_xcol)
# output : gnet_stimdata_matricesarray
# az ingerek új logika szeinti leképezéseit tartalmazó 3d mátrix - array