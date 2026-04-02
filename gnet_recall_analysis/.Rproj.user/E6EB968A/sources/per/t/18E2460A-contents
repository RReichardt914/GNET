library(tidyverse)
library(abind)

gnet_orig_stimdata <- read.csv("gnet_orig_stimdata.csv")

stimulusID <- c()

for (i in 1:(nrow(gnet_orig_stimdata)/3)){
  temp <- as_tibble(matrix(nrow = 5, ncol = 4), .name_repair = ~ c("shape","color","yrow","xcol"))
  k <- 0
  stimulusID <- c(stimulusID,gnet_orig_stimdata[(i-1)*3+1,1])
  for (j in 2:ncol(gnet_orig_stimdata)){
    if (gnet_orig_stimdata[(i-1)*3+1,j] == 1){
      k <- k + 1
      if (j-1 <= 3){
        temp[k,"xcol"] <- j-1
        temp[k,"yrow"] <- 3
      } else if (j-1 <= 6) {
        temp[k,"xcol"] <- j-4
        temp[k,"yrow"] <- 2
      } else {
        temp[k,"xcol"] <- j-7
        temp[k,"yrow"] <- 1
      }
      temp[k,"color"] <- gnet_orig_stimdata[(i-1)*3+2,j]
      temp[k,"shape"] <- gnet_orig_stimdata[(i-1)*3+3,j]
    }
  }
  if (!exists("gnet_stimdata_matricesarray")){
    gnet_stimdata_matricesarray <- temp
  } else if (i == 2){
    gnet_stimdata_matricesarray <- abind(gnet_stimdata_matricesarray, temp, along=0)
  } else {
    gnet_stimdata_matricesarray <- abind(gnet_stimdata_matricesarray, temp, along=1)
  }
}

gnet_stimdata_matricesarray_stimulusID <- stimulusID

rm(temp,i,j,k,stimulusID)
# output : gnet_stimdata_matricesarray
# az ingerek új logika szeinti leképezéseit tartalmazó 3d mátrix - array
