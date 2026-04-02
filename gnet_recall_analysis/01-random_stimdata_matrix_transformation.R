library(tidyverse)
library(abind)

gnet_random_stimdata <- read.csv("random_images.csv")


stimulusID <- rep(1:35, each = 4)


for (i in 1:(nrow(gnet_random_stimdata)/3)){
  temp <- as_tibble(matrix(nrow = 5, ncol = 4), .name_repair = ~ c("shape","color","yrow","xcol"))
  k <- 0
  #stimulusID <- c(stimulusID,i)
  for (j in 2:ncol(gnet_random_stimdata)){
    if (gnet_random_stimdata[(i-1)*3+1,j] == 1){
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
      temp[k,"color"] <- gnet_random_stimdata[(i-1)*3+2,j]
      temp[k,"shape"] <- gnet_random_stimdata[(i-1)*3+3,j]
    }
  }
  if (!exists("gnet_random_stimdata_matricesarray")){
    gnet_random_stimdata_matricesarray <- temp
  } else if (i == 2){
    gnet_random_stimdata_matricesarray <- abind(gnet_random_stimdata_matricesarray, temp, along=0)
  } else {
    gnet_random_stimdata_matricesarray <- abind(gnet_random_stimdata_matricesarray, temp, along=1)
  }
}

gnet_random_stimdata_matricesarray_stimulusID <- stimulusID

rm(temp,i,j,k,stimulusID)
# output : gnet_stimdata_matricesarray
# az ingerek új logika szeinti leképezéseit tartalmazó 3d mátrix - array
