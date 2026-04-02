# calculating relative position of elements in original and recalled stimuli
library(tidyverse)
library(abind)

# 1 - pairs of elements

for (i in 1:dim(gnet_random_stimdata_matricesarray)[1]){
  # temp - tárolja az eredeti adatokat
  temp <- gnet_random_stimdata_matricesarray[i,,]
  # el kell nevezni a sorait ()
  rownames(temp) <- c(1:5)
  
  # kivesszük a temp-ből az elempárok első elemeit 
  pairs_data_1 <- matrix(nrow = 0, ncol = 4)
  for (j in 1:(dim(temp)[1]-1)){
    for (k in 1:(dim(temp)[1]-j)){
      pairs_data_1 <- rbind(pairs_data_1,temp[j,])
    }
  }
  # kivesszük a temp-ből az elempárok második elemeit
  pairs_data_2 <- matrix(nrow = 0, ncol = 4)
  for (j in 1:(dim(temp)[1]-1)){
    for (k in 1:(dim(temp)[1]-j)){
      pairs_data_2 <- rbind(pairs_data_2,temp[dim(temp)[1]+1-k,])
    }
  }
  colnames(pairs_data_2) <- c("shape2","color2","yrow2","xcol2")
  pairs_data <- cbind(pairs_data_1,pairs_data_2)
  rm(pairs_data_1,pairs_data_2)
  
  pairs_data <- cbind(pairs_data, diffy = pairs_data[,"yrow"] - pairs_data[,"yrow2"])
  pairs_data <- cbind(pairs_data, diffx = pairs_data[,"xcol"] - pairs_data[,"xcol2"])
  
  if (!exists("gnet_random_stimdata_matricesarray_pairs")){
    gnet_random_stimdata_matricesarray_pairs <- pairs_data
  } else if (i == 2){
    gnet_random_stimdata_matricesarray_pairs <- abind(gnet_random_stimdata_matricesarray_pairs, pairs_data, along=0)
  } else {
    gnet_random_stimdata_matricesarray_pairs <- abind(gnet_random_stimdata_matricesarray_pairs, pairs_data, along=1)
  }
  rm(pairs_data)
}
rm(temp,i,j,k)

