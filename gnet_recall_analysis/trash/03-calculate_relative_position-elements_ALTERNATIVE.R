# 2 - elements and relative position of all other elements

for (i in 1:dim(gnet_stimdata_matricesarray)[1]){
  temp <- gnet_stimdata_matricesarray[i,,]
  rownames(temp) <- c(1:5)
  for (j in 1:dim(temp)[1]){
    temp <- cbind(temp,diffy= c((temp[j,"yrow"] - temp[1,"yrow"]),
                                (temp[j,"yrow"] - temp[2,"yrow"]),
                                (temp[j,"yrow"] - temp[3,"yrow"]),
                                (temp[j,"yrow"] - temp[4,"yrow"]),
                                (temp[j,"yrow"] - temp[5,"yrow"])),
                  diffx= c((temp[j,"xcol"] - temp[1,"xcol"]),
                           (temp[j,"xcol"] - temp[2,"xcol"]),
                           (temp[j,"xcol"] - temp[3,"xcol"]),
                           (temp[j,"xcol"] - temp[4,"xcol"]),
                           (temp[j,"xcol"] - temp[5,"xcol"])),deparse.level = 0)
  }
  colnames(temp) <- c("shape","color","yrow","xcol",
                      "diff1yrow","diff1xcol",
                      "diff2yrow","diff2xcol",
                      "diff3yrow","diff3xcol",
                      "diff4yrow","diff4xcol",
                      "diff5yrow","diff5xcol")
  if (!exists("gnet_stimdata_matricesarray_relative_position")){
    gnet_stimdata_matricesarray_relative_position <- temp
  } else if (i == 2){
    gnet_stimdata_matricesarray_relative_position <- abind(gnet_stimdata_matricesarray_relative_position, temp, along=0)
  } else {
    gnet_stimdata_matricesarray_relative_position <- abind(gnet_stimdata_matricesarray_relative_position, temp, along=1)
  }
}
rm(temp,i,j)