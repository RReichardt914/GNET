
original_stimulus <- gnet_stimdata_matricesarray_pairs[1,,]
recalled_stimulus <- gnet_stimdata_matricesarray_pairs[2,,]

comparison_table <- matrix(nrow = 100, ncol = 13)
colnames(comparison_table) <- c("orig_rownum","recall_rownum","shape","color","yrow","xcol","shape2","color2","yrow2","xcol2","diffy","diffx","sum_contrast")
for (i in 1:nrow(original_stimulus)){
  for (j in 1:nrow(recalled_stimulus)){
    comparison_table[(i-1)*10+j,"orig_rownum"] <- i
    comparison_table[(i-1)*10+j,"recall_rownum"] <- j
    comparison_table[(i-1)*10+j,"shape"] <- as.numeric(ifelse(original_stimulus[i,"shape"] == recalled_stimulus[j,"shape"],0,1))
    comparison_table[(i-1)*10+j,"color"] <- as.numeric(ifelse(original_stimulus[i,"color"] == recalled_stimulus[j,"color"],0,1))
    comparison_table[(i-1)*10+j,"yrow"] <- abs(original_stimulus[i,"yrow"] - recalled_stimulus[j,"yrow"])
    comparison_table[(i-1)*10+j,"xcol"] <- abs(original_stimulus[i,"xcol"] - recalled_stimulus[j,"xcol"])
    comparison_table[(i-1)*10+j,"shape2"] <- as.numeric(ifelse(original_stimulus[i,"shape2"] == recalled_stimulus[j,"shape2"],0,1))
    comparison_table[(i-1)*10+j,"color2"] <- as.numeric(ifelse(original_stimulus[i,"color2"] == recalled_stimulus[j,"color2"],0,1))
    comparison_table[(i-1)*10+j,"yrow2"] <- abs(original_stimulus[i,"yrow2"] - recalled_stimulus[j,"yrow2"])
    comparison_table[(i-1)*10+j,"xcol2"] <- abs(original_stimulus[i,"xcol2"] - recalled_stimulus[j,"xcol2"])
    comparison_table[(i-1)*10+j,"diffy"] <- abs(abs(original_stimulus[i,"diffy"]) - abs(recalled_stimulus[j,"diffy"]))
    comparison_table[(i-1)*10+j,"diffx"] <- abs(abs(original_stimulus[i,"diffx"]) - abs(recalled_stimulus[j,"diffx"]))
    comparison_table[(i-1)*10+j,"sum_contrast"] <- sum(comparison_table[(i-1)*10+j,c("shape","color","yrow","xcol","shape2","color2","yrow2","xcol2","diffy","diffx")])
  }
}

overall_contrast_possibilities <- matrix(nrow = 0, ncol = 21)

r <- 0

for (i in 1:nrow(comparison_table[comparison_table[, "orig_rownum"] == 1,])){
  
  current_contrast <- matrix(nrow = 1, ncol = 21)
  
  current_contrast[1,1] <- comparison_table[comparison_table[,"orig_rownum"] == 1,][i,"orig_rownum"]
  current_contrast[1,2] <- comparison_table[comparison_table[,"orig_rownum"] == 1,][i,"recall_rownum"]
  current_contrast[1,21] <- comparison_table[comparison_table[,"orig_rownum"] == 1,][i,"sum_contrast"]
  temp <- comparison_table[!(comparison_table[,"orig_rownum"] == 1) & !(comparison_table[,"recall_rownum"] == i),]
  
  for (j in 1:nrow(temp[temp[, "orig_rownum"] == 2,])){
    
    current_contrast2 <- current_contrast
    
    current_contrast2[1,3] <- temp[temp[,"orig_rownum"] == 2,][j,"orig_rownum"]
    current_contrast2[1,4] <- temp[temp[,"orig_rownum"] == 2,][j,"recall_rownum"]
    current_contrast2[1,21] <- current_contrast[1,21] + temp[temp[,"orig_rownum"] == 2,][j,"sum_contrast"]
    temp2 <- temp[!(temp[,"orig_rownum"] == 2) & !(temp[,"recall_rownum"] == temp[temp[,"orig_rownum"] == 2,][j,"recall_rownum"]),]
    
    for (k in 1:nrow(temp2[temp2[, "orig_rownum"] == 3,])){
      
      current_contrast3 <- current_contrast2
      
      current_contrast3[1,5] <- temp2[temp2[,"orig_rownum"] == 3,][k,"orig_rownum"]
      current_contrast3[1,6] <- temp2[temp2[,"orig_rownum"] == 3,][k,"recall_rownum"]
      current_contrast3[1,21] <- current_contrast2[1,21] + temp2[temp2[,"orig_rownum"] == 3,][k,"sum_contrast"]
      temp3 <- temp2[!(temp2[,"orig_rownum"] == 3) & !(temp2[,"recall_rownum"] == temp2[temp2[,"orig_rownum"] == 3,][k,"recall_rownum"]),]
      
      for (l in 1:nrow(temp3[temp3[, "orig_rownum"] == 4,])){
        
        current_contrast4 <- current_contrast3
        
        current_contrast4[1,7] <- temp3[temp3[,"orig_rownum"] == 4,][l,"orig_rownum"]
        current_contrast4[1,8] <- temp3[temp3[,"orig_rownum"] == 4,][l,"recall_rownum"]
        current_contrast4[1,21] <- current_contrast3[1,21] + temp3[temp3[,"orig_rownum"] == 4,][l,"sum_contrast"]
        temp4 <- temp3[!(temp3[,"orig_rownum"] == 4) & !(temp3[,"recall_rownum"] == temp3[temp3[,"orig_rownum"] == 4,][l,"recall_rownum"]),]
        
        for (m in 1:nrow(temp4[temp4[, "orig_rownum"] == 5,])){
          
          current_contrast5 <- current_contrast4
          
          current_contrast5[1,9] <- temp4[temp4[,"orig_rownum"] == 5,][m,"orig_rownum"]
          current_contrast5[1,10] <- temp4[temp4[,"orig_rownum"] == 5,][m,"recall_rownum"]
          current_contrast5[1,21] <- current_contrast4[1,21] + temp4[temp4[,"orig_rownum"] == 5,][m,"sum_contrast"]
          temp5 <- temp4[!(temp4[,"orig_rownum"] == 5) & !(temp4[,"recall_rownum"] == temp4[temp4[,"orig_rownum"] == 5,][m,"recall_rownum"]),]
          
          for (n in 1:nrow(temp5[temp5[, "orig_rownum"] == 6,])){
            
            current_contrast6 <- current_contrast5
            
            current_contrast6[1,11] <- temp5[temp5[,"orig_rownum"] == 6,][n,"orig_rownum"]
            current_contrast6[1,12] <- temp5[temp5[,"orig_rownum"] == 6,][n,"recall_rownum"]
            current_contrast6[1,21] <- current_contrast5[1,21] + temp5[temp5[,"orig_rownum"] == 6,][n,"sum_contrast"]
            temp6 <- temp5[!(temp5[,"orig_rownum"] == 6) & !(temp5[,"recall_rownum"] == temp5[temp5[,"orig_rownum"] == 6,][n,"recall_rownum"]),]
            
            for (o in 1:nrow(temp6[temp6[, "orig_rownum"] == 7,])){
              
              current_contrast7 <- current_contrast6
              
              current_contrast7[1,13] <- temp6[temp6[,"orig_rownum"] == 7,][o,"orig_rownum"]
              current_contrast7[1,14] <- temp6[temp6[,"orig_rownum"] == 7,][o,"recall_rownum"]
              current_contrast7[1,21] <- current_contrast6[1,21] + temp6[temp6[,"orig_rownum"] == 7,][o,"sum_contrast"]
              temp7 <- temp6[!(temp6[,"orig_rownum"] == 7) & !(temp6[,"recall_rownum"] == temp6[temp6[,"orig_rownum"] == 7,][o,"recall_rownum"]),]
              
              for (p in 1:nrow(temp7[temp7[, "orig_rownum"] == 8,])){
                
                current_contrast8 <- current_contrast7
                
                current_contrast8[1,15] <- temp7[temp7[,"orig_rownum"] == 8,][p,"orig_rownum"]
                current_contrast8[1,16] <- temp7[temp7[,"orig_rownum"] == 8,][p,"recall_rownum"]
                current_contrast8[1,21] <- current_contrast7[1,21] + temp7[temp7[,"orig_rownum"] == 8,][p,"sum_contrast"]
                temp8 <- temp7[!(temp7[,"orig_rownum"] == 8) & !(temp7[,"recall_rownum"] == temp7[temp7[,"orig_rownum"] == 8,][p,"recall_rownum"]),]
                
                for (q in 1:nrow(temp8[temp8[, "orig_rownum"] == 9,])){
                  
                  current_contrast9 <- current_contrast8
                  
                  current_contrast9[1,17] <- temp8[temp8[,"orig_rownum"] == 9,][q,"orig_rownum"]
                  current_contrast9[1,18] <- temp8[temp8[,"orig_rownum"] == 9,][q,"recall_rownum"]
                  current_contrast9[1,21] <- current_contrast8[1,21] + temp8[temp8[,"orig_rownum"] == 9,][q,"sum_contrast"]
                  temp9 <- temp8[!(temp8[,"orig_rownum"] == 9) & !(temp8[,"recall_rownum"] == temp8[temp8[,"orig_rownum"] == 9,][q,"recall_rownum"]),]
                  
                  temp9 <- t(as.matrix(temp9))
                  current_contrast9[1,19] <- temp9[1,"orig_rownum"]
                  current_contrast9[1,20] <- temp9[1,"recall_rownum"]
                  current_contrast9[1,21] <- current_contrast9[1,21] + temp9[1,"sum_contrast"]
                  
                  overall_contrast_possibilities <- rbind(overall_contrast_possibilities, current_contrast9)
                  
                  r <- r + 1
                  print(paste("calculating combination ",r,sep=""))
                }
              }
            }
          }
        }
      }
    }
  }
}



rm(temp,temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9)
rm(current_contrast,current_contrast2,current_contrast3,current_contrast4,current_contrast5,
   current_contrast6,current_contrast7,current_contrast8,current_contrast9)
