
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

# preselection process ####

sorted_comparison_table <- comparison_table[order(comparison_table[, "sum_contrast"]), ]

fixed_pairings <- sorted_comparison_table[1:3,]
remaining_pairings <- sorted_comparison_table[4:100,]

while (length(unique(fixed_pairings[,"orig_rownum"])) < 3 | length(unique(fixed_pairings[,"recall_rownum"])) < 3){
  if (length(unique(fixed_pairings[,"orig_rownum"])) < 3){
    fixed_pairings <- fixed_pairings[!duplicated(fixed_pairings[,"orig_rownum"]),]
    rowstoreplace <- 3-nrow(fixed_pairings)
    fixed_pairings <- rbind(fixed_pairings,remaining_pairings[1:rowstoreplace,])
    remaining_pairings <- remaining_pairings[-(1:rowstoreplace),]
  }
  if (length(unique(fixed_pairings[,"recall_rownum"])) < 3){
    fixed_pairings <- fixed_pairings[!duplicated(fixed_pairings[,"recall_rownum"]),]
    rowstoreplace <- 3-nrow(fixed_pairings)
    fixed_pairings <- rbind(fixed_pairings,remaining_pairings[1:rowstoreplace,])
    remaining_pairings <- remaining_pairings[-(1:rowstoreplace),]
  }
}

rm(sorted_comparison_table,remaining_pairings,rowstoreplace)

############

overall_contrast_possibilities <- matrix(nrow = 0, ncol = 21)

# variable to visually follow processing
r <- 0

reduced_comparison_table <- comparison_table[!(comparison_table[,"orig_rownum"] %in% fixed_pairings[,"orig_rownum"]) &
                                               !(comparison_table[,"recall_rownum"] %in% fixed_pairings[,"recall_rownum"]),]

for (i in 1:nrow(reduced_comparison_table[reduced_comparison_table[, "orig_rownum"] == reduced_comparison_table[1,"orig_rownum"],])){
  
  current_contrast <- matrix(nrow = 1, ncol = 21)
  
  current_contrast[1,1] <- reduced_comparison_table[reduced_comparison_table[, "orig_rownum"] == reduced_comparison_table[1,"orig_rownum"],][i,"orig_rownum"]
  current_contrast[1,2] <- reduced_comparison_table[reduced_comparison_table[, "orig_rownum"] == reduced_comparison_table[1,"orig_rownum"],][i,"recall_rownum"]
  current_contrast[1,21] <- reduced_comparison_table[reduced_comparison_table[, "orig_rownum"] == reduced_comparison_table[1,"orig_rownum"],][i,"sum_contrast"]
  temp <- reduced_comparison_table[!(reduced_comparison_table[,"orig_rownum"] == reduced_comparison_table[1,"orig_rownum"]) & !(reduced_comparison_table[,"recall_rownum"] == reduced_comparison_table[i,"recall_rownum"]),]
  
  for (j in 1:nrow(temp[temp[, "orig_rownum"] == temp[1,"orig_rownum"],])){
    
    current_contrast2 <- current_contrast
    
    current_contrast2[1,3] <- temp[temp[,"orig_rownum"] == temp[1,"orig_rownum"],][j,"orig_rownum"]
    current_contrast2[1,4] <- temp[temp[,"orig_rownum"] == temp[1,"orig_rownum"],][j,"recall_rownum"]
    current_contrast2[1,21] <- current_contrast[1,21] + temp[temp[,"orig_rownum"] == temp[1,"orig_rownum"],][j,"sum_contrast"]
    temp2 <- temp[!(temp[,"orig_rownum"] == temp[1,"orig_rownum"]) & !(temp[,"recall_rownum"] == temp[temp[,"orig_rownum"] == temp[1,"orig_rownum"],][j,"recall_rownum"]),]
    
    for (k in 1:nrow(temp2[temp2[, "orig_rownum"] == temp2[1,"orig_rownum"],])){
      
      current_contrast3 <- current_contrast2
      
      current_contrast3[1,5] <- temp2[temp2[,"orig_rownum"] == temp2[1,"orig_rownum"],][k,"orig_rownum"]
      current_contrast3[1,6] <- temp2[temp2[,"orig_rownum"] == temp2[1,"orig_rownum"],][k,"recall_rownum"]
      current_contrast3[1,21] <- current_contrast2[1,21] + temp2[temp2[,"orig_rownum"] == temp2[1,"orig_rownum"],][k,"sum_contrast"]
      temp3 <- temp2[!(temp2[,"orig_rownum"] == temp2[1,"orig_rownum"]) & !(temp2[,"recall_rownum"] == temp2[temp2[,"orig_rownum"] == temp2[1,"orig_rownum"],][k,"recall_rownum"]),]
      
      for (l in 1:nrow(temp3[temp3[, "orig_rownum"] == temp3[1,"orig_rownum"],])){
        
        current_contrast4 <- current_contrast3
        
        current_contrast4[1,7] <- temp3[temp3[,"orig_rownum"] == temp3[1,"orig_rownum"],][l,"orig_rownum"]
        current_contrast4[1,8] <- temp3[temp3[,"orig_rownum"] == temp3[1,"orig_rownum"],][l,"recall_rownum"]
        current_contrast4[1,21] <- current_contrast3[1,21] + temp3[temp3[,"orig_rownum"] == temp3[1,"orig_rownum"],][l,"sum_contrast"]
        temp4 <- temp3[!(temp3[,"orig_rownum"] == temp3[1,"orig_rownum"]) & !(temp3[,"recall_rownum"] == temp3[temp3[,"orig_rownum"] == temp3[1,"orig_rownum"],][l,"recall_rownum"]),]
        
        for (m in 1:nrow(temp4[temp4[, "orig_rownum"] == temp4[1,"orig_rownum"],])){
          
          current_contrast5 <- current_contrast4
          
          current_contrast5[1,9] <- temp4[temp4[,"orig_rownum"] == temp4[1,"orig_rownum"],][m,"orig_rownum"]
          current_contrast5[1,10] <- temp4[temp4[,"orig_rownum"] == temp4[1,"orig_rownum"],][m,"recall_rownum"]
          current_contrast5[1,21] <- current_contrast4[1,21] + temp4[temp4[,"orig_rownum"] == temp4[1,"orig_rownum"],][m,"sum_contrast"]
          temp5 <- temp4[!(temp4[,"orig_rownum"] == temp4[1,"orig_rownum"]) & !(temp4[,"recall_rownum"] == temp4[temp4[,"orig_rownum"] == temp4[1,"orig_rownum"],][m,"recall_rownum"]),]
          
          for (n in 1:nrow(temp5[temp5[, "orig_rownum"] == temp5[1,"orig_rownum"],])){
            
            current_contrast6 <- current_contrast5
            
            current_contrast6[1,11] <- temp5[temp5[,"orig_rownum"] == temp5[1,"orig_rownum"],][n,"orig_rownum"]
            current_contrast6[1,12] <- temp5[temp5[,"orig_rownum"] == temp5[1,"orig_rownum"],][n,"recall_rownum"]
            current_contrast6[1,21] <- current_contrast5[1,21] + temp5[temp5[,"orig_rownum"] == temp5[1,"orig_rownum"],][n,"sum_contrast"]
            temp6 <- temp5[!(temp5[,"orig_rownum"] == temp5[1,"orig_rownum"]) & !(temp5[,"recall_rownum"] == temp5[temp5[,"orig_rownum"] == temp5[1,"orig_rownum"],][n,"recall_rownum"]),]
            
            temp6 <- t(as.matrix(temp6))
            current_contrast6[1,13] <- temp6[1,"orig_rownum"]
            current_contrast6[1,14] <- temp6[1,"recall_rownum"]
            current_contrast6[1,21] <- current_contrast6[1,21] + temp6[1,"sum_contrast"]
            
            for (o in 1:nrow(fixed_pairings)){
              current_contrast6[1,13+o*2] <- fixed_pairings[o,"orig_rownum"]
              current_contrast6[1,14+o*2] <- fixed_pairings[o,"recall_rownum"]
              current_contrast6[1,21] <- current_contrast6[1,21] + fixed_pairings[o,"sum_contrast"]
            }
            
            overall_contrast_possibilities <- rbind(overall_contrast_possibilities, current_contrast6)
            
            # these lines print out the current combination number
            r <- r + 1
            print(paste("calculating combination ",r,sep=""))
                
          }
        }
      }
    }
  }
}



rm(temp,temp2,temp3,temp4,temp5,temp6)
rm(current_contrast,current_contrast2,current_contrast3,
   current_contrast4,current_contrast5,current_contrast6)
rm(i,j,k,l,m,n,o)
rm(fixed_pairings, reduced_comparison_table)

dataforplot <- as_tibble(overall_contrast_possibilities)
ggplot(data=dataforplot, aes(x=V21)) +
  geom_bar(stat="bin")
