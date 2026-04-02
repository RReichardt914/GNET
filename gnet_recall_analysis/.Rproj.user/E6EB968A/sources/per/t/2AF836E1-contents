

# get the location of the prototypes
prototype_pos <- c(match("prototype1",gnet_stimdata_matricesarray_stimulusID),
                   match("prototype2",gnet_stimdata_matricesarray_stimulusID),
                   match("prototype3",gnet_stimdata_matricesarray_stimulusID),
                   match("prototype4",gnet_stimdata_matricesarray_stimulusID))

# összehasonlító script

for (s in 1:length(unique(gnet_random_stimdata_matricesarray_stimulusID))){
  print(paste("NOW PROCESSING PARTICIPANT ",s,sep=""))
  recallidentity_pairings <- matrix(nrow = 16, ncol = 4)
  colnames(recallidentity_pairings) <- c("participantID","prototype_nr","recall_nr","overall_contrast")
  recalled_pos <- c(which(gnet_random_stimdata_matricesarray_stimulusID %in% unique(gnet_random_stimdata_matricesarray_stimulusID)[s]))
  for (p in 1:length(prototype_pos)){
    original_stimulus <- gnet_stimdata_matricesarray[prototype_pos[p],,]
    original_stim_pairs <- gnet_stimdata_matricesarray_pairs[prototype_pos[p],,]
    for (q in 1:length(recalled_pos)){
      recalled_stimulus <- gnet_random_stimdata_matricesarray[recalled_pos[q],,]
      recalled_stim_pairs <- gnet_random_stimdata_matricesarray_pairs[recalled_pos[q],,]
      source('05-comparator_preselection.R')
      recallidentity_pairings[(p-1)*4+q,"participantID"] <- unique(gnet_random_stimdata_matricesarray_stimulusID)[s]
      recallidentity_pairings[(p-1)*4+q,"prototype_nr"] <- p
      recallidentity_pairings[(p-1)*4+q,"recall_nr"] <- q
      recallidentity_pairings[(p-1)*4+q,"overall_contrast"] <- overall_contrast_possibilities[1,21]
    }
  }
  
  w <- 0
  overall_recallidentity_possibilities <- matrix(nrow = 0, ncol = 9)
  
  for (t in 1:nrow(recallidentity_pairings[recallidentity_pairings[, "prototype_nr"] == 1,])){
    
    current_pairing <- matrix(nrow = 1, ncol = 9)
    
    current_pairing[1,1] <- recallidentity_pairings[recallidentity_pairings[,"prototype_nr"] == 1,][t,"prototype_nr"]
    current_pairing[1,2] <- recallidentity_pairings[recallidentity_pairings[,"prototype_nr"] == 1,][t,"recall_nr"]
    if (!is.na(recallidentity_pairings[recallidentity_pairings[,"prototype_nr"] == 1,][t,"overall_contrast"])){
      current_pairing[1,9] <- recallidentity_pairings[recallidentity_pairings[,"prototype_nr"] == 1,][t,"overall_contrast"]
    } else {
      current_pairing[1,9] <- 0
    }
    
    temp <- recallidentity_pairings[!(recallidentity_pairings[,"prototype_nr"] == 1) & !(recallidentity_pairings[,"recall_nr"] == t),]
    
    for (u in 1:nrow(temp[temp[, "prototype_nr"] == 2,])){
      
      current_pairing2 <- current_pairing
      
      current_pairing2[1,3] <- temp[temp[,"prototype_nr"] == 2,][u,"prototype_nr"]
      current_pairing2[1,4] <- temp[temp[,"prototype_nr"] == 2,][u,"recall_nr"]
      if (!is.na(as.numeric(temp[temp[,"prototype_nr"] == 2,][u,"overall_contrast"]))){
        current_pairing2[1,9] <- as.numeric(current_pairing[1,9]) + as.numeric(temp[temp[,"prototype_nr"] == 2,][u,"overall_contrast"])
      }
      
      temp2 <- temp[!(temp[,"prototype_nr"] == 2) & !(temp[,"recall_nr"] == temp[temp[,"prototype_nr"] == 2,][u,"recall_nr"]),]
      
      for (v in 1:nrow(temp2[temp2[, "prototype_nr"] == 3,])){
        
        current_pairing3 <- current_pairing2
        
        current_pairing3[1,5] <- temp2[temp2[,"prototype_nr"] == 3,][v,"prototype_nr"]
        current_pairing3[1,6] <- temp2[temp2[,"prototype_nr"] == 3,][v,"recall_nr"]
        if (!is.na(as.numeric(temp2[temp2[,"prototype_nr"] == 3,][v,"overall_contrast"]))){
          current_pairing3[1,9] <- as.numeric(current_pairing2[1,9]) + as.numeric(temp2[temp2[,"prototype_nr"] == 3,][v,"overall_contrast"])
        }
        temp3 <- temp2[!(temp2[,"prototype_nr"] == 3) & !(temp2[,"recall_nr"] == temp2[temp2[,"prototype_nr"] == 3,][v,"recall_nr"]),]
        
        if (length(recalled_pos) == 4){
          temp3 <- t(as.matrix(temp3))
        }
        current_pairing3[1,7] <- temp3[1,"prototype_nr"]
        current_pairing3[1,8] <- temp3[1,"recall_nr"]
        if (!is.na(as.numeric(temp3[1,"overall_contrast"]))){
          current_pairing3[1,9] <- as.numeric(current_pairing3[1,9]) + as.numeric(temp3[1,"overall_contrast"])
        }
        
        overall_recallidentity_possibilities <- rbind(overall_recallidentity_possibilities, current_pairing3)
        
        w <- w + 1
        print(paste("Generating pairing ",w,sep=""))
      }
    }
  }
  
  overall_recallidentity_possibilities <- as.tibble(overall_recallidentity_possibilities)
  
  threshold <- (4 - length(recalled_pos)) * 2
  
  overall_recallidentity_possibilities <- overall_recallidentity_possibilities %>%
    filter(rowSums(is.na(.)) <= threshold)
  
  overall_recallidentity_possibilities <- overall_recallidentity_possibilities %>%
    mutate(V9 = as.numeric(V9))
  overall_recallidentity_possibilities <- overall_recallidentity_possibilities %>%
    arrange(V9)
  
  if (!exists("final_recollection_data")){
    final_recollection_data <- recallidentity_pairings[recallidentity_pairings[,"prototype_nr"] == as.numeric(overall_recallidentity_possibilities[1,1]) & recallidentity_pairings[,"recall_nr"] == as.numeric(overall_recallidentity_possibilities[1,2])|
                                                         recallidentity_pairings[,"prototype_nr"] == as.numeric(overall_recallidentity_possibilities[1,3]) & recallidentity_pairings[,"recall_nr"] == as.numeric(overall_recallidentity_possibilities[1,4])|
                                                         recallidentity_pairings[,"prototype_nr"] == as.numeric(overall_recallidentity_possibilities[1,5]) & recallidentity_pairings[,"recall_nr"] == as.numeric(overall_recallidentity_possibilities[1,6])|
                                                         recallidentity_pairings[,"prototype_nr"] == as.numeric(overall_recallidentity_possibilities[1,7]) & recallidentity_pairings[,"recall_nr"] == as.numeric(overall_recallidentity_possibilities[1,8]),]
  } else {
    final_recollection_data <- rbind(final_recollection_data, recallidentity_pairings[recallidentity_pairings[,"prototype_nr"] == as.numeric(overall_recallidentity_possibilities[1,1]) & recallidentity_pairings[,"recall_nr"] == as.numeric(overall_recallidentity_possibilities[1,2])|
                                                                                        recallidentity_pairings[,"prototype_nr"] == as.numeric(overall_recallidentity_possibilities[1,3]) & recallidentity_pairings[,"recall_nr"] == as.numeric(overall_recallidentity_possibilities[1,4])|
                                                                                        recallidentity_pairings[,"prototype_nr"] == as.numeric(overall_recallidentity_possibilities[1,5]) & recallidentity_pairings[,"recall_nr"] == as.numeric(overall_recallidentity_possibilities[1,6])|
                                                                                        recallidentity_pairings[,"prototype_nr"] == as.numeric(overall_recallidentity_possibilities[1,7]) & recallidentity_pairings[,"recall_nr"] == as.numeric(overall_recallidentity_possibilities[1,8]),])
  }
  rm(temp,temp2,temp3)
  rm(current_pairing,current_pairing2,current_pairing3)
  rm(recallidentity_pairings)
}

# elmentendő adatok
clean_vector <- na.omit(final_recollection_data)
write.csv(clean_vector, "gnet4s_random_final_recollection_data.csv")
