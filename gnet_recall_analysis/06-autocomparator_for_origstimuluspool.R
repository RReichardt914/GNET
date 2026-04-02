

# get the location of the prototypes
prototype_pos <- c(match("prototype1",gnet_stimdata_matricesarray_stimulusID),
                   match("prototype2",gnet_stimdata_matricesarray_stimulusID),
                   match("prototype3",gnet_stimdata_matricesarray_stimulusID),
                   match("prototype4",gnet_stimdata_matricesarray_stimulusID))

newstim_pos <- match(gnet_stimdata_matricesarray_stimulusID[str_detect(gnet_stimdata_matricesarray_stimulusID, "p[1-4:6-9]s..")],gnet_stimdata_matricesarray_stimulusID)

# összehasonlító script

for (s in 1:length(newstim_pos)){
  recallidentity_pairings <- matrix(nrow = 4, ncol = 4)
  colnames(recallidentity_pairings) <- c("participantID","prototype_nr","recall_nr","overall_contrast")
  # recalled_pos <- c(which(gnet_recalldata_matricesarray_stimulusID %in% unique(gnet_recalldata_matricesarray_stimulusID)[s]))
  recalled_stimulus <- gnet_stimdata_matricesarray[newstim_pos[s],,]
  recalled_stim_pairs <- gnet_stimdata_matricesarray_pairs[newstim_pos[s],,]
  for (p in 1:length(prototype_pos)){
    original_stimulus <- gnet_stimdata_matricesarray[prototype_pos[p],,]
    original_stim_pairs <- gnet_stimdata_matricesarray_pairs[prototype_pos[p],,]
    source('05-comparator_preselection.R')
    recallidentity_pairings[p,"participantID"] <- unique(gnet_stimdata_matricesarray_stimulusID[newstim_pos[s]])
    recallidentity_pairings[p,"prototype_nr"] <- p
    recallidentity_pairings[p,"recall_nr"] <- s
    recallidentity_pairings[p,"overall_contrast"] <- overall_contrast_possibilities[1,21]
  }
  
  recallidentity_pairings <- 
    recallidentity_pairings[order(as.numeric(recallidentity_pairings[,4]),decreasing=FALSE),]
  
  if (!exists("final_validation_data")){
    final_validation_data <- recallidentity_pairings[1,]
  } else {
    final_validation_data <- rbind(final_validation_data, recallidentity_pairings[1,])
  }
  rm(recallidentity_pairings)
}
  



# elmentendő adatok


