difference_indices <- read.csv("DiffIndices.csv")
difference_indices$X <- NULL

# new comparison algorithm - difference to closest original (NCA_DCO)
colnames(final_validation_data) <- c("pict","prototype_nr","recall_nr","NCA_DCO")
final_validation_data <- as.data.frame(final_validation_data)
difference_indices <- difference_indices %>% left_join(final_validation_data, by="pict")

# difference_indices$prototype_nr <- NULL
difference_indices$recall_nr <- NULL

write.csv(difference_indices,"difference_indices_202405.csv",row.names = FALSE)
