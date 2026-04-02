

for (i in 1:4){
  print(paste("Prototype Nr",i,sep=" "))
  print(mean(as.numeric(final_recollection_data_orig[final_recollection_data_orig[, "prototype_nr"] == i,"overall_contrast"])))
  print(sd(as.numeric(final_recollection_data_orig[final_recollection_data_orig[, "prototype_nr"] == i,"overall_contrast"])))
  print(mean(as.numeric(final_recollection_data_delay[final_recollection_data_delay[, "prototype_nr"] == i,"overall_contrast"])))
  print(sd(as.numeric(final_recollection_data_delay[final_recollection_data_delay[, "prototype_nr"] == i,"overall_contrast"])))
}
