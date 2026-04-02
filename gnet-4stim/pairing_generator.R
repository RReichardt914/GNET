
reprpict <- list.files("recalled_imgs/gnet4s_ORIG/")
reprpict <- as.data.table(reprpict)
reprpict$fampict <- "prototype1.png"
temp <- reprpict
  
reprpict <- list.files("recalled_imgs/gnet4s_ORIG/")
reprpict <- as.data.table(reprpict)
reprpict$fampict <- "prototype2.png"
temp <- rbind(temp,reprpict)

reprpict <- list.files("recalled_imgs/gnet4s_ORIG/")
reprpict <- as.data.table(reprpict)
reprpict$fampict <- "prototype3.png"
temp <- rbind(temp,reprpict)

reprpict <- list.files("recalled_imgs/gnet4s_ORIG/")
reprpict <- as.data.table(reprpict)
reprpict$fampict <- "prototype4.png"
pairings <- rbind(temp,reprpict)

write.csv(pairings,"pairings_orig.csv",row.names=FALSE)
rm(pairings,reprpict,temp)


