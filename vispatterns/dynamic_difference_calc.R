# we will need the vectors for the pictures
# we will need the presentation orders for each participant

ggplot(p2[workerId == "1761" & total_responses <76],aes(total_responses, DTAF)) + geom_point()

# clustering ####
cluster <- difference_indices
cluster$phase <- NULL
cluster$type <- NULL

cluster$pict <- as.factor(cluster$pict)
cluster$DTF <- as.integer(cluster$DTF)
cluster$DTAF <- as.integer(cluster$DTAF)
cluster$DTAN <- as.integer(cluster$DTAN)
cluster$DTN_byPhase <- as.integer(cluster$DTN_byPhase)

kmr <- kmeans(cluster[,2:5], 4)
#####

allids <- unique(p3$workerId)
p3[,stimpresseq := rep(1:175,length(allids))]
p3$DTPPm <- 0
p3$DTPPsd <- 0
for (l in 1:length(allids)){
  sequence <- p3[workerId == allids[l], c("stim","total_responses","workerId")]
  difftoprevpres <- data.table(pict = character(),
                               seqnr = numeric(),
                               dmean = numeric(),
                               dsd = numeric(),
                               dsum  = numeric())
  # 25 comparisons to prototypes
  prot_arr <- picdat[pict == "prototype1",]
  prot_col <- picdat[pict == "prototype1col",]
  prot_sha <- picdat[pict == "prototype1sha",]
  for (i in 1:24){
    prot_arr <- rbind(prot_arr, picdat[pict == paste("prototype",i %% 5 + 1,sep = ""),])
    prot_col <- rbind(prot_col, picdat[pict == paste("prototype",i %% 5 + 1,"col",sep = ""),])
    prot_sha <- rbind(prot_sha, picdat[pict == paste("prototype",i %% 5 + 1,"sha",sep = ""),])
  }
  # compare each picture to the pictures seen before it
  for (j in 1:nrow(sequence)){
    comp_arr <- picdat[pict == str_sub(sequence$stim[j],1,-5),]
    comp_col <- picdat[pict == paste(str_sub(sequence$stim[j],1,-5),"col",sep=""),]
    comp_sha <- picdat[pict == paste(str_sub(sequence$stim[j],1,-5),"sha",sep=""),]
    diffs <- c()
    if (j > 1){
      prot_arr <- rbind(prot_arr, picdat[pict == str_sub(sequence$stim[j-1],1,-5),])
      prot_col <- rbind(prot_col, picdat[pict == paste(str_sub(sequence$stim[j-1],1,-5),"col",sep=""),])
      prot_sha <- rbind(prot_sha, picdat[pict == paste(str_sub(sequence$stim[j-1],1,-5),"sha",sep=""),])
    }
    for (k in 1:(j+25)){
      # calculate diff in arrangement
      arrdiff <- prot_arr[k,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)] - comp_arr[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)]
      ad <- table(arrdiff != 0)["TRUE"][[1]]
      if (is.na(ad)){ad <- 0}
      # calc diff in color
      coldiff <- prot_col[k,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)] - comp_col[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)]
      cd <- table(coldiff != 0)["TRUE"][[1]]
      if (is.na(cd)){cd <- 0}
      # calc diff in shape
      shadiff <- prot_sha[k,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)] - comp_sha[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)]
      sd <- table(shadiff != 0)["TRUE"][[1]]
      if (is.na(sd)){sd <- 0}
      # summarize differences
      dsum <- ad + cd + sd
      # fill a vector containing the differences to each previously seen picture
      diffs[k] <- dsum
    }
    difftoprevpres <- rbind(difftoprevpres, list(sequence$stim[j],j,mean(diffs),sd(diffs),sum(diffs)))
  }
  for (m in 1:nrow(difftoprevpres)){
    p3[workerId == allids[l] & stim == difftoprevpres$pict[m] & stimpresseq == difftoprevpres$seqnr[m],
       DTPPm := difftoprevpres$dmean[m]]
    p3[workerId == allids[l] & stim == difftoprevpres$pict[m] & stimpresseq == difftoprevpres$seqnr[m],
       DTPPsd := difftoprevpres$dsd[m]]
  }
}

# we save this data as csv, since the calculation took almost 2 hours!
# maybe optimize the algorithm later

write.csv(p3,"filtereddata_pilot1_alldiffind.csv")

# Comp to all previously presented pictures ####
# we calculate the difference of a picture from all prototypes and take the mean
# this dt will hold the calculated values

#####