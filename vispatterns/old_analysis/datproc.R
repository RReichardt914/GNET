library(data.table)

# get preprocessed data ####
# preprocessed data is in a csv
p2 <- as.data.table(read.csv("cleandata.csv"))
p2$workerId <- as.character(p2$workerId)
p2$DTF <- 0
p2$DTAF <- 0
p2$DTAN <- 0
p2$DTN_byPhase <- 0
for (i in 1:nrow(p2)){
  for (j in 1:nrow(difference_indices)){
    if (grepl(difference_indices$pict[j],p2$stim[i])){
      p2$DTF[i] <- difference_indices$DTF[j]
      p2$DTAF[i] <- difference_indices$DTAF[j]
      p2$DTAN[i] <- difference_indices$DTAN[j]
      p2$DTN_byPhase[i] <- difference_indices$DTN_byPhase[j]
    }
  }
}

# check the nr of rows for each participant
# remove everyone that does not have the correct number of recorded responses (175)
ids <- p2[, .(rowCount = .N), by = workerId]
fineids <- as.character(ids$workerId[ids$rowCount == 175])
fdrop <- nrow(ids) - length(fineids) # this var shows the number of ppl dropped
p3 <- p2[workerId %in% fineids] # removal

# check the number of missed responses
tdat <- p3[, .(rowCount = .N), by = .(workerId,type,tq,response)]
misses <- tdat[response == "None", sum(rowCount),by = workerId]

# drop anyone who missed too many responses (>mean+2*sd)
dropbymiss <- 0 # var how many dropped
limit <- mean(misses$V1)+(2*sd(misses$V1))
for (i in 1:nrow(misses)){
  if (misses$V1[i] > limit){
    fineids <- fineids[!fineids == misses$workerId[i]]
    dropbymiss <- dropbymiss + 1 
  }
}
misslimit <- limit # var the miss limit
p3 <- p2[workerId %in% fineids]
#####
# p2 holds all the data, p3 is a filtered dt - not 175 rows, many misses

# GENERATE STUDY DT ####
# correct study responses by type
study_dat = p3[is.na(count_test_stimpres), sum(st_corr), by=.(datetime,workerId,type)]
# all study trials by type
study_TrialNrByType <- p3[is.na(count_test_stimpres), .N,by=.(datetime,workerId,type)]
strecog <- merge(study_dat,study_TrialNrByType,by=c("datetime","workerId","type"))
colnames(strecog) <- c("datetime","workerId","type","CCategorization","NrTrials")
# calculate correct categorization rate
strecog[, crrate := CCategorization/NrTrials]

# we drop anyone below 0.5 prototype recognition rate in study phase
limit <- 0.5
dropbyst <- 0
protrecog <- strecog[type == "prototype",]
 for (i in 1:nrow(protrecog)){
   if (protrecog$crrate[i] < limit){
     fineids <- fineids[!fineids == protrecog$workerId[i]]
     dropbyst <- dropbyst + 1 
   }
 }
study_dat <- strecog[workerId %in% fineids]
#####
# study_dat holds summarized study vars, filtered for prototype recognition

# GENERATE TEST DT ####
test_dat <- p3[!is.na(count_test_stimpres), .(rowCount = .N), by=.(workerId,type,tq,response)]
# we need to add some zeros for the type*tq*rkey(response) combinations that are left out
colnames(test_dat)[4] <- "rkey"
mycols  = c("workerId","type","tq","rkey")
setkeyv(test_dat,mycols)
test_dat_c <- test_dat[J(do.call(CJ,lapply(mycols,function(x)unique(get(x)))))]
for (i in 1:nrow(test_dat_c)){
  if (is.na(test_dat_c$rowCount[i])){
    test_dat_c$rowCount[i] <- 0
  }
}
# reorganize the table
resps <- c("left","down","right","None")
for (i in 1:length(resps)){
  tempdt <- test_dat_c[rkey == resps[i]]
  colnames(tempdt) <- c("workerId","type","tq","rkey",resps[i])
  tempdt$rkey <- NULL
  if (i > 1){
    safetemp <- merge(safetemp, tempdt, by = c("workerId","type","tq"))
  } else {
    safetemp <- tempdt
  }
}
test_dat <- safetemp
test_dat[,AllRec := left + down + right + None]
test_dat$corr <- 0
test_dat[tq == "fam", corr := left + down]
test_dat[tq == "new", corr := right + down]
test_dat$fa <- 0
test_dat[tq == "fam", fa := right]
test_dat[tq == "new", fa := left]
test_dat[,occr := corr - fa]



test_dat_c <- test_dat_c[workerId %in% fineids]
types <- c("diffarr","sarr","sarrcol","sarrsha","sarrshacol")

# dropping maybe resps
mayberesp <- test_dat[, sum(down), by = .(workerId)]
mayberesp[,mean(V1)]
mayberesp[,sd(V1)]

dropmaybe <- c()
dropmaybes <- 0
maybelimit <- 25
for (i in 1:nrow(mayberesp)){
  if (mayberesp$V1[i] >= maybelimit){
    dropmaybe <- c(dropmaybe, mayberesp$workerId[i])
    dropmaybes <- dropmaybes + 1
  }
}
# refresh fineids
fineids <- fineids[!(fineids %in% dropmaybe)]
# drop from st
study_dat <- study_dat[workerId %in% fineids]
# drop from test
test_dat <- test_dat[workerId %in% fineids]
#####
# test_dat holds summarized data for test - exclusion based on maybe resps is made here

# generate dfs for lmms ####

# THIS IS DONE IN ANALYSIS.RMD ATM - as we use a calculated variable
# the variable had to be saved in a file as the calculation took 2 hours

# tdlmm <- p3[workerId %in% fineids]
# tdlmm <- tdlmm[tq != "x",]
# tdlmm$cr <- 0
# tdlmm[tq == "new" & response_test_response %in% c("down","right"),cr := 1]
# tdlmm[tq == "fam" & response_test_response %in% c("down","left"),cr := 1]
# 
# 
# stlmm <- p3[workerId %in% fineids]
# stlmm <- stlmm[tq == "x"]
# 
# 
# t2lmm <- merge(tdlmm, stlmm, by=c("workerId","stim"), all.x = TRUE)
# t2lmm <- t2lmm[,c("workerId","stim","datetime.x","response.x",
#                   "response_time.x","total_responses.x",
#                   "count_test_stimpres.x","response_test_response.x",
#                   "response_time_test_response.x","type.x","tq.x","cr",
#                   "st_corr.y","DTF.y","DTAF.y","DTAN.y","DTN_byPhase.y")]  
# t2lmm <- t2lmm[tq.x == "fam",]
# colnames(t2lmm) <- c("workerId","stim","datetime",
#                      "response","response_time","total_responses","count_test_stimpres",
#                      "response_test_response","response_time_test_response",
#                      "type","tq","cr","st_corr","DTF","DTAF","DTAN","DTN_byPhase" )
#####
# tdlmm - holds test data
# stlmm - holds study data
# t2lmm - holds test data with some study vars