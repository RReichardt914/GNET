temp <- list.files("artnarr_data/")

stprob <- read.csv("study_probs_GNET.csv")
stprob <- as.data.table(stprob)
stprob[,oldresp_prob := oldr/(oldr+newr+miss)]
stprob[,newresp_prob := newr/(oldr+newr+miss)]
stprob[,miss_prob := miss/(oldr+newr+miss)]
tsprob <- read.csv("response_probabilities_origGNET.csv")
tsprob <- as.data.table(tsprob)

pool <- 1:1000

for (i in 1:length(temp)){
  current_data <- read.csv(paste("artnarr_data/",temp[i],sep=""))
  current_data <- as.data.table(current_data)
  current_data$ID <- strsplit(temp[i], split = ".csv")
  ###
  current_data$type <- ""
  current_data[str_detect(stim,"prototype"),type:="prototype"]
  current_data[str_detect(stim,"sarr[1-4]"),type:="sarr"]
  current_data[str_detect(stim,"sarrsha[1-4]"),type:="sarrsha"]
  current_data[str_detect(stim,"sarrcol[1-4]"),type:="sarrcol"]
  current_data[str_detect(stim,"sarrshacol[1-4]"),type:="sarrshacol"]
  current_data[str_detect(stim,"p[6-9]"),type:="diffarr"]
  current_data[str_detect(stim,"p10"),type:="diffarr"]
  ### trial quality
  current_data$tq <- ""
  current_data[is.na(count_test_sequence),tq:="study"]
  current_data[!is.na(count_test_sequence),tq:="test"]
  current_data[str_detect(stim,"[1-2].png") & tq == "test",tq:="old"]
  current_data[str_detect(stim,"[3-4].png") & tq == "test",tq:="new"]
  ###
  current_data$study_response <- ""
  current_data$test_response <- ""
  for (j in 1:nrow(current_data)){
    stimulus <- current_data$stim[j]
    phase <- current_data$tq[j]
    choice <- sample(pool,1)
    choice <- choice/1000
    if (phase == "study"){
      OLDR <- stprob[stim == stimulus,oldresp_prob]
      NEWR <- stprob[stim == stimulus,newresp_prob]
      MISS <- stprob[stim == stimulus,miss_prob]
      
      if (choice < OLDR){
        current_data$study_response[j] <- "left"
      } else if (choice < OLDR + NEWR){
        current_data$study_response[j] <- "right"
      } else {
        current_data$study_response[j] <- "None"
      }
    } else {
      OLDR <- tsprob[stim == stimulus,oldresp_prob]
      NEWR <- tsprob[stim == stimulus,newresp_prob]
      MISS <- tsprob[stim == stimulus,mayberesp_prob]
      if (choice < OLDR){
        current_data$test_response[j] <- "left"
      } else if (choice < OLDR + NEWR){
        current_data$test_response[j] <- "right"
      } else {
        current_data$test_response[j] <- "down"
      }
    }
  }
  if (!exists("GNET_artnarrative_data")){
    GNET_artnarrative_data <- current_data
  } else {
    if (ncol(current_data) > 122) {
      current_data$queryParams_fbclid <- NULL
    }
    GNET_artnarrative_data <- rbind(GNET_artnarrative_data,current_data)
  }
}
