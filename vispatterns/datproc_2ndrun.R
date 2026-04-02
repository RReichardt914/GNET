library(data.table)
library(stringr)

# get 2nd run data from csv
GNET_pilot_RUN2_alldata <- as.data.table(read.csv("gnet_pilot_2ndrun.csv"))
GNET_pilot_RUN2_alldata <-
  GNET_pilot_RUN2_alldata[,c("batchId","batchTitle",
                           "count_study_sequence","count_test_sequence","datetime",
                           "response","response_study_response","response_test_response",
                           "response_time","response_time_study_response","response_time_test_response",
                           "stim","studyResultId","workerId")]

GNET_pilot_RUN2_alldata$workerId <- as.character(GNET_pilot_RUN2_alldata$workerId)
# trial type category
GNET_pilot_RUN2_alldata$type <- ""
GNET_pilot_RUN2_alldata[str_detect(stim,"prototype"),type:="prototype"]
GNET_pilot_RUN2_alldata[str_detect(stim,"sarr[1-4]"),type:="sarr"]
GNET_pilot_RUN2_alldata[str_detect(stim,"sarrsha[1-4]"),type:="sarrsha"]
GNET_pilot_RUN2_alldata[str_detect(stim,"sarrcol[1-4]"),type:="sarrcol"]
GNET_pilot_RUN2_alldata[str_detect(stim,"sarrshacol[1-4]"),type:="sarrshacol"]
GNET_pilot_RUN2_alldata[str_detect(stim,"p[6-9]"),type:="diffarr"]
GNET_pilot_RUN2_alldata[str_detect(stim,"p10"),type:="diffarr"]
# trial quality
GNET_pilot_RUN2_alldata$tq <- ""
GNET_pilot_RUN2_alldata[is.na(count_test_sequence),tq:="study"]
GNET_pilot_RUN2_alldata[!is.na(count_test_sequence),tq:="test"]
GNET_pilot_RUN2_alldata[str_detect(stim,"[1-2].png") & tq == "test",tq:="old"]
GNET_pilot_RUN2_alldata[str_detect(stim,"[3-4].png") & tq == "test",tq:="new"]
# calculated indices
GNET_pilot_RUN2_alldata$DTF <- 0
GNET_pilot_RUN2_alldata$DTAF <- 0
GNET_pilot_RUN2_alldata$DTAN <- 0
GNET_pilot_RUN2_alldata$DTN_byPhase <- 0
for (i in 1:nrow(GNET_pilot_RUN2_alldata)){
  for (j in 1:nrow(difference_indices)){
    if (grepl(difference_indices$pict[j],GNET_pilot_RUN2_alldata$stim[i])){
      GNET_pilot_RUN2_alldata$DTF[i] <- difference_indices$DTF[j]
      GNET_pilot_RUN2_alldata$DTAF[i] <- difference_indices$DTAF[j]
      GNET_pilot_RUN2_alldata$DTAN[i] <- difference_indices$DTAN[j]
      GNET_pilot_RUN2_alldata$DTN_byPhase[i] <- difference_indices$DTN_byPhase[j]
      GNET_pilot_RUN2_alldata$DTN[i] <- difference_indices$DTN[j]
    }
  }
}

rm(i,j)