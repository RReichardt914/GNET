library(data.table)
library(stringr)

# get data from csv
GNET_artn_alldata <- as.data.table(read.csv("gnet-artnarr-elte202301_-_expdata.csv"))
GNET_artn_alldata <-
  GNET_artn_alldata[,c("count_study_sequence","count_test_sequence","datetime",
                             "response","response_study_response","response_test_response",
                             "response_time","response_time_study_response","response_time_test_response",
                             "stim","multichar_knowresp","sessionid")]
GNET_artn_alldata$multichar_knowresp <- as.character(GNET_artn_alldata$multichar_knowresp)

# trial type - generating categorical variable

GNET_artn_alldata$type <- ""
GNET_artn_alldata[str_detect(stim,"prototype"),type:="prototype"]
GNET_artn_alldata[str_detect(stim,"sarr[1-4]"),type:="sarr"]
GNET_artn_alldata[str_detect(stim,"sarrsha[1-4]"),type:="sarrsha"]
GNET_artn_alldata[str_detect(stim,"sarrcol[1-4]"),type:="sarrcol"]
GNET_artn_alldata[str_detect(stim,"sarrshacol[1-4]"),type:="sarrshacol"]
GNET_artn_alldata[str_detect(stim,"p[6-9]"),type:="diffarr"]
GNET_artn_alldata[str_detect(stim,"p10"),type:="diffarr"]

# trial quality - generating categ var

GNET_artn_alldata$tq <- ""
GNET_artn_alldata[is.na(count_test_sequence),tq:="study"]
GNET_artn_alldata[!is.na(count_test_sequence),tq:="test"]
GNET_artn_alldata[str_detect(stim,"[1-2].png") & tq == "test",tq:="old"]
GNET_artn_alldata[str_detect(stim,"[3-4].png") & tq == "test",tq:="new"]

# calculated indices

GNET_artn_alldata$DTF <- 0
GNET_artn_alldata$ColDTF <- 0
GNET_artn_alldata$ShaDTF <- 0
GNET_artn_alldata$ArrDTF <- 0

GNET_artn_alldata$DTAF <- 0
GNET_artn_alldata$ColDTAF <- 0
GNET_artn_alldata$ShaDTAF <- 0
GNET_artn_alldata$ArrDTAF <- 0

GNET_artn_alldata$DTAN <- 0
GNET_artn_alldata$ColDTAN <- 0
GNET_artn_alldata$ShaDTAN <- 0
GNET_artn_alldata$ArrDTAN <- 0

GNET_artn_alldata$DTN_byPhase <- 0
GNET_artn_alldata$ColDTN_bP <- 0
GNET_artn_alldata$ShaDTN_bP <- 0
GNET_artn_alldata$ArrDTN_bP <- 0

for (i in 1:nrow(GNET_artn_alldata)){
  for (j in 1:nrow(difference_indices)){
    if (grepl(difference_indices$pict[j],GNET_artn_alldata$stim[i])){
      GNET_artn_alldata$DTF[i] <- difference_indices$DTF[j]
      GNET_artn_alldata$ColDTF[i] <- difference_indices$ColDTF[j]
      GNET_artn_alldata$ShaDTF[i] <- difference_indices$ShaDTF[j]
      GNET_artn_alldata$ArrDTF[i] <- difference_indices$ArrDTF[j]
      
      GNET_artn_alldata$DTAF[i] <- difference_indices$DTAF[j]
      GNET_artn_alldata$ColDTAF[i] <- difference_indices$ColDTAF[j]
      GNET_artn_alldata$ShaDTAF[i] <- difference_indices$ShaDTAF[j]
      GNET_artn_alldata$ArrDTAF[i] <- difference_indices$ArrDTAF[j]
      
      GNET_artn_alldata$DTAN[i] <- difference_indices$DTAN[j]
      GNET_artn_alldata$ColDTAN[i] <- difference_indices$ColDTAN[j]
      GNET_artn_alldata$ShaDTAN[i] <- difference_indices$ShaDTAN[j]
      GNET_artn_alldata$ArrDTAN[i] <- difference_indices$ArrDTAN[j]
      
      GNET_artn_alldata$DTN_byPhase[i] <- difference_indices$DTN_byPhase[j]
      GNET_artn_alldata$ColDTN_bP[i] <- difference_indices$ColDTN_bP[j]
      GNET_artn_alldata$ShaDTN_bP[i] <- difference_indices$ShaDTN_bP[j]
      GNET_artn_alldata$ArrDTN_bP[i] <- difference_indices$ArrDTN_bP[j]
      
      GNET_artn_alldata$DTN[i] <- difference_indices$DTN[j]
    }
  }
}

rm(i,j)
