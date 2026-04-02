library(data.table)
library(stringr)
library(tidyverse)

# get data from csv
##############################################
gnet4s_artn <- read.csv("gnet4s_artn-PREPROC.csv",sep=",") %>%
  as.data.table() %>%
  filter(!(is.na(count_study_response)))
##################################################

# work through ids
####################################
for (i in 1:nrow(gnet4s_artn)){
  gnet4s_artn[i,"multichar_knowresp"] <- str_replace(gnet4s_artn[i,"multichar_knowresp"], "caps", "")
}

problem_children <- gnet4s_artn %>%
  group_by(multichar_knowresp) %>%
  summarise(N = n()) %>%
  filter(N > 144) %>%
  pull(multichar_knowresp)

gnet4s_artn <- gnet4s_artn %>%
  filter(!(multichar_knowresp %in% problem_children))

rm(problem_children)
############################################

gnet4s_artn_recall <- gnet4s_artn[!(cell1 == "NA"&cell2 == "NA"&cell3 == "NA"&cell4 == "NA"&cell5 == "NA"),]

gnet4s_artn_recog <- gnet4s_artn[is.na(plc1c),]
gnet4s_artn_recog <- gnet4s_artn_recog[,c("count_study_response","count_test_response","date_startdate","date_starttime","multichar_knowresp","p2dur",
                                          "response_study_response","response_test_response","response_time_study_response","response_time_test_response",
                                          "rtnr","stim","type","tq")]

ResponsesSummary_artn <- gnet4s_artn_recog[, .(rowCount = .N), by = .(multichar_knowresp,type,tq,response_study_response)]

MissedResSummary_artn <- ResponsesSummary_artn[response_study_response == "None", sum(rowCount),by = .(multichar_knowresp)]
missing <- data.table(multichar_knowresp = setdiff(unique(gnet4s_artn$multichar_knowresp),MissedResSummary_artn$multichar_knowresp), V1 = c(0,0))
MissedResSummary_artn <- rbind(MissedResSummary_artn,missing)
rm(missing)

#MissedResSummary <- MissedResSummary[V1 > 66,]

#exclude <- MissedResSummary$multichar_knowresp

StudySummary_artn <- gnet4s_artn_recog[is.na(count_test_response) & response_study_response == "left" & type == "prototype", .N/20, by=.(multichar_knowresp)]

StudyFA_artn <- gnet4s_artn_recog[is.na(count_test_response) & response_study_response == "left" & type != "prototype", .N/40, by=.(multichar_knowresp)]
missing <- data.table(multichar_knowresp = setdiff(unique(gnet4s_artn$multichar_knowresp),StudyFA_artn$multichar_knowresp), V1 = c(0,0))
StudyFA_artn <- rbind(StudyFA_artn,missing)
rm(missing)

StudyCR_artn <- merge(StudySummary_artn, StudyFA_artn, by=c("multichar_knowresp"), all = TRUE)
colnames(StudyCR_artn) <- c("multichar_knowresp","R","FA")
StudyCR_artn[is.na(FA), FA:=0]
StudyCR_artn[,CR := R - FA]
rm(StudySummary_artn,StudyFA_artn)

StudyRespSum_artn <- ResponsesSummary_artn[tq == "study",]
TestRespSum_artn <- ResponsesSummary_artn[!(tq == "study"),]

mycols  = c("response_study_response","multichar_knowresp","type")
setkeyv(StudyRespSum_artn,mycols)
Study_ResponsesByType_artn <- StudyRespSum_artn[J(do.call(CJ,lapply(mycols,function(x)unique(get(x)))))]
for (i in 1:nrow(Study_ResponsesByType_artn)){
  if (is.na(Study_ResponsesByType_artn$rowCount[i])){
    Study_ResponsesByType_artn$rowCount[i] <- 0
  }
}

Study_ResponsesByType_artn$tq <- NULL
rm(StudyRespSum_artn,ResponsesSummary_artn)

Study_RespWide_artn <- Study_ResponsesByType_artn %>% 
  # select only the columns we're interested in
  select(multichar_knowresp, type, response_study_response, rowCount) %>% 
  # use pivot_wider to go from long to wide format
  pivot_wider(names_from = "response_study_response", 
              names_prefix = "resp_",
              values_from = "rowCount")
rm(Study_ResponsesByType_artn)

# same for test

ResponsesSummary_artn <- gnet4s_artn_recog[, .(rowCount = .N), by = .(multichar_knowresp,type,tq,response_test_response)]

TestRespSum_artn <- ResponsesSummary_artn[!(tq == "study"),]

mycols  = c("response_test_response","multichar_knowresp","type","tq")
setkeyv(TestRespSum_artn,mycols)
Test_ResponsesByType_artn <- TestRespSum_artn[J(do.call(CJ,lapply(mycols,function(x)unique(get(x)))))]
for (i in 1:nrow(Test_ResponsesByType_artn)){
  if (is.na(Test_ResponsesByType_artn$rowCount[i])){
    Test_ResponsesByType_artn$rowCount[i] <- 0
  }
}
rm(TestRespSum_artn,ResponsesSummary_artn)

Test_RespWide_artn <- Test_ResponsesByType_artn %>% 
  # select only the columns we're interested in
  select(multichar_knowresp, type, tq, response_test_response, rowCount) %>% 
  # use pivot_wider to go from long to wide format
  pivot_wider(names_from = "response_test_response", 
              names_prefix = "resp_",
              values_from = "rowCount")
rm(Test_ResponsesByType_artn)

MaybeRespSummary <- gnet4s_artn_recog[response_test_response == "down", .(rowCount = .N), by = .(multichar_knowresp)]

gnet4s_artn_MissedRespSummary <- MissedResSummary_artn
rm(MissedResSummary_artn)
gnet4s_artn_StudyCR <- StudyCR_artn
rm(StudyCR_artn)
gnet4s_artn_StudyRespSummary <- Study_RespWide_artn
rm(Study_RespWide_artn)
gnet4s_artn_TestRespSummary <- Test_RespWide_artn
rm(Test_RespWide_artn)
rm(i,mycols)
gnet4s_artn_MaybeRespSummary <- MaybeRespSummary
rm(MaybeRespSummary)




