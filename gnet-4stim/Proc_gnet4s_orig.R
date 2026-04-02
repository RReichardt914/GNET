library(data.table)
library(stringr)
library(tidyverse)

# get data from csv
##############################################
gnet4s_orig <- read.csv("gnet4s_orig-PREPROC.csv",sep=",") %>%
  as.data.table() %>%
  filter(!(is.na(count_study_response)))
##################################################


# work through ids
####################################
for (i in 1:nrow(gnet4s_orig)){
  gnet4s_orig[i,"multichar_knowresp"] <- str_replace(gnet4s_orig[i,"multichar_knowresp"], "caps", "")
}

problem_children <- gnet4s_orig %>%
  group_by(multichar_knowresp) %>%
  summarise(N = n()) %>%
  filter(N > 144) %>%
  pull(multichar_knowresp)

gnet4s_orig <- gnet4s_orig %>%
  filter(!(multichar_knowresp %in% problem_children))

rm(problem_children)
############################################


gnet4s_orig_recall <- gnet4s_orig[!(cell1 == "NA"&cell2 == "NA"&cell3 == "NA"&cell4 == "NA"&cell5 == "NA"),]

gnet4s_orig_recog <- gnet4s_orig[is.na(plc1c),]
gnet4s_orig_recog <- gnet4s_orig_recog[,c("count_study_response","count_test_response","date_startdate","date_starttime","multichar_knowresp","p2dur",
                                          "response_study_response","response_test_response","response_time_study_response","response_time_test_response",
                                          "rtnr","stim","type","tq")]

ResponsesSummary <- gnet4s_orig_recog[, .(rowCount = .N), by = .(multichar_knowresp,type,tq,response_study_response)]


MissedResSummary <- ResponsesSummary[response_study_response == "None", sum(rowCount),by = .(multichar_knowresp)]
missing <- data.table(multichar_knowresp = setdiff(unique(gnet4s_orig$multichar_knowresp),MissedResSummary$multichar_knowresp), V1 = c(0,0))
MissedResSummary <- rbind(MissedResSummary,missing)
rm(missing)

#MissedResSummary <- MissedResSummary[V1 > 66,]

#exclude <- MissedResSummary$multichar_knowresp

StudySummary <- gnet4s_orig_recog[is.na(count_test_response) & response_study_response == "left" & type == "prototype", .N/20, by=.(multichar_knowresp)]

StudyFA <- gnet4s_orig_recog[is.na(count_test_response) & response_study_response == "left" & type != "prototype", .N/40, by=.(multichar_knowresp)]
missing <- data.table(multichar_knowresp = setdiff(unique(gnet4s_orig$multichar_knowresp),StudyFA$multichar_knowresp), V1 = c(0,0))
StudyFA <- rbind(StudyFA,missing)
rm(missing)


StudyCR <- merge(StudySummary, StudyFA,by=c("multichar_knowresp"), all = TRUE)
colnames(StudyCR) <- c("multichar_knowresp","R","FA")
StudyCR[is.na(FA), FA:=0]
StudyCR[,CR := R - FA]
rm(StudySummary,StudyFA)

StudyRespSum <- ResponsesSummary[tq == "study",]
TestRespSum <- ResponsesSummary[!(tq == "study"),]

mycols  = c("response_study_response","multichar_knowresp","type")
setkeyv(StudyRespSum,mycols)
Study_ResponsesByType <- StudyRespSum[J(do.call(CJ,lapply(mycols,function(x)unique(get(x)))))]
for (i in 1:nrow(Study_ResponsesByType)){
  if (is.na(Study_ResponsesByType$rowCount[i])){
    Study_ResponsesByType$rowCount[i] <- 0
  }
}
Study_ResponsesByType$tq <- NULL
rm(StudyRespSum,ResponsesSummary)


Study_RespWide <- Study_ResponsesByType %>% 
  # select only the columns we're interested in
  select(multichar_knowresp, type, response_study_response, rowCount) %>% 
  # use pivot_wider to go from long to wide format
  pivot_wider(names_from = "response_study_response", 
              names_prefix = "resp_",
              values_from = "rowCount")
rm(Study_ResponsesByType)

# same for test

ResponsesSummary <- gnet4s_orig_recog[, .(rowCount = .N), by = .(multichar_knowresp,type,tq,response_test_response)]

TestRespSum <- ResponsesSummary[!(tq == "study"),]

mycols  = c("response_test_response","multichar_knowresp","type","tq")
setkeyv(TestRespSum,mycols)
Test_ResponsesByType <- TestRespSum[J(do.call(CJ,lapply(mycols,function(x)unique(get(x)))))]
for (i in 1:nrow(Test_ResponsesByType)){
  if (is.na(Test_ResponsesByType$rowCount[i])){
    Test_ResponsesByType$rowCount[i] <- 0
  }
}
rm(TestRespSum,ResponsesSummary)

Test_RespWide <- Test_ResponsesByType %>% 
  # select only the columns we're interested in
  select(multichar_knowresp, type, tq, response_test_response, rowCount) %>% 
  # use pivot_wider to go from long to wide format
  pivot_wider(names_from = "response_test_response", 
              names_prefix = "resp_",
              values_from = "rowCount")
rm(Test_ResponsesByType)


MaybeRespSummary <- gnet4s_orig_recog[response_test_response == "down", .(rowCount = .N), by = .(multichar_knowresp)]

gnet4s_orig_MissedRespSummary <- MissedResSummary
rm(MissedResSummary)
gnet4s_orig_StudyCR <- StudyCR
rm(StudyCR)
gnet4s_orig_StudyRespSummary <- Study_RespWide
rm(Study_RespWide)
gnet4s_orig_TestRespSummary <- Test_RespWide
rm(Test_RespWide)
rm(i,mycols)
gnet4s_orig_MaybeRespSummary <- MaybeRespSummary
rm(MaybeRespSummary)
