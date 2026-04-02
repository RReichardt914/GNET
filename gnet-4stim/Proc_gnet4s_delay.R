library(data.table)
library(stringr)
library(tidyverse)

# get data from csv
##############################################
gnet4s_delay_ST <- read.csv("gnet4s_delay-ST-PREPROC.csv",sep=",") %>%
  as.data.table() %>%
  filter(!(is.na(count_study_response)))

gnet4s_delay_TE <- read.csv("gnet4s_delay-TE-PREPROC.csv",sep=",") %>%
  as.data.table() %>%
  filter(!(is.na(count_test_response)))
##################################################

# work through ids
####################################
for (i in 1:nrow(gnet4s_delay_ST)){
  gnet4s_delay_ST$multichar_knowresp[i] <- str_replace(gnet4s_delay_ST$multichar_knowresp[i], "caps", "")
}
for (i in 1:nrow(gnet4s_delay_TE)){
  gnet4s_delay_TE$multichar_knowresp[i] <- str_replace(gnet4s_delay_TE$multichar_knowresp[i], "caps", "")
}

problem_children <- gnet4s_delay_ST %>%
  group_by(multichar_knowresp) %>%
  summarise(N = n()) %>%
  filter(N > 60) %>%
  pull(multichar_knowresp)

problem_children2 <- gnet4s_delay_TE %>%
  group_by(multichar_knowresp) %>%
  summarise(N = n()) %>%
  filter(N != 84) %>%
  pull(multichar_knowresp)

problem_children <- c(problem_children,setdiff(unique(gnet4s_delay_ST$multichar_knowresp),unique(gnet4s_delay_TE$multichar_knowresp)))
problem_children2 <- c(problem_children2,setdiff(unique(gnet4s_delay_TE$multichar_knowresp),unique(gnet4s_delay_ST$multichar_knowresp)))

gnet4s_delay_ST <- gnet4s_delay_ST %>%
  filter(!(multichar_knowresp %in% problem_children)) %>%
  filter(!(multichar_knowresp %in% problem_children2))

gnet4s_delay_TE <- gnet4s_delay_TE %>%
  filter(!(multichar_knowresp %in% problem_children)) %>%
  filter(!(multichar_knowresp %in% problem_children2))

rm(problem_children,problem_children2)

#generate recall table
gnet4s_delay_recall <- gnet4s_delay_TE[!(cell1 == "NA"&cell2 == "NA"&cell3 == "NA"&cell4 == "NA"&cell5 == "NA"),]
gnet4s_delay_TE <- gnet4s_delay_TE[rtnr == 0,]

############################################

#responses

ResponsesSummary_delay <- gnet4s_delay_ST[, .(rowCount = .N), by = .(multichar_knowresp,type,response_study_response)]

MissedResSummary_delay <- ResponsesSummary_delay[response_study_response == "None", sum(rowCount),by = .(multichar_knowresp)]
missing <- data.table(multichar_knowresp = setdiff(unique(gnet4s_delay_ST$multichar_knowresp),MissedResSummary_delay$multichar_knowresp), V1 = c(0,0))
MissedResSummary_delay <- rbind(MissedResSummary_delay,missing)
rm(missing)

StudySummary <- gnet4s_delay_ST[response_study_response == "left" & type == "prototype", .N/20, by=.(multichar_knowresp)]

StudyFA <- gnet4s_delay_ST[response_study_response == "left" & type != "prototype", .N/40, by=.(multichar_knowresp)]
missing <- data.table(multichar_knowresp = setdiff(unique(gnet4s_delay_ST$multichar_knowresp),StudyFA$multichar_knowresp), V1 = c(0,0))
StudyFA <- rbind(StudyFA,missing)
rm(missing)

######################################################################################################################################################

StudySummary_delay <- StudySummary
StudyFA_delay <- StudyFA
rm(StudySummary,StudyFA)

StudyCR_delay <- merge(StudySummary_delay, StudyFA_delay,by=c("multichar_knowresp"), all = TRUE)
colnames(StudyCR_delay) <- c("multichar_knowresp","R","FA")
StudyCR_delay[is.na(FA), FA:=0]
StudyCR_delay[,CR := R - FA]
rm(StudySummary_delay,StudyFA_delay)

mycols  = c("response_study_response","multichar_knowresp","type")
setkeyv(ResponsesSummary_delay,mycols)
Study_ResponsesByType_delay <- ResponsesSummary_delay[J(do.call(CJ,lapply(mycols,function(x)unique(get(x)))))]
for (i in 1:nrow(Study_ResponsesByType_delay)){
  if (is.na(Study_ResponsesByType_delay$rowCount[i])){
    Study_ResponsesByType_delay$rowCount[i] <- 0
  }
}
rm(ResponsesSummary_delay)

Study_RespWide_delay <- Study_ResponsesByType_delay %>% 
  # select only the columns we're interested in
  select(multichar_knowresp, type, response_study_response, rowCount) %>% 
  # use pivot_wider to go from long to wide format
  pivot_wider(names_from = "response_study_response", 
              names_prefix = "resp_",
              values_from = "rowCount")
rm(Study_ResponsesByType_delay)




# same for test

ResponsesSummary_delay <- gnet4s_delay_TE[, .(rowCount = .N), by = .(multichar_knowresp,type,tq,response_test_response)]

mycols  = c("response_test_response","multichar_knowresp","type","tq")
setkeyv(ResponsesSummary_delay,mycols)
Test_ResponsesByType_delay <- ResponsesSummary_delay[J(do.call(CJ,lapply(mycols,function(x)unique(get(x)))))]
for (i in 1:nrow(Test_ResponsesByType_delay)){
  if (is.na(Test_ResponsesByType_delay$rowCount[i])){
    Test_ResponsesByType_delay$rowCount[i] <- 0
  }
}
rm(ResponsesSummary_delay)

Test_RespWide_delay <- Test_ResponsesByType_delay %>% 
  # select only the columns we're interested in
  select(multichar_knowresp, type, tq, response_test_response, rowCount) %>% 
  # use pivot_wider to go from long to wide format
  pivot_wider(names_from = "response_test_response", 
              names_prefix = "resp_",
              values_from = "rowCount")
rm(Test_ResponsesByType_delay)

MaybeRespSummary <- gnet4s_delay_TE[response_test_response == "down", .(rowCount = .N), by = .(multichar_knowresp)]

gnet4s_delay_MissedRespSummary <- MissedResSummary_delay
rm(MissedResSummary_delay)
gnet4s_delay_StudyCR <- StudyCR_delay
rm(StudyCR_delay)
gnet4s_delay_StudyRespSummary <- Study_RespWide_delay
rm(Study_RespWide_delay)
gnet4s_delay_TestRespSummary <- Test_RespWide_delay
rm(Test_RespWide_delay)
rm(i,mycols)
gnet4s_delay_MaybeRespSummary <- MaybeRespSummary
rm(MaybeRespSummary)
