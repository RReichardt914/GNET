library(data.table)
library(stringr)
library(tidyverse)

# get data from csv
##############################################
gnet4s_delart_ST <- read.csv("gnet4s_delart-ST-PREPROC.csv",sep=",") %>%
  as.data.table() %>%
  filter(!(is.na(count_study_response)))

gnet4s_delart_TE <- read.csv("gnet4s_delart-TE-PREPROC.csv",sep=",") %>%
  as.data.table() %>%
  filter(!(is.na(count_test_response)))
##################################################

# work through ids
####################################
for (i in 1:nrow(gnet4s_delart_ST)){
  gnet4s_delart_ST$multichar_knowresp[i] <- str_replace(gnet4s_delart_ST$multichar_knowresp[i], "caps", "")
}
for (i in 1:nrow(gnet4s_delart_TE)){
  gnet4s_delart_TE$multichar_knowresp[i] <- str_replace(gnet4s_delart_TE$multichar_knowresp[i], "caps", "")
}

problem_children <- gnet4s_delart_ST %>%
  group_by(multichar_knowresp) %>%
  summarise(N = n()) %>%
  filter(N > 60) %>%
  pull(multichar_knowresp)

problem_children2 <- gnet4s_delart_TE %>%
  group_by(multichar_knowresp) %>%
  summarise(N = n()) %>%
  filter(N != 84) %>%
  pull(multichar_knowresp)

problem_children <- c(problem_children,setdiff(unique(gnet4s_delart_ST$multichar_knowresp),unique(gnet4s_delart_TE$multichar_knowresp)))
problem_children2 <- c(problem_children2,setdiff(unique(gnet4s_delart_TE$multichar_knowresp),unique(gnet4s_delart_ST$multichar_knowresp)))

gnet4s_delart_ST <- gnet4s_delart_ST %>%
  filter(!(multichar_knowresp %in% problem_children)) %>%
  filter(!(multichar_knowresp %in% problem_children2))

gnet4s_delart_TE <- gnet4s_delart_TE %>%
  filter(!(multichar_knowresp %in% problem_children)) %>%
  filter(!(multichar_knowresp %in% problem_children2))

rm(problem_children,problem_children2)

#generate recall table
gnet4s_delart_recall <- gnet4s_delart_TE[!(cell1 == "NA"&cell2 == "NA"&cell3 == "NA"&cell4 == "NA"&cell5 == "NA"),]
gnet4s_delart_TE <- gnet4s_delart_TE[rtnr == 0,]

####
# deliquent participant switched responses during study - bxa3vr
gnet4s_delart_ST[multichar_knowresp == "bxa3vr" & response_study_response == "left", response_study_response := "temp"]
gnet4s_delart_ST[multichar_knowresp == "bxa3vr" & response_study_response == "right", response_study_response := "left"]
gnet4s_delart_ST[multichar_knowresp == "bxa3vr" & response_study_response == "temp", response_study_response := "right"]
###

############################################

#responses

ResponsesSummary_delart <- gnet4s_delart_ST[, .(rowCount = .N), by = .(multichar_knowresp,type,response_study_response)]

MissedResSummary_delart <- ResponsesSummary_delart[response_study_response == "None", sum(rowCount),by = .(multichar_knowresp)]
missing <- data.table(multichar_knowresp = setdiff(unique(gnet4s_delart_ST$multichar_knowresp),MissedResSummary_delart$multichar_knowresp), V1 = c(0,0))
MissedResSummary_delart <- rbind(MissedResSummary_delart,missing)
rm(missing)

StudySummary <- gnet4s_delart_ST[response_study_response == "left" & type == "prototype", .N/20, by=.(multichar_knowresp)]

StudyFA <- gnet4s_delart_ST[response_study_response == "left" & type != "prototype", .N/40, by=.(multichar_knowresp)]
missing <- data.table(multichar_knowresp = setdiff(unique(gnet4s_delart_ST$multichar_knowresp),StudyFA$multichar_knowresp), V1 = c(0,0))
StudyFA <- rbind(StudyFA,missing)
rm(missing)

######################################################################################################################################################

StudySummary_delart <- StudySummary
StudyFA_delart <- StudyFA
rm(StudySummary,StudyFA)

StudyCR_delart <- merge(StudySummary_delart, StudyFA_delart,by=c("multichar_knowresp"), all = TRUE)
colnames(StudyCR_delart) <- c("multichar_knowresp","R","FA")
StudyCR_delart[is.na(FA), FA:=0]
StudyCR_delart[,CR := R - FA]
rm(StudySummary_delart,StudyFA_delart)

mycols  = c("response_study_response","multichar_knowresp","type")
setkeyv(ResponsesSummary_delart,mycols)
Study_ResponsesByType_delart <- ResponsesSummary_delart[J(do.call(CJ,lapply(mycols,function(x)unique(get(x)))))]
for (i in 1:nrow(Study_ResponsesByType_delart)){
  if (is.na(Study_ResponsesByType_delart$rowCount[i])){
    Study_ResponsesByType_delart$rowCount[i] <- 0
  }
}
rm(ResponsesSummary_delart)

Study_RespWide_delart <- Study_ResponsesByType_delart %>% 
  # select only the columns we're interested in
  select(multichar_knowresp, type, response_study_response, rowCount) %>% 
  # use pivot_wider to go from long to wide format
  pivot_wider(names_from = "response_study_response", 
              names_prefix = "resp_",
              values_from = "rowCount")
rm(Study_ResponsesByType_delart)




# same for test

ResponsesSummary_delart <- gnet4s_delart_TE[, .(rowCount = .N), by = .(multichar_knowresp,type,tq,response_test_response)]

mycols  = c("response_test_response","multichar_knowresp","type","tq")
setkeyv(ResponsesSummary_delart,mycols)
Test_ResponsesByType_delart <- ResponsesSummary_delart[J(do.call(CJ,lapply(mycols,function(x)unique(get(x)))))]
for (i in 1:nrow(Test_ResponsesByType_delart)){
  if (is.na(Test_ResponsesByType_delart$rowCount[i])){
    Test_ResponsesByType_delart$rowCount[i] <- 0
  }
}
rm(ResponsesSummary_delart)

Test_RespWide_delart <- Test_ResponsesByType_delart %>% 
  # select only the columns we're interested in
  select(multichar_knowresp, type, tq, response_test_response, rowCount) %>% 
  # use pivot_wider to go from long to wide format
  pivot_wider(names_from = "response_test_response", 
              names_prefix = "resp_",
              values_from = "rowCount")
rm(Test_ResponsesByType_delart)

MaybeRespSummary <- gnet4s_delart_TE[response_test_response == "down", .(rowCount = .N), by = .(multichar_knowresp)]

gnet4s_delart_MissedRespSummary <- MissedResSummary_delart
rm(MissedResSummary_delart)
gnet4s_delart_StudyCR <- StudyCR_delart
rm(StudyCR_delart)
gnet4s_delart_StudyRespSummary <- Study_RespWide_delart
rm(Study_RespWide_delart)
gnet4s_delart_TestRespSummary <- Test_RespWide_delart
rm(Test_RespWide_delart)
rm(i,mycols)
gnet4s_delart_MaybeRespSummary <- MaybeRespSummary
rm(MaybeRespSummary)
