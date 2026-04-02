# Study correct rejections ####
Study_ResponsesByType <-
  GNET_pilot_filtered[is.na(count_test_sequence),.N,by=.(response,ID,type)]
# we need to add some zeros for the type*tq*rkey(response) combinations that are left out

mycols  = c("response","ID","type")
setkeyv(Study_ResponsesByType,mycols)
Study_ResponsesByType_c <- Study_ResponsesByType[J(do.call(CJ,lapply(mycols,function(x)unique(get(x)))))]
for (i in 1:nrow(Study_ResponsesByType_c)){
  if (is.na(Study_ResponsesByType_c$N[i])){
    Study_ResponsesByType_c$N[i] <- 0
  }
}
Study_ResponsesByType <- Study_ResponsesByType_c
rm(Study_ResponsesByType_c,mycols,i)
Study_CorrRejectionByType <- Study_ResponsesByType[response == "right" & type != "prototype"]
#####
# Study_CorrRejectionByType , Study_ResponsesByType

# Test Corr recog/reject and overall ####
Test_CorrectResponsesByType <-
  GNET_pilot_filtered[!is.na(count_test_sequence), .N,by=.(response,ID,tq,type)]
# we need to add some zeros for the type*tq*rkey(response) combinations that are left out

mycols  = c("response","ID","tq","type")
setkeyv(Test_CorrectResponsesByType,mycols)
Test_CorrectResponsesByType_c <- Test_CorrectResponsesByType[J(do.call(CJ,lapply(mycols,function(x)unique(get(x)))))]
for (i in 1:nrow(Test_CorrectResponsesByType_c)){
  if (is.na(Test_CorrectResponsesByType_c$N[i])){
    Test_CorrectResponsesByType_c$N[i] <- 0
  }
}
Test_CorrectResponsesByType <- Test_CorrectResponsesByType_c
rm(Test_CorrectResponsesByType_c,mycols,i)


o1 <- Test_CorrectResponsesByType[tq=="old" & response=="down"]
colnames(o1)[5] <- "maybe_O"

o3 <- Test_CorrectResponsesByType[tq=="old" & response=="left"]
colnames(o3)[5] <- "recog_O"
o1 <- merge(o1,o3,by=c("ID","type","tq"))
o4 <- Test_CorrectResponsesByType[tq=="old" & response=="right"]
colnames(o4)[5] <- "miss_O"
o1 <- merge(o1,o4,by=c("ID","type","tq"))
Test_OldPResponses <- o1
Test_OldPResponses <- Test_OldPResponses[,c("ID","type",
                                            "maybe_O","recog_O","miss_O")]
rm(o1,o2,o3,o4)

o1 <- Test_CorrectResponsesByType[tq=="new" & response=="down"]
colnames(o1)[5] <- "maybe_N"

o3 <- Test_CorrectResponsesByType[tq=="new" & response=="left"]
colnames(o3)[5] <- "fa_N"
o1 <- merge(o1,o3,by=c("ID","type","tq"))
o4 <- Test_CorrectResponsesByType[tq=="new" & response=="right"]
colnames(o4)[5] <- "reject_N"
o1 <- merge(o1,o4,by=c("ID","type","tq"))
Test_NewPResponses <- o1
Test_NewPResponses <- Test_NewPResponses[,c("ID","type",
                                            "maybe_N","fa_N","reject_N")]
rm(o1,o2,o3,o4,Test_CorrectResponsesByType)

Test_ResponsesByType <- merge(Test_OldPResponses,Test_NewPResponses,by=c("ID","type"))
Test_ResponsesByType[,CorrRecog := recog_O - fa_N]
Test_ResponsesByType[,CorrRejec := reject_N - miss_O]
Test_ResponsesByType[,OverallCCat := CorrRecog + CorrRejec]
rm(Test_OldPResponses,Test_NewPResponses)

#####
# Test_ResponsesByType