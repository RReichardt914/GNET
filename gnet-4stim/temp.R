d1 <- filter(gnet4s_orig_StudyRespSummary, type != "prototype")
d1$des <- "orig"
d2 <- filter(gnet4s_artn_StudyRespSummary, type != "prototype")
d2$des <- "artn"
temporarydat <- rbind(d1,d2)
rm(d1,d2)
tempANOVA <- aov_ez(id = "multichar_knowresp",dv = "resp_right",within = "type",between = "des", data = temporarydat)
print(tempANOVA)
tempPH <- emmeans(tempANOVA, ~ des)
pairs(tempPH)
contrast(tempPH, "poly")




write.csv(gnet4s_delay_recall,"gnet4s_delay_recall.csv")

write.csv(gnet4s_delart_recall,"gnet4s_delart_recall.csv")

write.csv(gnet4s_artn_recall,"gnet4s_artn_recall.csv")
