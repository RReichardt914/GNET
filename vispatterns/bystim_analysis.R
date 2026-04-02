data <- GNET_pilot_filtered
data$rt_corr <- 0
data[tq=="old" & response=="left", rt_corr := 1,]
data[tq=="new" & response=="right", rt_corr := 1,]
data <- data[,sum(rt_corr),by=c("stim","tq","type","DTF","DTAF","DTAN","DTN_byPhase")] 
data <- data[tq != "study"]
colnames(data)[8] <- "corrresp"

d2 <- GNET_pilot_filtered
d2$oldr <- 0
d2[response=="left", oldr := 1,]
d2 <- d2[,sum(oldr),by=c("stim","tq","type","DTF","DTAF","DTAN","DTN_byPhase")] 
colnames(d2)[8] <- "oldresp"

d3 <- GNET_pilot_filtered
d3$newr <- 0
d3[response=="right", newr := 1,]
d3 <- d3[,sum(newr),by=c("stim","tq","type","DTF","DTAF","DTAN","DTN_byPhase")]
colnames(d3)[8] <- "newresp"

d4 <- GNET_pilot_filtered
d4$newr <- 0
d4[response=="down", newr := 1,]
d4 <- d4[,sum(newr),by=c("stim","tq","type","DTF","DTAF","DTAN","DTN_byPhase")]
colnames(d4)[8] <- "mayberesp"

data <- merge(data,d2,by=c("stim","tq","type","DTF","DTAF","DTAN","DTN_byPhase"))
data <- merge(data,d3,by=c("stim","tq","type","DTF","DTAF","DTAN","DTN_byPhase"))
data <- merge(data,d4,by=c("stim","tq","type","DTF","DTAF","DTAN","DTN_byPhase"))
rm(d2,d3,d4)
data[,index:=oldresp/newresp]
data[,oldresp_prob := oldresp/(oldresp+newresp+mayberesp)]
data[,mayberesp_prob := mayberesp/(oldresp+newresp+mayberesp)]
data[,newresp_prob := newresp/(oldresp+newresp+mayberesp)]

#RESPONSES TO DISTRACTORS
ggplot(data[tq == "new"], aes(x=stim, y=corrresp, label = DTF, fill = type)) +
  geom_col() +
  ggtitle("Responses to distractors") +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  geom_text(vjust=2) +
  xlab("Distractor stimulus") +
  ylab("Number of correct responses") +
  facet_wrap(~type, scales="free_x") +
  theme(strip.text.x = element_blank()) +
  scale_fill_discrete(name = "Stimulus category", labels=c("diffarr" = "fully novel", "sarr" = "novel shapes & colors", "sarrcol" = "novel shapes", "sarrsha" = "novel colors", "sarrshacol" = "novel color combinations")) 

#RESPONSES TO PREV PRES
ggplot(data[tq == "old"], aes(x=stim, y=corrresp, label = DTF, fill = type)) +
  geom_col() +
  ggtitle("Responses to previously presented stimuli") +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  geom_text(vjust=2) +
  xlab("Distractor stimulus") +
  ylab("Number of correct responses") +
  facet_wrap(~type, scales="free_x") +
  theme(strip.text.x = element_blank()) +
  scale_fill_discrete(name = "Stimulus category", labels=c("diffarr" = "fully novel", "sarr" = "novel shapes & colors", "sarrcol" = "novel shapes", "sarrsha" = "novel colors", "sarrshacol" = "novel color combinations")) 
