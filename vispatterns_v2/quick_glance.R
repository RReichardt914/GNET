correct <- gnet2_data[test_quality == "new" & rt_corr == 1, .N, by=workerId]
miss <- gnet2_data[test_quality == "new" & rt_corr == 0, .N, by=workerId]
sum <- merge(correct,miss,by="workerId")
colnames(sum) <- c("workerId","CorrReject","FalseAlarm")
correct <- gnet2_data[test_quality == "fam" & rt_corr == 1, .N, by=workerId]
miss <- gnet2_data[test_quality == "fam" & rt_corr == 0, .N, by=workerId]
sum2 <- merge(correct,miss,by="workerId")
colnames(sum2) <- c("workerId","CorrRecog","Miss")
all <- merge(sum,sum2,by="workerId")
all[,RecognitionScore := CorrRecog - FalseAlarm]
all[,RejectionScore := CorrReject - Miss]
all[,DistinctionScore := RecognitionScore + RejectionScore]


ggplot(all, aes(RecognitionScore)) + 
  geom_bar()

ggplot(all, aes(RejectionScore)) +
  geom_bar()

ggplot(all, aes(DistinctionScore)) +
  geom_bar()

# just for gnet_v2
stprotrecog <- gnet2_data[stimtype == "prototype" & st_corr == 1, .N, by=workerId]
stdiffrecog <- gnet2_data[stimtype != "prototype" & st_corr == 1 & total_responses < 76, .N, by=workerId]
# jsut for judge diff
gnet2_data$response_study_response <- as.numeric(gnet2_data$response_study_response)
data <- gnet2_data[!is.na(response_study_response),mean(response_study_response),by=c("workerId","stimtype")]
ggplot(data,aes(stimtype,V1)) + 
  geom_jitter(width=0.2) + 
  stat_summary(fun.y=mean, colour="red", geom="text", show.legend = FALSE, 
               vjust=-3.0, aes( label=round(..y.., digits=3)))
######
# BY STIM RECOG

dat <- gnet2_data[!(is.na(rt_corr)),sum(rt_corr),by=c("stim")]

dat$stimtype <- ""
dat[str_detect(stim, "prototype"),stimtype:="prototype"]
dat[str_detect(stim, "p[1-5]d1"),stimtype:="D1"]
dat[str_detect(stim, "p[1-5]d2"),stimtype:="D2"]
dat[str_detect(stim, "p[1-5]d3"),stimtype:="D3"]
dat[str_detect(stim, "p[1-5]d4"),stimtype:="D4"]
dat[str_detect(stim, "p[6-9]"),stimtype:="D5"]
dat[str_detect(stim, "p10"),stimtype:="D5"]
# dat$stimtype <- as.factor(dat$stimtype, ordered = TRUE, levels = c("D1","D2","D3","D4","D5"))
ggplot(dat[order(dat$stimtype)],aes(x = V1, y = stim, fill=stimtype, order = stimtype)) + geom_col()
# not in order
