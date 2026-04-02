temp1 <- Test_ResponsesByType[,mean(recog_O),by=type]
temp1$resp <- "Már láttam"

temp <- Test_ResponsesByType[,mean(miss_O),by=type]
temp$resp <- "Nem láttam"

temp <- rbind(temp1,temp)

ggplot(temp, aes(fill=resp, y=V1, x=type)) + 
  geom_bar(position="stack", stat="identity") +
  scale_x_discrete(labels=c("diffarr" = "ful. n.", "sarr" = "n. s. & c.",
                            "sarrcol" = "n. s.", "sarrsha" = "n. c.", "sarrshacol" = "n. c. c.")) +
  scale_fill_discrete(name = "Válasz") +
  xlab("") +
  ylab("")
  


###

temp1 <- Test_ResponsesByType[,mean(fa_N),by=type]
temp1$resp <- "Már láttam"

temp <- Test_ResponsesByType[,mean(reject_N),by=type]
temp$resp <- "Nem láttam"

temp <- rbind(temp1,temp)

ggplot(temp, aes(fill=resp, y=V1, x=type)) + 
  geom_bar(position="stack", stat="identity") +
  scale_x_discrete(labels=c("diffarr" = "ful. n.", "sarr" = "n. s. & c.",
                            "sarrcol" = "n. s.", "sarrsha" = "n. c.", "sarrshacol" = "n. c. c.")) +
  scale_fill_discrete(name = "Válasz") +
  xlab("") +
  ylab("")