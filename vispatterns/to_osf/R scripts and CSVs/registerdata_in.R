jkwid <- as.data.table(read.csv("run1_register_exclusions.csv",encoding = "UTF-8", sep = ";"))
registernmail <- as.data.table(read.csv("run2_register_exclusions.csv",encoding = "UTF-8", sep = ";"))

needtoexcl <- jkwid[exclude == 1,]
needtoexcl <- needtoexcl[,c("X.U.FEFF.id")]

temp <- registernmail[exclude == 1,]
temp <- temp[,c("X.U.FEFF.id")]

needtoexcl <- rbind(needtoexcl,temp)
rm(temp)

exptimes <- jkwid[,c("X.U.FEFF.id","date")]
exptimes[,exptime := ymd_hm(date)]
exptimes[,hour := hour(exptime)]
exptimes[,min := minute(exptime)]

exptimes2 <- registernmail[,c("X.U.FEFF.id","date")]
exptimes2[,exptime := ymd_hm(date)]
exptimes2[,hour := hour(exptime)]
exptimes2[,min := minute(exptime)]

exptimes <- rbind(exptimes,exptimes2)

rm(exptimes2)

# ggplot(exptimes,aes(hour)) +
#   geom_histogram(color="black",fill="lightblue") +
#   ggtitle("The hour of the day when participants completed the task") +
#   ylab("Nr of participants") +
#   xlab("Hour of the day")
