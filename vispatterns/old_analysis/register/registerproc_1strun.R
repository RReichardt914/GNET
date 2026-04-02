library(data.table)
library(lubridate)
email <- as.data.table(read.csv("idemail.csv"))
colnames(email) <- c("id","email")
email$id <- as.factor(email$id)

jk <- as.data.table(read.csv("vispatterns_jk.csv",encoding = "UTF-8"))
colnames(jk) <- c("date","id","age","gen","health","caff","smoke","drug")
jk$id <- as.factor(jk$id)

jkwid <- merge(email, jk, by="id")
jkwid$gen <- as.character(jkwid$gen)
jkwid[gen == "Nő", gen := 1]
jkwid[gen == "Férfi", gen := 0]
jkwid$gen <- as.numeric(jkwid$gen)

write.csv(jkwid,"run1_register.csv", row.names = FALSE)




