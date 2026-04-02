library(data.table)
library(lubridate)

email <- as.data.table(read.csv("uid_vdiscr.csv"))
colnames(email) <- c("id","email")
email2 <- as.data.table(read.csv("uid_vdiscr_elte.csv"))
colnames(email2) <- c("id","email")
email <- rbind(email,email2)
rm(email2)

register <- as.data.table(read.csv("visual_patterns_original_-_register.csv",encoding = "UTF-8"))
colnames(register) <- c("date","confirm","id","age","gen","edu","health","sleep","caff","smoke","drug")
# a person typed her email address instead of the code
# we add it from the outgoing data of emailing script
# register$id <- as.character(register$id)
register[id == "kiszelyattila96@gmail.com", id := "1952"]

email$id <- as.factor(email$id)
register$id <- as.factor(register$id)

registernmail<- merge(email, register, by="id")

registernmail$gen <- as.character(registernmail$gen)
registernmail[gen == "Nő", gen := 1]
registernmail[gen == "Férfi", gen := 0]
registernmail$gen <- as.numeric(registernmail$gen)

write.csv(registernmail,"run2_register.csv", row.names = FALSE)

