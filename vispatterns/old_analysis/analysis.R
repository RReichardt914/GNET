

# study szakasz elemzés
# recog rate in study
strecog <- study_dat[type == "prototype"]
strecog$type <- NULL
strecog$V1 <- NULL

ezANOVA(study_dat, dv =  crrate, within = type, wid = workerId)
a1 <- aov_ez("workerId", "crrate",
             study_dat, within = "type")
m3 <- emmeans(a1, ~ type)
pairs(m3)
# csak az újakra adott válaszok
study_dat_news <- study_dat[type != "prototype"]
ezANOVA(study_dat_news, dv =  crrate, within = type, wid = workerId)
a1 <- aov_ez("workerId", "crrate",
             study_dat_news, within = "type")
m3 <- emmeans(a1, ~ type)
pairs(m3)
ggplot(study_dat_news, aes(x=type, y=crrate, fill = type)) + 
  geom_boxplot()

study_dat[type == "prototype", type := "z_prototype"] # z... so that it is on the far right
ggplot(study_dat, aes(x=type, y=crrate, fill = type)) + 
  geom_boxplot()
ggsave("study_boxplot.png")

# test szakasz

ezANOVA(test_dat_c, dv =  rowCount, within = .(type,tq,rkey), wid = workerId)
a1 <- aov_ez("workerId", "rowCount",
             test_dat_c, within = c("type", "tq", "rkey"))
m3 <- emmeans(a1, ~ type + tq + rkey)
pairs(m3)
posthoc <- pairs(m3)
# only one response type
a1 <- aov_ez("workerId", "rowCount",
             test_dat_c[rkey == "right"], within = c("type", "tq"))
m3 <- emmeans(a1, ~ type + tq)
pairs(m3)
posthoc <- pairs(m3)



ggplot(test_dat[tq == "fam"], aes(x=rkey, y=freq, fill=type))+
  geom_boxplot()
ggsave("testfam_boxplot.png")

ggplot(test_dat[tq == "new"], aes(x=rkey, y=freq, fill=type))+
  geom_boxplot()
ggsave("testnew_boxplot.png")


