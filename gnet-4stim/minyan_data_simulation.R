gnet4s_actualdata <- as.data.table(gnet4s_delart_StudyRespSummary)
gnet4s_actualdata$hitrate <- 0
gnet4s_actualdata[, hitrate := (resp_right - resp_left)/(8 - resp_None)]
gnet4s_actualdata <- gnet4s_actualdata[type != "prototype"]
gnet4s_actualdata <- gnet4s_actualdata[!(multichar_knowresp %in% exclude_gnet4s_delart)]

library(afex)

gnet4s_actual_studyANOVA <- aov_ez(id = "multichar_knowresp",
                                   dv = "hitrate",
                                   within = "type",
                                   data = gnet4s_actualdata)


ggplot(gnet4s_actualdata, aes(x=type, y=hitrate, color = type)) + 
  geom_jitter(width=0.2, alpha=0.2) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2) +
  stat_summary(geom = "point",fun.y = "mean",col = "black",size = 2,shape = 16) +
  stat_summary(fun.y=mean, colour="black", geom="text", show.legend = FALSE, 
               vjust=-3.0, aes( label=round(..y.., digits=3))) +
  scale_x_discrete(labels=c("diffarr" = "fully novel", "sarr" = "novel shapes & colors",
                            "sarrcol" = "novel shapes", "sarrsha" = "novel colors", "sarrshacol" = "novel color combinations")) + 
  ggtitle("Number of correct rejections in the study phase by novel picture category") +
  xlab("") +
  ylab("Nr of correct rejections") +
  theme(legend.position = "none")

#######################################################

library(stringi)

randomcodes <- sprintf("%s%s%s", stri_rand_strings(36, 2, '[A-Z]'),
        stri_rand_strings(36, 2, '[0-9]'), stri_rand_strings(36, 2, '[A-Z]'))

typecodes <- c("diffarr","sarr","sarrcol","sarrsha","sarrshacol")

gnet4s_simulateddata <- data.table(multichar_knowresp = rep(randomcodes, each=5),
                                   type = rep(typecodes, times=36),
                                   hitrate = 0)

for (i in 1:nrow(gnet4s_simulateddata)){
  gnet4s_simulateddata[i,hitrate := rnorm(1, mean = gnet4s_actualdata[type == gnet4s_simulateddata[i,type],mean(hitrate)], sd = gnet4s_actualdata[type == gnet4s_simulateddata[i,type],sd(hitrate)])]
}

gnet4s_simulateddata[hitrate > 1.0, hitrate := 1.0]

gnet4s_simulated_studyANOVA <- aov_ez(id = "multichar_knowresp",
                                   dv = "hitrate",
                                   within = "type",
                                   data = gnet4s_simulateddata)

ggplot(gnet4s_simulateddata, aes(x=type, y=hitrate, color = type)) + 
  geom_jitter(width=0.2, alpha=0.2) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2) +
  stat_summary(geom = "point",fun.y = "mean",col = "black",size = 2,shape = 16) +
  stat_summary(fun.y=mean, colour="black", geom="text", show.legend = FALSE, 
               vjust=-3.0, aes( label=round(..y.., digits=3))) +
  scale_x_discrete(labels=c("diffarr" = "fully novel", "sarr" = "novel shapes & colors",
                            "sarrcol" = "novel shapes", "sarrsha" = "novel colors", "sarrshacol" = "novel color combinations")) + 
  ggtitle("Number of correct rejections in the study phase by novel picture category") +
  xlab("") +
  ylab("Nr of correct rejections") +
  theme(legend.position = "none")

#############################

ANOVA_gnet4s_delart_T <- aov_ez(id = "multichar_knowresp", dv = "CorrRecog",data = gnet4s_delart_TestRespByType, within = c("type"))
print(ANOVA_gnet4s_delart_T)

PostHoc_gnet4s_delart_T <- emmeans(ANOVA_gnet4s_delart_T, ~ type)
pairs(PostHoc_gnet4s_delart_T)
contrast(pairs(PostHoc_gnet4s_delart_T), "poly")

ggplot(filter(gnet4s_delart_TestRespByType,!(multichar_knowresp %in% exclude_gnet4s_delart)), aes(x=type, y=CorrRecog, color=type))+
  geom_jitter(width=0.3, alpha=0.3) +
  stat_summary(geom = "errorbar", fun.data = mean_se, col = "black", width = 0.2) +
  stat_summary(geom = "point",fun.y = "mean",col = "black",size = 2,shape = 16) +
  stat_summary(fun.y=mean, colour="black", geom="text", show.legend = FALSE, 
               vjust=-3.0, aes( label=round(..y.., digits=3))) +
  scale_x_discrete(labels=c("A" = "fully novel", "B" = "novel shapes & colors",
                            "D" = "novel shapes", "C" = "novel colors", "E" = "novel color combinations")) +
  ggtitle("Corrected recognition score in the test phase by picture category")+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")

library(stringi)

gnet4s_simulateddata_T <- data.table(multichar_knowresp = rep(randomcodes, each=5),
                                   type = rep(typecodes, times=36),
                                   CorrRecog = 0)

gnet4s_actualdata_T <- as.data.table(gnet4s_delart_TestRespByType)

for (i in 1:nrow(gnet4s_simulateddata_T)){
  if (gnet4s_simulateddata_T[i,type] == "diffarr"){
    gnet4s_simulateddata_T[i,CorrRecog := rnorm(1, 
                                                mean = gnet4s_actualdata_T[type == gnet4s_simulateddata_T[i,type],mean(CorrRecog)] + 2, 
                                                sd = gnet4s_actualdata_T[type == gnet4s_simulateddata_T[i,type],sd(CorrRecog)])]
    
  } else if (gnet4s_simulateddata_T[i,type] == "sarr"){
    gnet4s_simulateddata_T[i,CorrRecog := rnorm(1, 
                                                mean = gnet4s_actualdata_T[type == gnet4s_simulateddata_T[i,type],mean(CorrRecog)] + 1, 
                                                sd = gnet4s_actualdata_T[type == gnet4s_simulateddata_T[i,type],sd(CorrRecog)])]
    
  } else {
    gnet4s_simulateddata_T[i,CorrRecog := rnorm(1, 
                                                mean = gnet4s_actualdata_T[type == gnet4s_simulateddata_T[i,type],mean(CorrRecog)], 
                                                sd = gnet4s_actualdata_T[type == gnet4s_simulateddata_T[i,type],sd(CorrRecog)])]
  }
}

gnet4s_simulated_testANOVA <- aov_ez(id = "multichar_knowresp",
                                      dv = "CorrRecog",
                                      within = "type",
                                      data = gnet4s_simulateddata_T)

ggplot(gnet4s_simulateddata_T, aes(x=type, y=CorrRecog, color=type))+
  geom_jitter(width=0.3, alpha=0.3) +
  stat_summary(geom = "errorbar", fun.data = mean_se, col = "black", width = 0.2) +
  stat_summary(geom = "point",fun.y = "mean",col = "black",size = 2,shape = 16) +
  stat_summary(fun.y=mean, colour="black", geom="text", show.legend = FALSE, 
               vjust=-3.0, aes( label=round(..y.., digits=3))) +
  scale_x_discrete(labels=c("A" = "fully novel", "B" = "novel shapes & colors",
                            "D" = "novel shapes", "C" = "novel colors", "E" = "novel color combinations")) +
  ggtitle("Corrected recognition score in the test phase by picture category")+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")

##################################

gnet4s_simulateddata_T$grp <- "simulated_sleep"
gnet4s_actualdata_T$grp <- "actual_wake"
gnet4s_simandactualdata_T <- rbind(gnet4s_simulateddata_T,gnet4s_actualdata_T[,c("multichar_knowresp","type","CorrRecog","grp")])

gnet4s_simulated_testANOVA <- aov_ez(id = "multichar_knowresp",
                                     dv = "CorrRecog",
                                     within = "type",
                                     between = "grp",
                                     data = gnet4s_simandactualdata_T)
print(gnet4s_simulated_testANOVA)

ggplot(gnet4s_simandactualdata_T, aes(x=type, y=CorrRecog, color=type))+
  geom_jitter(width=0.3, alpha=0.3) +
  stat_summary(geom = "errorbar", fun.data = mean_se, col = "black", width = 0.2) +
  stat_summary(geom = "point",fun.y = "mean",col = "black",size = 2,shape = 16) +
  stat_summary(fun.y=mean, colour="black", geom="text", show.legend = FALSE, 
               vjust=-3.0, aes( label=round(..y.., digits=3))) +
  scale_x_discrete(labels=c("A" = "fully novel", "B" = "novel shapes & colors",
                            "D" = "novel shapes", "C" = "novel colors", "E" = "novel color combinations")) +
  ggtitle("Corrected recognition score in the test phase by picture category")+
  xlab("")+
  ylab("")+
  facet_grid(grp ~ .) +
  theme(legend.position = "none")
