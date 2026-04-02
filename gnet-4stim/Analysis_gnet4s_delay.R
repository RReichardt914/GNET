library(afex)
library(emmeans)
library(effsize)

# STUDY #

ANOVA_gnet4s_delay_ST <- aov_ez(id = "multichar_knowresp",dv = "resp_right",within = "type",data = filter(gnet4s_delay_StudyRespSummary, !(multichar_knowresp %in% exclude_gnet4s_delay) & type != "prototype"))
print(ANOVA_gnet4s_delay_ST)
PostHoc_gnet4s_delay_ST <- emmeans(ANOVA_gnet4s_delay_ST, ~ type)
pairs(PostHoc_gnet4s_delay_ST)
contrast(PostHoc_gnet4s_delay_ST, "poly")

library(ggplot2)
png('gnet4s_delay_STUDY.png')
ggplot(filter(gnet4s_delay_StudyRespSummary, !(multichar_knowresp %in% exclude_gnet4s_delay) & type != "prototype"), aes(x=type, y=resp_right, color = type)) + 
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
dev.off()
#####################

# TEST #

library(rstatix)

a <- c(select(filter(gnet4s_delay_TestRespSummary, !(multichar_knowresp %in% exclude_gnet4s_delay) & tq == "old"),resp_left))$resp_left
b <- c(select(filter(gnet4s_delay_TestRespSummary, !(multichar_knowresp %in% exclude_gnet4s_delay) & tq == "new"),resp_left))$resp_left

diff <- a - b

shapiro.test(diff)

TTEST_gnet4s_delay_T <- t.test(a,b,paired = TRUE)

COHEND_gnet4s_delay_T <- cohen.d(a,b,paired = TRUE,conf.level=0.95)

WILCOXT_gnet4s_delay_T <- wilcox.test(a,b,paired = TRUE,conf.level = 0.95)

rm(a,b)

png('gnet4s_delay_TEST1.png')
ggplot(filter(gnet4s_delay_TestRespSummary,!(multichar_knowresp %in% exclude_gnet4s_delay)), aes(tq,resp_left, color=type)) +
  stat_summary(geom = "point",fun.y = "mean",size = 2,shape = 16,position=position_dodge(width=0.4)) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.2,position=position_dodge(width=0.4)) +
  stat_summary(fun.y=mean, geom="text", show.legend = FALSE, 
               vjust=0.0,hjust=1.5, position=position_dodge(width=0.4),aes( label=round(..y.., digits=3))) +
  #geom_jitter(width=0.2, alpha=0.2) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", col ="black", width = 0.2) +
  stat_summary(geom = "point",fun.y = "mean",col="black",size = 2,shape = 16) +
  stat_summary(fun.y=mean, colour="black", geom="text", show.legend = FALSE, 
               vjust=0.0,hjust=-1, aes( label=round(..y.., digits=3))) +
  scale_color_discrete(name = "Stimulus category", labels=c("diffarr" = "fully novel", "sarr" = "novel shapes & colors","sarrcol" = "novel shapes", "sarrsha" = "novel colors", "sarrshacol" = "novel color combinations")) + 
  ylab("Mean 'already seen' responses") + 
  xlab("") + 
  scale_x_discrete(labels=c("recog_O" = "Correct recognition", "fa_N" = "False positive"))
dev.off()
######################

# TEST #
# ANOVA

gnet4s_delay_TestRespByType <- left_join(filter(gnet4s_delay_TestRespSummary,!(multichar_knowresp %in% exclude_gnet4s_delay) & tq=="old"),
                                        filter(gnet4s_delay_TestRespSummary,!(multichar_knowresp %in% exclude_gnet4s_delay) & tq=="new"),
                                        by=c("multichar_knowresp","type"))

colnames(gnet4s_delay_TestRespByType) <- c("multichar_knowresp","type","tq.x","noresp_O","maybe_O","recog_O","miss_O",
                                          "tq.y","noresp_N","maybe_N","fa_N","reject_N")

gnet4s_delay_TestRespByType <- select(gnet4s_delay_TestRespByType,-tq.x,-tq.y)

gnet4s_delay_TestRespByType <- mutate(gnet4s_delay_TestRespByType,CorrRecog = recog_O - fa_N)
gnet4s_delay_TestRespByType <- mutate(gnet4s_delay_TestRespByType,CorrRejec = reject_N - miss_O)
gnet4s_delay_TestRespByType <- mutate(gnet4s_delay_TestRespByType,OverallCCat = CorrRecog + CorrRejec)

ANOVA_gnet4s_delay_T <- aov_ez(id = "multichar_knowresp", dv = "CorrRecog",data = gnet4s_delay_TestRespByType, within = c("type"))
print(ANOVA_gnet4s_delay_T)

PostHoc_gnet4s_delay_T <- emmeans(ANOVA_gnet4s_delay_T, ~ type)
pairs(PostHoc_gnet4s_delay_T)
contrast(pairs(PostHoc_gnet4s_delay_T), "poly")

png('gnet4s_delay_TEST2.png')
ggplot(filter(gnet4s_delay_TestRespByType,!(multichar_knowresp %in% exclude_gnet4s_delay)), aes(x=type, y=CorrRecog, color=type))+
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
dev.off()
################################