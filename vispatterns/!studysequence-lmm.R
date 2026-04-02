tdlmm$tq <- as.factor(tdlmm$tq)
tdlmm$sColDTF <- scale(tdlmm$ColDTF)
tdlmm$sShaDTF <- scale(tdlmm$ShaDTF)
tdlmm$sArrDTF <- scale(tdlmm$ArrDTF)
logmr2 <- glmer(rt_resp ~ sColDTF * tq + sShaDTF * tq + sArrDTF * tq + (tq|workerId),
                data = tdlmm,
                family = binomial,
                control = glmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8, maxeval=100000)))
summary(logmr2)
plot_model(logmr2, type = "eff")
plot_model(logmr2, type = "int")[[2]]
plot_model(logmr2, type = "pred", terms = c("sShaDTF", "tq"))

#### lmms by dimension ####
library(ggpubr)

logmr1 <- glmer(rt_resp ~ ArrDTF * tq + (tq|workerId),
                data = tdlmm,
                family = binomial,
                control = glmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8, maxeval=100000)))
summary(logmr1)
p1 <- plot_model(logmr1, type = "eff")[[1]] +
  xlab("DTF - Arrangement dimension") +
  ylab("Probability of 'seen' response") +
  ggtitle("") +
  scale_color_discrete(name = "Trial type", labels=c("new" = "distractor", "old" = "prev. pres"))

p2 <- plot_model(logmr1, type = "eff")[[2]] +
  xlab("DTF - Arrangement dimension") +
  ylab("Probability of 'seen' response") +
  ggtitle("") +
  scale_x_discrete(limits=c("new" = "distractor", "old" = "previously presented"))

p3 <- plot_model(logmr1, type = "int") +
  xlab("DTF - Arrangement dimension") +
  ylab("Probability of 'seen' response") +
  ggtitle("") +
  scale_color_discrete(name = "Trial type", labels=c("new" = "distractor", "old" = "prev. pres"))

ggarrange(p1, p2, p3, ncol = 3, nrow = 1)

logmr2 <- glmer(rt_resp ~ ColDTF * tq + (tq|workerId),
                data = tdlmm,
                family = binomial,
                control = glmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8, maxeval=100000)))

summary(logmr2)
p1 <- plot_model(logmr2, type = "eff")[[1]] +
  xlab("DTF - Color dimension") +
  ylab("Probability of 'seen' response") +
  ggtitle("") +
  scale_color_discrete(name = "Trial type", labels=c("new" = "distractor", "old" = "prev. pres"))

p2 <- plot_model(logmr2, type = "eff")[[2]] +
  xlab("DTF - Color dimension") +
  ylab("Probability of 'seen' response") +
  ggtitle("") +
  scale_x_discrete(limits=c("new" = "distractor", "old" = "previously presented"))

p3 <- plot_model(logmr2, type = "int") +
  xlab("DTF - Color dimension") +
  ylab("Probability of 'seen' response") +
  ggtitle("") +
  scale_color_discrete(name = "Trial type", labels=c("new" = "distractor", "old" = "prev. pres"))

ggarrange(p1, p2, p3, ncol = 3, nrow = 1)

logmr3 <- glmer(rt_resp ~ ShaDTF * tq + (tq|workerId),
                data = tdlmm,
                family = binomial,
                control = glmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8, maxeval=100000)))

summary(logmr3)
p1 <- plot_model(logmr3, type = "eff")[[1]] +
  xlab("DTF - Shape dimension") +
  ylab("Probability of 'seen' response") +
  ggtitle("") +
  scale_color_discrete(name = "Trial type", labels=c("new" = "distractor", "old" = "prev. pres"))

p2 <- plot_model(logmr3, type = "eff")[[2]] +
  xlab("DTF - Shape dimension") +
  ylab("Probability of 'seen' response") +
  ggtitle("") +
  scale_x_discrete(limits=c("new" = "distractor", "old" = "previously presented"))

p3 <- plot_model(logmr3, type = "int") +
  xlab("DTF - Shape dimension") +
  ylab("Probability of 'seen' response") +
  ggtitle("") +
  scale_color_discrete(name = "Trial type", labels=c("new" = "distractor", "old" = "prev. pres"))

ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
#####


########################
t2lmm <- t2lmm[response_test_response.y != "down",]
t2lmm[response_test_response.y == "left", response_test_response.y := 1,]
t2lmm[response_test_response.y == "right", response_test_response.y := 0,]
t2lmm$response_test_response.y <- as.numeric(t2lmm$response_test_response.y)
t2lmm$count_study_sequence.x <- as.numeric(scale(t2lmm$count_study_sequence.x))
t2lmm <- t2lmm[response_study_response.x != "None",]
t2lmm[response_study_response.x == "right", response_study_response.x := 1]
t2lmm[response_study_response.x == "left", response_study_response.x := 0]
t2lmm$response_study_response.x <- as.factor(t2lmm$response_study_response.x)
t2lmm$count_test_sequence.y <- as.numeric(scale(t2lmm$count_test_sequence.y))


logmr6 <- glmer(response_test_response.y ~ count_test_sequence.y * DTF + (1|workerId),
                data = t2lmm,
                family = binomial,
                control = glmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8, maxeval=100000)))
summary(logmr6)
plot_model(logmr6, type = "eff")
plot_model(logmr6, type = "int")

model <- glmer(response_test_response.y ~ DTF * response_study_response.x + (1|workerId),
               data = t2lmm,
               family = binomial)
summary(model)
plot_model(model, type = "eff")
plot_model(model, type = "int")
