library(lme4)
library(lmerTest)
library(sjPlot)

gnet4s_orig_LMMdata <- gnet4s_orig_recog %>% left_join(difference_indices, by="stim")
gnet4s_orig_LMMdata$type.y <- NULL
gnet4s_orig_LMMdata <- rename(gnet4s_orig_LMMdata, type = type.x)

gnet4s_orig_LMMdataTEST <- filter(gnet4s_orig_LMMdata, !(multichar_knowresp %in% exclude_gnet4s_orig) & tq != "study")
gnet4s_orig_LMMdataTEST$TESTRESP <- NA
gnet4s_orig_LMMdataTEST <- as.tibble(gnet4s_orig_LMMdataTEST)
gnet4s_orig_LMMdataTEST <- as.data.table(gnet4s_orig_LMMdataTEST)
gnet4s_orig_LMMdataTEST[response_test_response == "left",TESTRESP := 1]
gnet4s_orig_LMMdataTEST[response_test_response == "right",TESTRESP := 0]
gnet4s_orig_LMMdataTEST$TESTRESP <- as.numeric(gnet4s_orig_LMMdataTEST$TESTRESP)


gnet4s_orig_TLMM <- glmer(TESTRESP ~ DTF * tq + (1|multichar_knowresp),
                          data = gnet4s_orig_LMMdataTEST,
                          family = binomial)
summary(gnet4s_orig_TLMM)
plot_model(gnet4s_orig_TLMM, type = "eff")$DTF
plot_model(gnet4s_orig_TLMM, type = "eff")$tq
plot_model(gnet4s_orig_TLMM, type = "int")

#############################

gnet4s_orig_TLMM_NCA_DCO <- glmer(TESTRESP ~ NCA_DCO * tq + (1|multichar_knowresp),
                          data = gnet4s_orig_LMMdataTEST,
                          family = binomial)
summary(gnet4s_orig_TLMM_NCA_DCO)
plot_model(gnet4s_orig_TLMM_NCA_DCO, type = "eff")$NCA_DCO
plot_model(gnet4s_orig_TLMM_NCA_DCO, type = "eff")$tq
plot_model(gnet4s_orig_TLMM_NCA_DCO, type = "int")

#################################

tab_model(gnet4s_orig_TLMM,gnet4s_orig_TLMM_NCA_DCO,
          # = c("Model 1", "Model 2", "Model 3", "Model 4"),
          show.est = TRUE,
          # show.ci = TRUE,
          show.se = TRUE,
          show.r2 = TRUE,
          # show.adj.icc = TRUE,
          # show.icc = FALSE,
          # pred.labels =
          show.aic = TRUE,
          show.aicc = TRUE,
          # show.std = TRUE,
          transform = NULL)
