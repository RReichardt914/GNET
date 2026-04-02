gnet4s_orig_LMMdataTEST<- gnet4s_orig_LMMdataTEST[,c("count_test_response","date_startdate","date_starttime","multichar_knowresp",
                                                       "p2dur","response_test_response","response_time_test_response",
                                                       "rtnr","stim","type","tq","DTF",
                                                       "ColDTF","ShaDTF","ArrDTF","DTAF","ColDTAF",
                                                       "ShaDTAF","ArrDTAF","DTAN","ColDTAN","ShaDTAN",
                                                       "ArrDTAN","DTN_byPhase","ColDTN_bP","ShaDTN_bP","ArrDTN_bP",
                                                       "phase","DTN","TESTRESP")]

gnet4s_delay_LMMdataTEST<- gnet4s_delay_LMMdataTEST[,c("count_test_response","date_startdate","date_starttime","multichar_knowresp",
                                                       "p2dur","response_test_response","response_time_test_response",
                                                       "rtnr","stim","type","tq","DTF",
                                                       "ColDTF","ShaDTF","ArrDTF","DTAF","ColDTAF",
                                                       "ShaDTAF","ArrDTAF","DTAN","ColDTAN","ShaDTAN",
                                                       "ArrDTAN","DTN_byPhase","ColDTN_bP","ShaDTN_bP","ArrDTN_bP",
                                                       "phase","DTN","TESTRESP")]

gnet4s_orig_LMMdataTEST$group <- "orig"
gnet4s_delay_LMMdataTEST$group <- "delay"

gnet4s_ovsd_LMMdataTEST <- rbind(gnet4s_orig_LMMdataTEST,gnet4s_delay_LMMdataTEST)

gnet4s_ovsd_TLMM <- glmer(TESTRESP ~ DTF * group + (1|multichar_knowresp),
                          data = gnet4s_ovsd_LMMdataTEST[tq == "old",],
                          family = binomial,
                          control = lmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8, maxeval=100000)))
summary(gnet4s_ovsd_TLMM)
plot_model(gnet4s_ovsd_TLMM, type = "eff")$DTF
plot_model(gnet4s_ovsd_TLMM, type = "eff")$tq
plot_model(gnet4s_ovsd_TLMM, type = "int")
