gnet4s_artn_LMMdataTEST<- gnet4s_artn_LMMdataTEST[,c("count_test_response","date_startdate","date_starttime","multichar_knowresp",
                                                     "p2dur","response_test_response","response_time_test_response",
                                                     "rtnr","stim","type","tq","DTF",
                                                     "ColDTF","ShaDTF","ArrDTF","DTAF","ColDTAF",
                                                     "ShaDTAF","ArrDTAF","DTAN","ColDTAN","ShaDTAN",
                                                     "ArrDTAN","DTN_byPhase","ColDTN_bP","ShaDTN_bP","ArrDTN_bP",
                                                     "phase","DTN","TESTRESP")]

gnet4s_delart_LMMdataTEST<- gnet4s_delart_LMMdataTEST[,c("count_test_response","date_startdate","date_starttime","multichar_knowresp",
                                                       "p2dur","response_test_response","response_time_test_response",
                                                       "rtnr","stim","type","tq","DTF",
                                                       "ColDTF","ShaDTF","ArrDTF","DTAF","ColDTAF",
                                                       "ShaDTAF","ArrDTAF","DTAN","ColDTAN","ShaDTAN",
                                                       "ArrDTAN","DTN_byPhase","ColDTN_bP","ShaDTN_bP","ArrDTN_bP",
                                                       "phase","DTN","TESTRESP")]

gnet4s_artn_LMMdataTEST$group <- "artn"
gnet4s_delart_LMMdataTEST$group <- "delart"

gnet4s_avsd_LMMdataTEST <- rbind(gnet4s_artn_LMMdataTEST,gnet4s_delart_LMMdataTEST)

gnet4s_avsd_TLMM <- glmer(TESTRESP ~ DTF * tq * group + (1|multichar_knowresp),
                          data = gnet4s_avsd_LMMdataTEST,
                          family = binomial,
                          control = lmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8, maxeval=100000)))
summary(gnet4s_avsd_TLMM)
plot_model(gnet4s_avsd_TLMM, type = "eff")
plot_model(gnet4s_avsd_TLMM, type = "eff")
plot_model(gnet4s_avsd_TLMM, type = "int")
