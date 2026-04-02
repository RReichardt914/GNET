library(data.table)
library(stringr)
#get RAW data
gnet4s_delay_ST <- read.csv("gnet-4stim-delay_STUDY.csv")
gnet4s_delay_TE <- read.csv("gnet-4stim-delay_TEST.csv")
#choose cols
gnet4s_delay_ST <- gnet4s_delay_ST[,c("count_famrecall","count_prestudy_sequence","count_study_response",
                                      "date_startdate","date_starttime","multichar_knowresp",
                                      "response_study_response","response_time_study_response",
                                      "sessionid","stim")]

gnet4s_delay_TE <- gnet4s_delay_TE[,c("cell1","cell2","cell3","cell4","cell5","cell6",
                                      "cell7","cell8","cell9",
                                      "count_famrecall",
                                      "count_test_response","date_startdate","date_starttime",
                                      "multichar_knowresp","p2dur",
                                      "plc1","plc1c","plc1s","plc1sh","plc1x","plc1y",
                                      "plc2","plc2c","plc2s","plc2sh","plc2x","plc2y",
                                      "plc3","plc3c","plc3s","plc3sh","plc3x","plc3y",
                                      "plc4","plc4c","plc4s","plc4sh","plc4x","plc4y",
                                      "plc5","plc5c","plc5s","plc5sh","plc5x","plc5y",
                                      "plcundefined","plcundefineds","plcundefinedsh","plcundefinedx","plcundefinedy",
                                      "response_test_response","response_time_test_response",
                                      "rtnr","sessionid","stim")]

gnet4s_delay_ST <- as.data.table(gnet4s_delay_ST)
gnet4s_delay_TE <- as.data.table(gnet4s_delay_TE)

# stim category
gnet4s_delay_ST$type <- ""
gnet4s_delay_ST[str_detect(stim,"prototype"),type:="prototype"]
gnet4s_delay_ST[str_detect(stim,"sarr[1-4]"),type:="sarr"]
gnet4s_delay_ST[str_detect(stim,"sarrsha[1-4]"),type:="sarrsha"]
gnet4s_delay_ST[str_detect(stim,"sarrcol[1-4]"),type:="sarrcol"]
gnet4s_delay_ST[str_detect(stim,"sarrshacol[1-4]"),type:="sarrshacol"]
gnet4s_delay_ST[str_detect(stim,"p[6-9]"),type:="diffarr"]
gnet4s_delay_ST[str_detect(stim,"p10"),type:="diffarr"]

# stim category
gnet4s_delay_TE$type <- ""
gnet4s_delay_TE[str_detect(stim,"prototype"),type:="prototype"]
gnet4s_delay_TE[str_detect(stim,"sarr[1-4]"),type:="sarr"]
gnet4s_delay_TE[str_detect(stim,"sarrsha[1-4]"),type:="sarrsha"]
gnet4s_delay_TE[str_detect(stim,"sarrcol[1-4]"),type:="sarrcol"]
gnet4s_delay_TE[str_detect(stim,"sarrshacol[1-4]"),type:="sarrshacol"]
gnet4s_delay_TE[str_detect(stim,"p[6-9]"),type:="diffarr"]
gnet4s_delay_TE[str_detect(stim,"p10"),type:="diffarr"]

# trial quality
gnet4s_delay_TE$tq <- ""
gnet4s_delay_TE[is.na(count_test_response),tq:="study"]
gnet4s_delay_TE[!is.na(count_test_response),tq:="test"]
gnet4s_delay_TE[str_detect(stim,"[1-2].png") & tq == "test",tq:="old"]
gnet4s_delay_TE[str_detect(stim,"[3-4].png") & tq == "test",tq:="new"]

write.csv(gnet4s_delay_ST,"gnet4s_delay-ST-PREPROC.csv")
write.csv(gnet4s_delay_TE,"gnet4s_delay-TE-PREPROC.csv")

