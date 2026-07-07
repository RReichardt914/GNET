library(data.table)
library(stringr)
library(ggplot2)
gnet2_data <- as.data.table(read.csv("gnet_v2_jd_data.csv", encoding = "UTF-8", stringsAsFactors = FALSE))
gnet2_data <- gnet2_data[,c("count_study_sequence","count_test_sequence","datetime",
                            "response","response_study_response","response_test_response",
                            "response_time","response_time_study_response","response_time_test_response",
                            "stim","stim_2","studyResultId","total_responses",
                            "workerId")]
gnet2_data <- gnet2_data[!is.na(workerId),]

gnet2_data[is.na(count_test_sequence) & str_detect(stim_2, "prototype"),stimtype:="prototype"]
gnet2_data[is.na(count_test_sequence) & str_detect(stim_2, "p[1-5]d1"),stimtype:="D1"]
gnet2_data[is.na(count_test_sequence) & str_detect(stim_2, "p[1-5]d2"),stimtype:="D2"]
gnet2_data[is.na(count_test_sequence) & str_detect(stim_2, "p[1-5]d3"),stimtype:="D3"]
gnet2_data[is.na(count_test_sequence) & str_detect(stim_2, "p[1-5]d4"),stimtype:="D4"]
gnet2_data[is.na(count_test_sequence) & str_detect(stim_2, "p[6-9]"),stimtype:="D5"]
gnet2_data[is.na(count_test_sequence) & str_detect(stim_2, "p10"),stimtype:="D5"]

gnet2_data[!is.na(count_test_sequence) & str_detect(stim, "p[1-5]d1"),stimtype:="D1"]
gnet2_data[!is.na(count_test_sequence) & str_detect(stim, "p[1-5]d2"),stimtype:="D2"]
gnet2_data[!is.na(count_test_sequence) & str_detect(stim, "p[1-5]d3"),stimtype:="D3"]
gnet2_data[!is.na(count_test_sequence) & str_detect(stim, "p[1-5]d4"),stimtype:="D4"]
gnet2_data[!is.na(count_test_sequence) & str_detect(stim, "p[6-9]"),stimtype:="D5"]
gnet2_data[!is.na(count_test_sequence) & str_detect(stim, "p10"),stimtype:="D5"]

gnet2_data[str_detect(stim, "_[1-2]") & !(is.na(response_time_test_response)),test_quality := "fam"]
gnet2_data[str_detect(stim, "_[3-4]") & !(is.na(response_time_test_response)),test_quality := "new"]

gnet2_data[str_detect(stim, "_[1-2]") & response_test_response == "left", rt_corr := 1]
gnet2_data[str_detect(stim, "_[1-2]") & response_test_response == "right", rt_corr := 0]
gnet2_data[str_detect(stim, "_[3-4]") & response_test_response == "left", rt_corr := 0]
gnet2_data[str_detect(stim, "_[3-4]") & response_test_response == "right", rt_corr := 1]

# FROM HERE WE USE THE DATA PRODUCED BY: difference_innumbers.R
