# this does not need to be repeated
# jatos data takes a long time to get into R

# libraries ---------------------------------------------------------------


library(tidyverse) # some data table functions do not work with this lib
library(stringr)
library(data.table)
library(plyr)


# read data ---------------------------------------------------------------


# get jatos results in r
filek <- list.files()
filek <- filek[grepl("txt",filek, fixed = TRUE)]
sample_data <- readLines(filek)
#sample_data <- gsub(":", ",", sample_data)
# recoding
sample_data <- str_replace_all(sample_data, "Ăˇ", "á")
sample_data <- str_replace_all(sample_data, "Ăł", "ó")
sample_data <- str_replace_all(sample_data, "Ă¶", "ö")
sample_data <- str_replace_all(sample_data, "Ă©", "é")
sample_data <- str_replace_all(sample_data, "Ĺ‘", "ő")
sample_data <- str_replace_all(sample_data, "Ăş", "ú")
sample_data <- str_replace_all(sample_data, "Ĺ±", "ű")
sample_data <- str_replace_all(sample_data, "ĂĽ", "ü")
sample_data <- str_replace_all(sample_data, "Ă", "í")
# get variables and values
masterdt <- data.table(mi = as.character("x"),
                       mennyi = as.character("x"))

for (j in 1:length(sample_data)){
  temp <- str_split(sample_data[j], ",", n = Inf)
  temp <- unlist(temp)
  tempdt <- data.table(mi = as.character("x"),
                       mennyi = as.character("x"))
  for (i in 1:length(temp)){
    mimennyi <- str_split(temp[i], ":", n = Inf)
    mimennyi <- unlist(mimennyi)
    if (length(mimennyi)>1){
      tdtnrow <- data.table(mi = as.character("x"),mennyi = as.character("x"))
      tempdt <- rbind(tempdt,tdtnrow)
      tempdt$mi[nrow(tempdt)-1] <- mimennyi[1]
      tempdt$mennyi[nrow(tempdt)-1] <- mimennyi[2]
    } else {
      tempdt$mennyi[nrow(tempdt)-1] <- str_c(tempdt$mennyi[nrow(tempdt)-1], mimennyi)
    }
  }
  masterdt <- rbind(masterdt, tempdt)
}
# drop meaningless varaiables
probadt <- masterdt
probadt <- probadt[!str_detect(mi,"backend")]
probadt <- probadt[!str_detect(mi,"sound")]
probadt <- probadt[!str_detect(mi,"font")]
probadt <- probadt[!str_detect(mi,"component")]
probadt <- probadt[!str_detect(mi,"background")]
probadt <- probadt[!str_detect(mi,"bidi")]
probadt <- probadt[!str_detect(mi,"compensation")]
probadt <- probadt[!str_detect(mi,"width")]
probadt <- probadt[!str_detect(mi,"coordinates")]
probadt <- probadt[!str_detect(mi,"garbage")]
probadt <- probadt[!str_detect(mi,"path")]
probadt <- probadt[!str_detect(mi,"foreground")]
probadt <- probadt[!str_detect(mi,"study")]
probadt <- probadt[!str_detect(mi,"acc")]
probadt <- probadt[!str_detect(mi,"batchId")]
probadt <- probadt[!str_detect(mi,"batchTitle")]
probadt <- probadt[!str_detect(mi,"correct")]
probadt <- probadt[!str_detect(mi,"count_experiment")]
probadt <- probadt[!str_detect(mi,"topic")]
probadt <- probadt[!str_detect(mi,"erejét")]
probadt <- probadt[!str_detect(mi,"hiszen")]
probadt <- probadt[!str_detect(mi,"fullscreen")]
probadt <- probadt[!str_detect(mi,"height")]
probadt <- probadt[!str_detect(mi,"description")]
probadt <- probadt[!str_detect(mi,"empty_column")]
probadt <- probadt[!str_detect(mi,"groupMember")]
probadt <- probadt[!str_detect(mi,"groupResult")]
probadt <- probadt[!str_detect(mi,"opensesame")]
probadt <- probadt[!str_detect(mi,"subject_parity")]
probadt <- probadt[!str_detect(mi,"system")]
probadt <- probadt[!str_detect(mi,"screen")]
probadt <- probadt[!str_detect(mi,"Depth")]
probadt <- probadt[!str_detect(mi,"pilot")]
probadt <- probadt[!str_detect(mi,"parameters")]
probadt <- probadt[!str_detect(mi,"name")]
probadt <- probadt[!str_detect(mi,"browser")]
probadt <- probadt[!str_detect(mi,"available")]
probadt <- probadt[!str_detect(mi,"version")]
probadt <- probadt[!str_detect(mi,"starttime")]
probadt <- probadt[!str_detect(mi,"startdate")]
probadt <- probadt[!str_detect(mi,"experiment")]

probadt <- probadt[!str_detect(mennyi,"experiment")]
# take out "/ from vars and values
for (i in 1:nrow(probadt)){
  probadt$mi[i] <- gsub('\"', "", probadt$mi[i], fixed = TRUE)
  probadt$mennyi[i] <- gsub('\"', "", probadt$mennyi[i], fixed = TRUE)
}
# reform data
cnam <- unique(probadt$mi)
library(stringi)
for (i in 1:length(cnam)){
  cnam[i] <- gsub('\"', "", cnam[i], fixed = TRUE)
}

p2 <- setNames(data.table(matrix(nrow = 100000, ncol = length(cnam))), cnam)

cval <- replicate(length(cnam), 0)

cline <- 0 # current line
for (i in 1:nrow(probadt)){
  if (probadt$mi[i] == "x"){
    cline <- cline + 1
  }
  for (j in 1:ncol(p2)){
    if (probadt$mi[i] == colnames(p2)[j]){
      eval(parse(text=paste("p2$",probadt$mi[i],"[cline] <- probadt$mennyi[i]", sep="")))
    }
  }
}
# drop some more meaningless vars
p2$x <- NULL
p2$count_osweb <- NULL
p2$count_end <- NULL
p2$average_response_time <- NULL
p2$avg_rt <- NULL
p2$count_start <- NULL
p2$count_test <- NULL
p2$count_test_start <- NULL
p2$form_clicks <- NULL
p2$fampict <- NULL
p2$response_start <- NULL
p2$response_time_start <- NULL
p2$round_decimals <- NULL
p2$sessionid <- NULL
p2$time_start <- NULL
p2$time_test <- NULL
p2$subject_nr <- NULL
p2$title <- NULL
p2$total_response_time <- NULL
p2$count_test_isi <- NULL
p2$count_test_logger <- NULL
p2$count_test_response <- NULL
p2$count_test_sequence <- NULL
p2$count_test_stimpres_2 <- NULL
p2$response_test_start <- NULL
p2$response_time_test_start <- NULL
p2$time_test_isi <- NULL
p2$time_test_logger <- NULL
p2$time_test_response <- NULL
p2$time_test_start <- NULL
p2$time_test_stimpres <- NULL
p2$time_test_stimpres_2 <- NULL
p2$date <- NULL
p2 <- p2[!is.na(p2$datetime),]


# data processing ---------------------------------------------------------



# correct study responses
p2$st_corr <- 0
for (i in 1:nrow(p2)){
  if (is.na(p2$count_test_stimpres[i])){
    if (str_detect(p2$stim[i],"prototype")){
      if (p2$response[i] == "left"){
        p2$st_corr[i] <- 1
      }
    }
  }
}

for (i in 1:nrow(p2)){
  if (is.na(p2$count_test_stimpres[i])){
    if (!str_detect(p2$stim[i],"prototype")){
      if (p2$response[i] == "right"){
        p2$st_corr[i] <- 1
      }
    }
  }
}
# correct test responses
# if stim id ends with 1 or 2 the stim has been seen during study
# ends with 3 or 4 - has never been seen

# factorize stimid
# prototype - familiars
# p1-5sarr1-2
p2$type <- "x" # level of similarity to a prototype
p2$tq <- "x" # test quality as in fam or new
for (i in 1:nrow(p2)){
  x <- str_extract_all(p2$stim[i],"[:digit:]")[[1]]
  if (is.na(p2$count_test_stimpres[i])){
    if (length(x) < 2){
      p2$type[i] <- "prototype"
    } else {
      if (x[1]<6 & length(x)<3){
        y <- str_extract_all(p2$stim[i],"[:alpha:]")[[1]]
        y <- paste(y,collapse="")
        y <- str_sub(y,start = 2, end = -4)
        p2$type[i] <- y
      } else {
        p2$type[i] <- "diffarr"
      }
    }
  }
  if (!is.na(p2$count_test_stimpres[i])){
    if (x[length(x)]>2){
      p2$tq[i] <- "new"
    } else {
      p2$tq[i] <- "fam"
    }
    if (x[1]<6 & length(x)<3){
      y <- str_extract_all(p2$stim[i],"[:alpha:]")[[1]]
      y <- paste(y,collapse="")
      y <- str_sub(y,start = 2, end = -4)
      p2$type[i] <- y
    } else {
      p2$type[i] <- "diffarr"
    }
  }
}

write.csv(p2, "cleandata.csv", row.names = FALSE)