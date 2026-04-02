library(data.table)
library(stringr)
picdat <- as.data.table(read.csv("picturedata.csv"))

# Comp to a Single Prototype ####
# we calculate the differences to the prototypes
difftoprot <- data.table(pict = character(),
                         dsum = numeric())
for (i in 1:10){
  prot_arr <- picdat[pict == paste("prototype",i,sep = ""),]
  prot_col <- picdat[pict == paste("prototype",i,"col",sep = ""),]
  prot_sha <- picdat[pict == paste("prototype",i,"sha",sep = ""),]
  
  cpdat <- picdat[grep(paste("p",i,"d",sep=""), picdat$pict),]
  
  for (j in 0:((nrow(cpdat)/3)-1)){
    compname <- as.character(cpdat$pict[3*j+1])
    comp_arr <- cpdat[3*j+1,]
    comp_col <- cpdat[3*j+2,]
    comp_sha <- cpdat[3*j+3,]
    
    coldiff <- prot_col[,2:10] - comp_col[,2:10]
    cd <- table(coldiff[,coldiff != 0])["TRUE"][[1]]
    if (is.na(cd)){cd <- 0}
    shadiff <- prot_sha[,2:10] - comp_sha[,2:10]
    sd <- table(shadiff[,shadiff != 0])["TRUE"][[1]]
    if (is.na(sd)){sd <- 0}
    dsum <- cd + sd
    arrdiff <- prot_arr[,2:10] - comp_arr[,2:10]
    ad <- table(arrdiff[,arrdiff != 0])["TRUE"][[1]]
    if (is.na(ad)){ad <- 0}
    dsum <- dsum + ad
    difftoprot <- rbind(difftoprot, list(compname, dsum))
  }
}

# we have to calculate the diffarr pictures differently
difftoprotda <- data.table(pict = character(),dsum = numeric())
for (i in 1:5){
  prot_arr <- picdat[pict == paste("prototype",i,sep = ""),]
  prot_col <- picdat[pict == paste("prototype",i,"col",sep = ""),]
  prot_sha <- picdat[pict == paste("prototype",i,"sha",sep = ""),]
  
  cpdat <- picdat[grep(paste("p",i+5,"d",sep=""), picdat$pict),]
  for (j in 0:((nrow(cpdat)/3)-1)){
    compname <- as.character(cpdat$pict[3*j+1])
    comp_arr <- cpdat[3*j+1,]
    comp_col <- cpdat[3*j+2,]
    comp_sha <- cpdat[3*j+3,]
    
    coldiff <- prot_col[,2:10] - comp_col[,2:10]
    cd <- table(coldiff[,coldiff != 0])["TRUE"][[1]]
    if (is.na(cd)){cd <- 0}
    shadiff <- prot_sha[,2:10] - comp_sha[,2:10]
    sd <- table(shadiff[,shadiff != 0])["TRUE"][[1]]
    if (is.na(sd)){sd <- 0}
    dsum <- cd + sd
    arrdiff <- prot_arr[,2:10] - comp_arr[,2:10]
    ad <- table(arrdiff[,arrdiff != 0])["TRUE"][[1]]
    if (is.na(ad)){ad <- 0}
    dsum <- dsum + ad
    difftoprotda <- rbind(difftoprotda, list(compname, dsum))
  }
}
difftoprot <- rbind(difftoprot[1:80], difftoprotda)

difftoprot$type <- ""
patts <- c("p6","p7","p8","p9","p10")
difftoprot[str_detect(pict, paste(patts, collapse = '|')), type := "D5"]
difftoprot[str_detect(pict, "p[1-5]d4"), type := "D4"]
difftoprot[str_detect(pict, "p[1-5]d3"), type := "D3"]
difftoprot[str_detect(pict, "p[1-5]d2"), type := "D2"]
difftoprot[str_detect(pict, "p[1-5]d1"), type := "D1"]

#####
# difftoprot contains the values


# Comp to all Prototypes ####
# we calculate the difference of a picture from all prototypes and take the mean
difftoprots <- data.table(pict = character(),
                          dsum = numeric(),
                          dss  = numeric())
# create dts containing the arrangement color and shape of all prototypes
prot_arr <- picdat[pict == "prototype1",]
prot_col <- picdat[pict == "prototype1col",]
prot_sha <- picdat[pict == "prototype1sha",]
for (i in 2:5){
  prot_arr <- rbind(prot_arr, picdat[pict == paste("prototype",i,sep = ""),])
  prot_col <- rbind(prot_col, picdat[pict == paste("prototype",i,"col",sep = ""),])
  prot_sha <- rbind(prot_sha, picdat[pict == paste("prototype",i,"sha",sep = ""),])
}
# compare each picture to the five prototypes  
for (j in 0:((nrow(picdat)/3)-1)){
  compname <- as.character(picdat$pict[3*j+1])
  comp_arr <- picdat[3*j+1,]
  comp_col <- picdat[3*j+2,]
  comp_sha <- picdat[3*j+3,]
  diffs <- c(0,0,0,0,0)
  for (i in 1:5){
    # calculate diff in arrangement
    arrdiff <- prot_arr[i,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)] - comp_arr[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)]
    ad <- table(arrdiff != 0)["TRUE"][[1]]
    if (is.na(ad)){ad <- 0}
    # calc diff in color
    coldiff <- prot_col[i,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)] - comp_col[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)]
    cd <- table(coldiff != 0)["TRUE"][[1]]
    if (is.na(cd)){cd <- 0}
    # calc diff in shape
    shadiff <- prot_sha[i,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)] - comp_sha[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)]
    sd <- table(shadiff != 0)["TRUE"][[1]]
    if (is.na(sd)){sd <- 0}
    # summarize differences
    dsum <- ad + cd + sd
    # fill a vector containing the differences to each prototype
    diffs[i] <- dsum
  }
  difftoprots <- rbind(difftoprots, list(compname, mean(diffs), sum(diffs)))
}

difftoprots$type <- ""
patts <- c("p6","p7","p8","p9","p10")
difftoprots[str_detect(pict, paste(patts, collapse = '|')), type := "D5"]
difftoprots[str_detect(pict, "p[1-5]d4"), type := "D4"]
difftoprots[str_detect(pict, "p[1-5]d3"), type := "D3"]
difftoprots[str_detect(pict, "p[1-5]d2"), type := "D2"]
difftoprots[str_detect(pict, "p[1-5]d1"), type := "D1"]

#####
# difftoprots contains the values


# Calculate Novel Pict Categories Internal Differences ####

diffinnovs <- data.table(pict = character(),
                         dmean = numeric(),
                         dalls  = numeric())
# create dts containing the arrangement color and shape of all prototypes
# diffarr
patts <- c("p6","p7","p8","p9","p10")
D5 <- picdat[str_detect(pict, paste(patts, collapse = '|'))]
# sarr
D4 <- picdat[str_detect(pict, "p[1-5]d4")]
# sarrcol
D3 <- picdat[str_detect(pict, "p[1-5]d3")]
# sarrsha
D2 <- picdat[str_detect(pict, "p[1-5]d2")]
# sarrshacol
D1 <- picdat[str_detect(pict, "p[1-5]d1")]
# calculate all
novcats <- c("D5", "D4", "D3", "D2", "D1")
for (i in 1:length(novcats)){
  whichcat <- novcats[i]
  for (j in 0:((nrow(eval(parse(text=paste(whichcat,sep=""))))/3)-1)){
    compname <- as.character(eval(parse(text=paste(whichcat,sep="")))$pict[3*j+1])
    eval(parse(text=paste(compname," <- c()",sep="")))
    comp_arr <- eval(parse(text=paste(whichcat,sep="")))[3*j+1,]
    comp_col <- eval(parse(text=paste(whichcat,sep="")))[3*j+2,]
    comp_sha <- eval(parse(text=paste(whichcat,sep="")))[3*j+3,]
    for (k in 0:((nrow(eval(parse(text=paste(whichcat,sep=""))))/3)-1)){
      cname <- as.character(eval(parse(text=paste(whichcat,sep="")))$pict[3*k+1])
      c_arr <- eval(parse(text=paste(whichcat,sep="")))[3*k+1,]
      c_col <- eval(parse(text=paste(whichcat,sep="")))[3*k+2,]
      c_sha <- eval(parse(text=paste(whichcat,sep="")))[3*k+3,]
      if (compname != cname){
        arrdiff <- comp_arr[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)] - c_arr[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)]
        ad <- table(arrdiff != 0)["TRUE"][[1]]
        if (is.na(ad)){ad <- 0}
        # calc diff in color
        coldiff <- comp_col[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)] - c_col[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)]
        cd <- table(coldiff != 0)["TRUE"][[1]]
        if (is.na(cd)){cd <- 0}
        # calc diff in shape
        shadiff <- comp_sha[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)] - c_sha[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)]
        sd <- table(shadiff != 0)["TRUE"][[1]]
        if (is.na(sd)){sd <- 0}
        # summarize differences
        dsum <- ad + cd + sd
        # fill a vector containing the differences to each prototype
        eval(parse(text=paste(compname," <- c(",compname,",dsum)",sep="")))
      }
    }
    eval(parse(text=paste("diffinnovs <- rbind(diffinnovs,list(compname,mean(",compname,"),sum(",compname,")))",sep="")))
  }
}

diffinnovs$type <- ""
patts <- c("p6","p7","p8","p9","p10")
diffinnovs[str_detect(pict, paste(patts, collapse = '|')), type := "D5"]
diffinnovs[str_detect(pict, "p[1-5]d4"), type := "D4"]
diffinnovs[str_detect(pict, "p[1-5]d3"), type := "D3"]
diffinnovs[str_detect(pict, "p[1-5]d2"), type := "D2"]
diffinnovs[str_detect(pict, "p[1-5]d1"), type := "D1"]

#####
# output data table : diffinnovs


# Calculate Novel Pict Categories Internal Differences by study/test phase ####

diffinnovs2 <- data.table(pict = character(),
                         dmean = numeric(),
                         dalls  = numeric())
# create dts containing the arrangement color and shape of all novels shown during study
# diffarr
patts <- c("p6","p7","p8","p9","p10")
D5 <- picdat[str_detect(pict, paste(patts, collapse = '|'))]
D5 <- D5[str_detect(pict, "d4_[1-2]")]
# sarr
D4 <- picdat[str_detect(pict, "p[1-5]d4_[1-2]")]
# sarrcol
D3 <- picdat[str_detect(pict, "p[1-5]d3_[1-2]")]
# sarrsha
D2 <- picdat[str_detect(pict, "p[1-5]d2_[1-2]")]
# sarrshacol
D1 <- picdat[str_detect(pict, "p[1-5]d1_[1-2]")]
# calculate all
novcats <- c("D5", "D4", "D3", "D2", "D1")
for (i in 1:length(novcats)){
  whichcat <- novcats[i]
  for (j in 0:((nrow(eval(parse(text=paste(whichcat,sep=""))))/3)-1)){
    compname <- as.character(eval(parse(text=paste(whichcat,sep="")))$pict[3*j+1])
    eval(parse(text=paste(compname," <- c()",sep="")))
    comp_arr <- eval(parse(text=paste(whichcat,sep="")))[3*j+1,]
    comp_col <- eval(parse(text=paste(whichcat,sep="")))[3*j+2,]
    comp_sha <- eval(parse(text=paste(whichcat,sep="")))[3*j+3,]
    for (k in 0:((nrow(eval(parse(text=paste(whichcat,sep=""))))/3)-1)){
      cname <- as.character(eval(parse(text=paste(whichcat,sep="")))$pict[3*k+1])
      c_arr <- eval(parse(text=paste(whichcat,sep="")))[3*k+1,]
      c_col <- eval(parse(text=paste(whichcat,sep="")))[3*k+2,]
      c_sha <- eval(parse(text=paste(whichcat,sep="")))[3*k+3,]
      if (compname != cname){
        arrdiff <- comp_arr[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)] - c_arr[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)]
        ad <- table(arrdiff != 0)["TRUE"][[1]]
        if (is.na(ad)){ad <- 0}
        # calc diff in color
        coldiff <- comp_col[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)] - c_col[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)]
        cd <- table(coldiff != 0)["TRUE"][[1]]
        if (is.na(cd)){cd <- 0}
        # calc diff in shape
        shadiff <- comp_sha[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)] - c_sha[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)]
        sd <- table(shadiff != 0)["TRUE"][[1]]
        if (is.na(sd)){sd <- 0}
        # summarize differences
        dsum <- ad + cd + sd
        # fill a vector containing the differences to each prototype
        eval(parse(text=paste(compname," <- c(",compname,",dsum)",sep="")))
      }
    }
    eval(parse(text=paste("diffinnovs2 <- rbind(diffinnovs2,list(compname,mean(",compname,"),sum(",compname,")))",sep="")))
  }
}
# do the same for distractors presented during test
# create dts containing the arrangement color and shape of all novels shown during study
# diffarr
patts <- c("p6","p7","p8","p9","p10")
D5 <- picdat[str_detect(pict, paste(patts, collapse = '|'))]
D5 <- D5[str_detect(pict, "d4_[3-4]")]
# sarr
D4 <- picdat[str_detect(pict, "p[1-5]d4_[3-4]")]
# sarrcol
D3 <- picdat[str_detect(pict, "p[1-5]d3_[3-4]")]
# sarrsha
D2 <- picdat[str_detect(pict, "p[1-5]d2_[3-4]")]
# sarrshacol
D1 <- picdat[str_detect(pict, "p[1-5]d1_[3-4]")]
# calculate all
novcats <- c("D5", "D4", "D3", "D2", "D1")
for (i in 1:length(novcats)){
  whichcat <- novcats[i]
  for (j in 0:((nrow(eval(parse(text=paste(whichcat,sep=""))))/3)-1)){
    compname <- as.character(eval(parse(text=paste(whichcat,sep="")))$pict[3*j+1])
    eval(parse(text=paste(compname," <- c()",sep="")))
    comp_arr <- eval(parse(text=paste(whichcat,sep="")))[3*j+1,]
    comp_col <- eval(parse(text=paste(whichcat,sep="")))[3*j+2,]
    comp_sha <- eval(parse(text=paste(whichcat,sep="")))[3*j+3,]
    for (k in 0:((nrow(eval(parse(text=paste(whichcat,sep=""))))/3)-1)){
      cname <- as.character(eval(parse(text=paste(whichcat,sep="")))$pict[3*k+1])
      c_arr <- eval(parse(text=paste(whichcat,sep="")))[3*k+1,]
      c_col <- eval(parse(text=paste(whichcat,sep="")))[3*k+2,]
      c_sha <- eval(parse(text=paste(whichcat,sep="")))[3*k+3,]
      if (compname != cname){
        arrdiff <- comp_arr[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)] - c_arr[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)]
        ad <- table(arrdiff != 0)["TRUE"][[1]]
        if (is.na(ad)){ad <- 0}
        # calc diff in color
        coldiff <- comp_col[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)] - c_col[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)]
        cd <- table(coldiff != 0)["TRUE"][[1]]
        if (is.na(cd)){cd <- 0}
        # calc diff in shape
        shadiff <- comp_sha[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)] - c_sha[,c(V1,V2,V3,V4,V5,V6,V7,V8,V9)]
        sd <- table(shadiff != 0)["TRUE"][[1]]
        if (is.na(sd)){sd <- 0}
        # summarize differences
        dsum <- ad + cd + sd
        # fill a vector containing the differences to each prototype
        eval(parse(text=paste(compname," <- c(",compname,",dsum)",sep="")))
      }
    }
    eval(parse(text=paste("diffinnovs2 <- rbind(diffinnovs2,list(compname,mean(",compname,"),sum(",compname,")))",sep="")))
  }
}

diffinnovs2$type <- ""
patts <- c("p6","p7","p8","p9","p10")
diffinnovs2[str_detect(pict, paste(patts, collapse = '|')), type := "D5"]
diffinnovs2[str_detect(pict, "p[1-5]d4"), type := "D4"]
diffinnovs2[str_detect(pict, "p[1-5]d3"), type := "D3"]
diffinnovs2[str_detect(pict, "p[1-5]d2"), type := "D2"]
diffinnovs2[str_detect(pict, "p[1-5]d1"), type := "D1"]

diffinnovs2$phase <- ""
diffinnovs2[str_detect(pict, "d4_[1-2]") & type == "D5", phase := "study"]
diffinnovs2[str_detect(pict, "p[1-5]d4_[1-2]"), phase := "study"]
diffinnovs2[str_detect(pict, "p[1-5]d3_[1-2]"), phase := "study"]
diffinnovs2[str_detect(pict, "p[1-5]d2_[1-2]"), phase := "study"]
diffinnovs2[str_detect(pict, "p[1-5]d1_[1-2]"), phase := "study"]

diffinnovs2[str_detect(pict, "d4_[3-4]") & type == "D5", phase := "test"]
diffinnovs2[str_detect(pict, "p[1-5]d4_[3-4]"), phase := "test"]
diffinnovs2[str_detect(pict, "p[1-5]d3_[3-4]"), phase := "test"]
diffinnovs2[str_detect(pict, "p[1-5]d2_[3-4]"), phase := "test"]
diffinnovs2[str_detect(pict, "p[1-5]d1_[3-4]"), phase := "test"]

#####
# output data table : diffinnovs2


# Tidy up these DTs and make one out of them ####

colnames(difftoprot) <- c("pict","DTF","type")
colnames(difftoprots) <- c("pict","DTAF","dss","type")
difftoprots$dss <- NULL
difference_indices <- merge(difftoprot, difftoprots, by=c("pict","type"))
colnames(diffinnovs) <- c("pict","DTAN","dalls","type" )
diffinnovs$dalls <- NULL
difference_indices <- merge(difference_indices, diffinnovs, by=c("pict","type"))
colnames(diffinnovs2) <- c("pict","DTN_byPhase","dalls","type","phase")
diffinnovs2$dalls <- NULL
difference_indices <- merge(difference_indices, diffinnovs2, by=c("pict","type"))
#####
# difference_indices is the output