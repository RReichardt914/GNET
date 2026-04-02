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
  
  cpdat <- picdat[grep(paste("p",i,"s",sep=""), picdat$pict),]
  
  for (j in 0:((nrow(cpdat)/3)-1)){
    compname <- as.character(cpdat$pict[3*j+1])
    comp_arr <- cpdat[3*j+1,]
    comp_col <- cpdat[3*j+2,]
    comp_sha <- cpdat[3*j+3,]
    
    coldiff <- prot_col - comp_col
    cd <- table(coldiff[,coldiff != 0])["TRUE"][[1]]
    if (is.na(cd)){cd <- 0}
    shadiff <- prot_sha - comp_sha
    sd <- table(shadiff[,shadiff != 0])["TRUE"][[1]]
    if (is.na(sd)){sd <- 0}
    dsum <- cd + sd
    arrdiff <- prot_arr - comp_arr
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
  
  cpdat <- picdat[grep(paste("p",i+5,"s",sep=""), picdat$pict),]
  for (j in 0:((nrow(cpdat)/3)-1)){
    compname <- as.character(cpdat$pict[3*j+1])
    comp_arr <- cpdat[3*j+1,]
    comp_col <- cpdat[3*j+2,]
    comp_sha <- cpdat[3*j+3,]
    
    coldiff <- prot_col - comp_col
    cd <- table(coldiff[,coldiff != 0])["TRUE"][[1]]
    if (is.na(cd)){cd <- 0}
    shadiff <- prot_sha - comp_sha
    sd <- table(shadiff[,shadiff != 0])["TRUE"][[1]]
    if (is.na(sd)){sd <- 0}
    dsum <- cd + sd
    arrdiff <- prot_arr - comp_arr
    ad <- table(arrdiff[,arrdiff != 0])["TRUE"][[1]]
    if (is.na(ad)){ad <- 0}
    dsum <- dsum + ad
    difftoprotda <- rbind(difftoprotda, list(compname, dsum))
  }
}
difftoprot <- rbind(difftoprot[1:80], difftoprotda)

difftoprot$type <- ""
patts <- c("p6","p7","p8","p9","p10")
difftoprot[str_detect(pict, paste(patts, collapse = '|')), type := "diffarr"]
difftoprot[str_detect(pict, "p[1-5]sarr[1-4]"), type := "sarr"]
difftoprot[str_detect(pict, "p[1-5]sarrcol[1-4]"), type := "sarrcol"]
difftoprot[str_detect(pict, "p[1-5]sarrsha[1-4]"), type := "sarrsha"]
difftoprot[str_detect(pict, "p[1-5]sarrshacol[1-4]"), type := "sarrshacol"]

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
difftoprots[str_detect(pict, paste(patts, collapse = '|')), type := "diffarr"]
difftoprots[str_detect(pict, "p[1-5]sarr[1-4]"), type := "sarr"]
difftoprots[str_detect(pict, "p[1-5]sarrcol[1-4]"), type := "sarrcol"]
difftoprots[str_detect(pict, "p[1-5]sarrsha[1-4]"), type := "sarrsha"]
difftoprots[str_detect(pict, "p[1-5]sarrshacol[1-4]"), type := "sarrshacol"]

#####
# difftoprots contains the values


# Calculate Novel Pict Categories Internal Differences ####

diffinnovs <- data.table(pict = character(),
                         dmean = numeric(),
                         dalls  = numeric())
# create dts containing the arrangement color and shape of all prototypes
# diffarr
patts <- c("p6","p7","p8","p9","p10")
diffarr <- picdat[str_detect(pict, paste(patts, collapse = '|'))]
# sarr
sarr <- picdat[str_detect(pict, "p[1-5]sarr[1-4]")]
# sarrcol
sarrcol <- picdat[str_detect(pict, "p[1-5]sarrcol[1-4]")]
# sarrsha
sarrsha <- picdat[str_detect(pict, "p[1-5]sarrsha[1-4]")]
# sarrshacol
sarrshacol <- picdat[str_detect(pict, "p[1-5]sarrshacol[1-4]")]
# calculate all
novcats <- c("diffarr", "sarr", "sarrcol", "sarrsha", "sarrshacol")
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
diffinnovs[str_detect(pict, paste(patts, collapse = '|')), type := "diffarr"]
diffinnovs[str_detect(pict, "p[1-5]sarr[1-4]"), type := "sarr"]
diffinnovs[str_detect(pict, "p[1-5]sarrcol[1-4]"), type := "sarrcol"]
diffinnovs[str_detect(pict, "p[1-5]sarrsha[1-4]"), type := "sarrsha"]
diffinnovs[str_detect(pict, "p[1-5]sarrshacol[1-4]"), type := "sarrshacol"]

#####
# output data table : diffinnovs


# Calculate Novel Pict Categories Internal Differences by study/test phase ####

diffinnovs2 <- data.table(pict = character(),
                         dmean = numeric(),
                         dalls  = numeric())
# create dts containing the arrangement color and shape of all novels shown during study
# diffarr
patts <- c("p6","p7","p8","p9","p10")
diffarr <- picdat[str_detect(pict, paste(patts, collapse = '|'))]
diffarr <- diffarr[str_detect(pict, "sarr[1-2]")]
# sarr
sarr <- picdat[str_detect(pict, "p[1-5]sarr[1-2]")]
# sarrcol
sarrcol <- picdat[str_detect(pict, "p[1-5]sarrcol[1-2]")]
# sarrsha
sarrsha <- picdat[str_detect(pict, "p[1-5]sarrsha[1-2]")]
# sarrshacol
sarrshacol <- picdat[str_detect(pict, "p[1-5]sarrshacol[1-2]")]
# calculate all
novcats <- c("diffarr", "sarr", "sarrcol", "sarrsha", "sarrshacol")
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
diffarr <- picdat[str_detect(pict, paste(patts, collapse = '|'))]
diffarr <- diffarr[str_detect(pict, "sarr[3-4]")]
# sarr
sarr <- picdat[str_detect(pict, "p[1-5]sarr[3-4]")]
# sarrcol
sarrcol <- picdat[str_detect(pict, "p[1-5]sarrcol[3-4]")]
# sarrsha
sarrsha <- picdat[str_detect(pict, "p[1-5]sarrsha[3-4]")]
# sarrshacol
sarrshacol <- picdat[str_detect(pict, "p[1-5]sarrshacol[3-4]")]
# calculate all
novcats <- c("diffarr", "sarr", "sarrcol", "sarrsha", "sarrshacol")
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
diffinnovs2[str_detect(pict, paste(patts, collapse = '|')), type := "diffarr"]
diffinnovs2[str_detect(pict, "p[1-5]sarr[1-4]"), type := "sarr"]
diffinnovs2[str_detect(pict, "p[1-5]sarrcol[1-4]"), type := "sarrcol"]
diffinnovs2[str_detect(pict, "p[1-5]sarrsha[1-4]"), type := "sarrsha"]
diffinnovs2[str_detect(pict, "p[1-5]sarrshacol[1-4]"), type := "sarrshacol"]

diffinnovs2$phase <- ""
diffinnovs2[str_detect(pict, "sarr[1-2]") & type == "diffarr", phase := "study"]
diffinnovs2[str_detect(pict, "p[1-5]sarr[1-2]"), phase := "study"]
diffinnovs2[str_detect(pict, "p[1-5]sarrcol[1-2]"), phase := "study"]
diffinnovs2[str_detect(pict, "p[1-5]sarrsha[1-2]"), phase := "study"]
diffinnovs2[str_detect(pict, "p[1-5]sarrshacol[1-2]"), phase := "study"]

diffinnovs2[str_detect(pict, "sarr[3-4]") & type == "diffarr", phase := "test"]
diffinnovs2[str_detect(pict, "p[1-5]sarr[3-4]"), phase := "test"]
diffinnovs2[str_detect(pict, "p[1-5]sarrcol[3-4]"), phase := "test"]
diffinnovs2[str_detect(pict, "p[1-5]sarrsha[3-4]"), phase := "test"]
diffinnovs2[str_detect(pict, "p[1-5]sarrshacol[3-4]"), phase := "test"]

#####
# output data table : diffinnovs2


# Calculate Novel Pict Differences ####

diffinnovs3 <- data.table(pict = character(),
                          dmean = numeric(),
                          dalls  = numeric())
# create dts containing the arrangement color and shape of all prototypes
# diffarr
patts <- c("p6","p7","p8","p9","p10")
diffarr <- picdat[str_detect(pict, paste(patts, collapse = '|'))]
# sarr
sarr <- picdat[str_detect(pict, "p[1-5]sarr[1-4]")]
# sarrcol
sarrcol <- picdat[str_detect(pict, "p[1-5]sarrcol[1-4]")]
# sarrsha
sarrsha <- picdat[str_detect(pict, "p[1-5]sarrsha[1-4]")]
# sarrshacol
sarrshacol <- picdat[str_detect(pict, "p[1-5]sarrshacol[1-4]")]
# calculate all
novcats <- c("diffarr", "sarr", "sarrcol", "sarrsha", "sarrshacol")
for (i in 1:length(novcats)){
  whichcat <- novcats[i]
  for (j in 0:((nrow(eval(parse(text=paste(whichcat,sep=""))))/3)-1)){
    compname <- as.character(eval(parse(text=paste(whichcat,sep="")))$pict[3*j+1])
    eval(parse(text=paste(compname," <- c()",sep="")))
    comp_arr <- eval(parse(text=paste(whichcat,sep="")))[3*j+1,]
    comp_col <- eval(parse(text=paste(whichcat,sep="")))[3*j+2,]
    comp_sha <- eval(parse(text=paste(whichcat,sep="")))[3*j+3,]
    
    for (l in 1:length(novcats)){
      whcat2 <- novcats[l]
      for (k in 0:((nrow(eval(parse(text=paste(whcat2,sep=""))))/3)-1)){
        cname <- as.character(eval(parse(text=paste(whcat2,sep="")))$pict[3*k+1])
        c_arr <- eval(parse(text=paste(whcat2,sep="")))[3*k+1,]
        c_col <- eval(parse(text=paste(whcat2,sep="")))[3*k+2,]
        c_sha <- eval(parse(text=paste(whcat2,sep="")))[3*k+3,]
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
    }
    eval(parse(text=paste("diffinnovs3 <- rbind(diffinnovs3,list(compname,mean(",compname,"),sum(",compname,")))",sep="")))
  }
}

diffinnovs3$type <- ""
patts <- c("p6","p7","p8","p9","p10")
diffinnovs3[str_detect(pict, paste(patts, collapse = '|')), type := "diffarr"]
diffinnovs3[str_detect(pict, "p[1-5]sarr[1-4]"), type := "sarr"]
diffinnovs3[str_detect(pict, "p[1-5]sarrcol[1-4]"), type := "sarrcol"]
diffinnovs3[str_detect(pict, "p[1-5]sarrsha[1-4]"), type := "sarrsha"]
diffinnovs3[str_detect(pict, "p[1-5]sarrshacol[1-4]"), type := "sarrshacol"]

#####
# output data table : diffinnovs


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
colnames(diffinnovs3) <- c("pict","DTN","dalls","type")
diffinnovs3$dalls <- NULL
difference_indices <- merge(difference_indices, diffinnovs3, by=c("pict","type"))
#####
# difference_indices is the output

rm(list = ls()[!grepl("difference_indices", ls())])
