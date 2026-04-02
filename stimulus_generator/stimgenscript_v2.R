require(grDevices)
library(plotrix)
library(data.table)

# koordináták & szinek ----------------------------------------------------------
# kozepso koordinatak
x <- 25
y <- 25
cent_coords_x <- c()
cent_coords_y <- c()
for (i in 1:3){
  for (j in 1:3){
    cent_coords_x <- append(cent_coords_x,x)
    cent_coords_y <- append(cent_coords_y,y)
    x <- x + 50
  }
  y <- y + 50
  x <- 25
}
# szelso koordinatak
x <- 2
y <- 2
corn_coords_x <- c()
corn_coords_y <- c()
for (i in 1:3){
  for (j in 1:3){
    corn_coords_x <- append(corn_coords_x,x)
    corn_coords_y <- append(corn_coords_y,y)
    x <- x + 50
  }
  y <- y + 50
  x <- 2
}
# szinek
szinek <- c("red","blue","green")

# prototípus generalas ----------------------------------------------------

for (z in 1:10){
  pict <- c(0,0,0,0,0,0,0,0,0)
  ok <- FALSE
  while (ok == FALSE){
    while (sum(pict == 0) > 5){
      k <- sample(1:9,1)
      if (pict[k] == 0){
        pict[k] <- 1
      }
    }
    # prototypeX - contains arrangement info
    eval(parse(text=paste("prototype",z," <- pict", sep="")))
    # we need to compare current prototype to the rest, so that arrangements are unique
    if (z > 1){
      check <- 0
      for (j in 2:z){
        eval(parse(text=paste("if (all(prototype",z," == prototype",j-1,")){}else{check <- check + 1}", sep="")))
        if (check == z-1){
          ok <- TRUE
        }
      }
    } else {
      ok <- TRUE
    }
  }
  # prototypeXsha - contains shape info (+arrangement)
  eval(parse(text=paste("prototype",z,"sha <- pict", sep="")))
  for (i in 1:9){
    eval(parse(text=paste("if (prototype",z,"sha[i] > 0){prototype",z,"sha[i] <- sample(1:3,1)}", sep="")))
  }
  # prototypeXcol - contains color info (+arrangement)
  eval(parse(text=paste("prototype",z,"col <- pict", sep="")))
  for (i in 1:9){
    eval(parse(text=paste("if (prototype",z,"col[i] > 0){prototype",z,"col[i] <- sample(1:3,1)}", sep="")))
  }
  # coloring always contains the current col info
  eval(parse(text=paste("coloring <- prototype",z,"col", sep="")))
  # pict should now take on the data from shape info
  eval(parse(text=paste("pict <- prototype",z,"sha", sep="")))
  # generate prototype pics
  eval(parse(text=paste("png('prototype",z,".png')", sep="")))
  plot(c(0, 150), c(0, 150), type = "n", xlab = "", ylab = "", main = "", axes=FALSE)
  for (i in 1:9){
    if (pict[i] == 1){
      rect(corn_coords_x[i], corn_coords_y[i], corn_coords_x[i]+46, corn_coords_y[i]+46, col = szinek[coloring[i]], lwd = 2)
    }
    if (pict[i] == 2){
      polygon(c(corn_coords_x[i],corn_coords_x[i]+23,corn_coords_x[i]+46,corn_coords_x[i]),
              c(corn_coords_y[i],corn_coords_y[i]+46, corn_coords_y[i], corn_coords_y[i]),
              xpd = TRUE, col = szinek[coloring[i]], lty = 1, lwd = 2, border = "black")
    }
    if (pict[i] == 3){
      draw.circle(cent_coords_x[i],cent_coords_y[i], radius = 21, col = szinek[coloring[i]], border="black",lty=1,lwd=2)
    }
  }
  dev.off()
}


# focsoport ingerek -------------------------------------------------------
difflev <- c("d1_","d2_","d3_","d4_")
for (z in 1:5){
  for (y in 1:4){
    # csak egy ponton tér el az eredetitől (1 alakzat és szín különbség)
    for (i in 1:4){
      # arrangement is from prototypeX
      eval(parse(text=paste("p",z,difflev[y],i," <- prototype",z, sep="")))
      
      # the stimuli could theoretically be the same if generated this way so we make sure that
      # does not happen
      ok <- FALSE
      while (ok == FALSE){
        # get shape and color from prototype
        eval(parse(text=paste("p",z,difflev[y],i,"sha <- prototype",z,"sha", sep="")))
        eval(parse(text=paste("p",z,difflev[y],i,"col <- prototype",z,"col", sep="")))
        eval(parse(text=paste("helyek <- which(prototype",z," == 1)", sep="")))
        valt <- sample(helyek,y)
        while (eval(parse(text=paste("p",z,difflev[y],i,"sha[valt] == ",0, sep="")))){
          valt <- sample(helyek,y)
        }
        for (x in 1:length(valt)){
          egyketha <- c(1,2,3)
          eval(parse(text=paste("egyketha <- egyketha[!egyketha == p",z,difflev[y],i,"sha[valt[x]]]",sep="")))
          eval(parse(text=paste("p",z,difflev[y],i,"sha[valt[x]] <- sample(egyketha,1)", sep="")))
          egyketha <- c(1,2,3)
          eval(parse(text=paste("egyketha <- egyketha[!egyketha == p",z,difflev[y],i,"col[valt[x]]]",sep="")))
          eval(parse(text=paste("p",z,difflev[y],i,"col[valt[x]] <- sample(egyketha,1)", sep="")))
          egyketha <- c(1,2,3)
        }
        
        # compare new stim to the rest
        check <- 1
        for (j in 1:i){
          if (j != 1){
            eval(parse(text=paste("if (all(p",z,difflev[y],i,"sha == p",z,difflev[y],j-1,"sha)){}else{check <- check + 0.5}", sep="")))
            eval(parse(text=paste("if (all(p",z,difflev[y],i,"col == p",z,difflev[y],j-1,"col)){}else{check <- check + 0.5}", sep="")))
            if (check %% 1 > 0){
              check <- check + 0.5
            }
          }
          if (check == i){
            ok <- TRUE
          }
        }
      }
      # generate sarr pict
      # coloring always contains the current col info
      eval(parse(text=paste("coloring <- p",z,difflev[y],i,"col", sep="")))
      # pict should now take on the data from shape info
      eval(parse(text=paste("pict <- p",z,difflev[y],i,"sha", sep="")))
      eval(parse(text=paste("png('p",z,difflev[y],i,".png')", sep="")))
      plot(c(0, 150), c(0, 150), type = "n", xlab = "", ylab = "", main = "", axes=FALSE)
      for (i in 1:9){
        if (pict[i] == 1){
          rect(corn_coords_x[i], corn_coords_y[i], corn_coords_x[i]+46, corn_coords_y[i]+46, col = szinek[coloring[i]], lwd = 2)
        }
        if (pict[i] == 2){
          polygon(c(corn_coords_x[i],corn_coords_x[i]+23,corn_coords_x[i]+46,corn_coords_x[i]),
                  c(corn_coords_y[i],corn_coords_y[i]+46, corn_coords_y[i], corn_coords_y[i]),
                  xpd = TRUE, col = szinek[coloring[i]], lty = 1, lwd = 2, border = "black")
        }
        if (pict[i] == 3){
          draw.circle(cent_coords_x[i],cent_coords_y[i], radius = 21, col = szinek[coloring[i]], border="black",lty=1,lwd=2)
        }
      }
      dev.off()
    }
  }
}


# mellekcsoport ingerek ---------------------------------------------------

difflev <- c("d1_","d2_","d3_","d4_")
for (z in 6:10){
  for (y in 4:4){
    # csak egy ponton tér el az eredetitől (1 alakzat és szín különbség)
    for (i in 1:4){
      # arrangement is from prototypeX
      eval(parse(text=paste("p",z,difflev[y],i," <- prototype",z, sep="")))
      
      # the stimuli could theoretically be the same if generated this way so we make sure that
      # does not happen
      ok <- FALSE
      while (ok == FALSE){
        # get shape and color from prototype
        eval(parse(text=paste("p",z,difflev[y],i,"sha <- prototype",z,"sha", sep="")))
        eval(parse(text=paste("p",z,difflev[y],i,"col <- prototype",z,"col", sep="")))
        eval(parse(text=paste("helyek <- which(prototype",z," == 1)", sep="")))
        valt <- sample(helyek,y)
        while (eval(parse(text=paste("p",z,difflev[y],i,"sha[valt] == ",0, sep="")))){
          valt <- sample(helyek,y)
        }
        for (x in 1:length(valt)){
          egyketha <- c(1,2,3)
          eval(parse(text=paste("egyketha <- egyketha[!egyketha == p",z,difflev[y],i,"sha[valt[x]]]",sep="")))
          eval(parse(text=paste("p",z,difflev[y],i,"sha[valt[x]] <- sample(egyketha,1)", sep="")))
          egyketha <- c(1,2,3)
          eval(parse(text=paste("egyketha <- egyketha[!egyketha == p",z,difflev[y],i,"col[valt[x]]]",sep="")))
          eval(parse(text=paste("p",z,difflev[y],i,"col[valt[x]] <- sample(egyketha,1)", sep="")))
          egyketha <- c(1,2,3)
        }
        
        # compare new stim to the rest
        check <- 1
        for (j in 1:i){
          if (j != 1){
            eval(parse(text=paste("if (all(p",z,difflev[y],i,"sha == p",z,difflev[y],j-1,"sha)){}else{check <- check + 0.5}", sep="")))
            eval(parse(text=paste("if (all(p",z,difflev[y],i,"col == p",z,difflev[y],j-1,"col)){}else{check <- check + 0.5}", sep="")))
            if (check %% 1 > 0){
              check <- check + 0.5
            }
          }
          if (check == i){
            ok <- TRUE
          }
        }
      }
      # generate sarr pict
      # coloring always contains the current col info
      eval(parse(text=paste("coloring <- p",z,difflev[y],i,"col", sep="")))
      # pict should now take on the data from shape info
      eval(parse(text=paste("pict <- p",z,difflev[y],i,"sha", sep="")))
      eval(parse(text=paste("png('p",z,difflev[y],i,".png')", sep="")))
      plot(c(0, 150), c(0, 150), type = "n", xlab = "", ylab = "", main = "", axes=FALSE)
      for (i in 1:9){
        if (pict[i] == 1){
          rect(corn_coords_x[i], corn_coords_y[i], corn_coords_x[i]+46, corn_coords_y[i]+46, col = szinek[coloring[i]], lwd = 2)
        }
        if (pict[i] == 2){
          polygon(c(corn_coords_x[i],corn_coords_x[i]+23,corn_coords_x[i]+46,corn_coords_x[i]),
                  c(corn_coords_y[i],corn_coords_y[i]+46, corn_coords_y[i], corn_coords_y[i]),
                  xpd = TRUE, col = szinek[coloring[i]], lty = 1, lwd = 2, border = "black")
        }
        if (pict[i] == 3){
          draw.circle(cent_coords_x[i],cent_coords_y[i], radius = 21, col = szinek[coloring[i]], border="black",lty=1,lwd=2)
        }
      }
      dev.off()
    }
  }
}

# stimdata to csv ---------------------------------------------------------
# create a csv which contains all the data of the pictures

remove(cent_coords_x, cent_coords_y, corn_coords_x, corn_coords_y, check, coloring, i, j, k, g,ok,x,y,z,szinek,sarrshacol_colors1, sarrshacol_colors2, sarrshacol_colors3, sarrshacol_colors4, pict, difflev, egyketha, helyek, valt)
data <- ls()

dt <- data.table(pict = "faszom",
                 V1 = 1,
                 V2 = 1,
                 V3 = 1,
                 V4 = 1,
                 V5 = 1,
                 V6 = 1,
                 V7 = 1,
                 V8 = 1,
                 V9 = 1)
kurvaisten <- dt
for (i in 1:329){
  dt <- rbind(dt, kurvaisten)
}

for (i in 1:length(data)){
  eval(parse(text=paste("dt[i,1] <- data[i]", sep="")))
  for (j in 2:10){
    eval(parse(text=paste("dt[i,j] <- ",data[i],"[j-1]", sep="")))
  }
}

write.csv(dt,"picturedata.csv", row.names = FALSE)


# generate experimental sequence ------------------------------------------

#PRESTUDY
count <- 1
familiars <- NULL
for (i in 1:length(data)){
  if (grepl("prototype", data[i])){
    if (endsWith(data[i], "sha")){
      
    } else if (endsWith(data[i], "col")){
      
    } else {
      matches <- regmatches(data[i], gregexpr("[[:digit:]]+", data[i]))
      which <- as.numeric(unlist(matches))
      if (which < 6){
        familiars[count] <- data[i]
        count <- count + 1
      }
    }
  }
}

# STUDY
count <- 1
study <- NULL
for (i in 1:length(data)){
  if (endsWith(data[i], "sha")){
    
  } else if (endsWith(data[i], "col")){
    
  } else {
    matches <- regmatches(data[i], gregexpr("[[:digit:]]+", data[i]))
    which <- as.numeric(unlist(matches))
    if (length(which)>1){
      if (which[3] < 3){
        study[count] <- data[i]
        count <- count + 1
      }  
    }
  }
}

#TEST
count <- 1
test <- NULL
for (i in 1:length(data)){
  if (endsWith(data[i], "sha")){
    
  } else if (endsWith(data[i], "col")){
    
  } else {
    matches <- regmatches(data[i], gregexpr("[[:digit:]]+", data[i]))
    which <- as.numeric(unlist(matches))
    if (length(which)>1){
      if (which[3] > 2){
        test[count] <- data[i]
        count <- count + 1
      }  
    }
  }
}

# STUDY SEQ GEN

stfam <- rep(familiars, times = 5)
studyseq <- c(study, stfam)
studyseq <- sample(studyseq)

for (i in 1:length(studyseq)){
  studyseq[i] <- paste0(studyseq[i],".png")
}
write.csv(studyseq,"studyseq.csv", row.names = FALSE)

# TEST SEQ GEN

tfam <- rep(familiars, times = 5)
testseq <- c(study, test, tfam)
testseq <- sample(testseq)
for (i in 1:length(testseq)){
  testseq[i] <- paste0(testseq[i],".png")
}
write.csv(testseq,"testseq.csv", row.names = FALSE)

# FINALLY FAMILIARS - adding .png to names would screw up the processes before

for (i in 1:5){
  familiars[i] <- paste0(familiars[i],".png")
}
write.csv(familiars,"familiars.csv", row.names = FALSE)
