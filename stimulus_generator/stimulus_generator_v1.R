require(grDevices)
library(plotrix)
library(data.table)

# coordinates & colors ----------------------------------------------------------
# central coordinates
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
# corner coordinates
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
# colors (szín is hungarian for color, színek is plural)
szinek <- c("red","blue","green","yellow","orange","purple")

# generate prototypes ----------------------------------------------------

for (z in 1:10){
  pict <- c(0,0,0,0,0,0,0,0,0)
  ok <- FALSE
  while (ok == FALSE){
    while (sum(pict == 0) > 4){
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
    eval(parse(text=paste("if (prototype",z,"col[i] > 0){prototype",z,"col[i] <- sample(1:6,1)}", sep="")))
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


# generate stimuli from the prototypes later used as familiars -------------------------------------------------------

for (z in 1:5){
  # SAME ARRANGEMENT
  for (i in 1:4){
    # arrangement is from prototypeX
    eval(parse(text=paste("p",z,"sarr",i," <- prototype",z, sep="")))
    # shape generation needs to be controlled
    ok <- FALSE
    while (ok == FALSE){
      # generate shapes
      eval(parse(text=paste("p",z,"sarr",i,"sha <- p",z,"sarr",i, sep="")))
      for (g in 1:9){
        eval(parse(text=paste("if (p",z,"sarr",i,"sha[g] > 0){p",z,"sarr",i,"sha[g] <- sample(1:3,1)}", sep="")))
      }
      # compare shapes to prototype and other stim in grp
      check <- 0
      for (j in 1:i){
        if (j == 1){
          eval(parse(text=paste("if (all(prototype",z,"sha == p",z,"sarr",i,"sha)){}else{check <- check + 1}", sep="")))
        } else {
          eval(parse(text=paste("if (all(p",z,"sarr",i,"sha == p",z,"sarr",j-1,"sha)){}else{check <- check + 1}", sep="")))
        }
        if (check == i){
          ok <- TRUE
        }
      }
    }
    # color generation also needs to be controlled
    ok <- FALSE
    while (ok == FALSE){
      # generate colors
      eval(parse(text=paste("p",z,"sarr",i,"col <- p",z,"sarr",i, sep="")))
      for (g in 1:9){
        eval(parse(text=paste("if (p",z,"sarr",i,"col[g] > 0){p",z,"sarr",i,"col[g] <- sample(1:6,1)}", sep="")))
      }
      # compare colors to prototype and other stim in grp
      check <- 0
      for (j in 1:i){
        if (j == 1){
          eval(parse(text=paste("if (all(prototype",z,"col == p",z,"sarr",i,"col)){}else{check <- check + 1}", sep="")))
        } else {
          eval(parse(text=paste("if (all(p",z,"sarr",i,"col == p",z,"sarr",j-1,"col)){}else{check <- check + 1}", sep="")))
        }
        if (check == i){
          ok <- TRUE
        }
      }
    }
    # generate sarr pict
    # coloring always contains the current col info
    eval(parse(text=paste("coloring <- p",z,"sarr",i,"col", sep="")))
    # pict should now take on the data from shape info
    eval(parse(text=paste("pict <- p",z,"sarr",i,"sha", sep="")))
    eval(parse(text=paste("png('p",z,"sarr",i,".png')", sep="")))
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
  # SAME ARRANGEMENT + COLOR
  for (i in 1:4){
    # arrangement is from prototypeX
    eval(parse(text=paste("p",z,"sarrcol",i," <- prototype",z, sep="")))
    # shape generation needs to be controlled
    ok <- FALSE
    while (ok == FALSE){
      # generate shapes
      eval(parse(text=paste("p",z,"sarrcol",i,"sha <- p",z,"sarrcol",i, sep="")))
      for (g in 1:9){
        eval(parse(text=paste("if (p",z,"sarrcol",i,"sha[g] > 0){p",z,"sarrcol",i,"sha[g] <- sample(1:3,1)}", sep="")))
      }
      # compare shapes to prototype and other stim in grp
      check <- 0
      for (j in 1:i){
        if (j == 1){
          eval(parse(text=paste("if (all(prototype",z,"sha == p",z,"sarrcol",i,"sha)){}else{check <- check + 1}", sep="")))
          for (k in 1:4){
            eval(parse(text=paste("if (all(p",z,"sarrcol",i,"sha == p",z,"sarr",k,"sha)){}else{check <- check + 1}", sep="")))
          }
        } else {
          eval(parse(text=paste("if (all(p",z,"sarrcol",i,"sha == p",z,"sarrcol",j-1,"sha)){}else{check <- check + 1}", sep="")))
        }
        if (check == i+4){
          ok <- TRUE
        }
      }
    }
    # colors are the same as the prototype
    eval(parse(text=paste("p",z,"sarrcol",i,"col <- prototype",z,"col", sep="")))
    # generate sarr pict
    # coloring always contains the current col info
    eval(parse(text=paste("coloring <- p",z,"sarrcol",i,"col", sep="")))
    # pict should now take on the data from shape info
    eval(parse(text=paste("pict <- p",z,"sarrcol",i,"sha", sep="")))
    eval(parse(text=paste("png('p",z,"sarrcol",i,".png')", sep="")))
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
  # SAME ARRANGEMENT + SHAPE
  for (i in 1:4){
    # arrangement is from prototypeX
    eval(parse(text=paste("p",z,"sarrsha",i," <- prototype",z, sep="")))
    # shapes are the same as prototype
    eval(parse(text=paste("p",z,"sarrsha",i,"sha <- prototype",z,"sha", sep="")))
    # color generation needs to be controlled
    ok <- FALSE
    while (ok == FALSE){
      # generate shapes
      eval(parse(text=paste("p",z,"sarrsha",i,"col <- p",z,"sarrsha",i, sep="")))
      for (g in 1:9){
        eval(parse(text=paste("if (p",z,"sarrsha",i,"col[g] > 0){p",z,"sarrsha",i,"col[g] <- sample(1:6,1)}", sep="")))
      }
      # compare shapes to prototype and other stim in grp
      check <- 0
      for (j in 1:i){
        if (j == 1){
          eval(parse(text=paste("if (all(prototype",z,"col == p",z,"sarrsha",i,"col)){}else{check <- check + 1}", sep="")))
          for (k in 1:4){
            eval(parse(text=paste("if (all(p",z,"sarrsha",i,"col == p",z,"sarr",k,"col)){}else{check <- check + 1}", sep="")))
          }
        } else {
          eval(parse(text=paste("if (all(p",z,"sarrsha",i,"col == p",z,"sarrsha",j-1,"col)){}else{check <- check + 1}", sep="")))
        }
        if (check == i+4){
          ok <- TRUE
        }
      }
    }
    # generate sarr pict
    # coloring always contains the current col info
    eval(parse(text=paste("coloring <- p",z,"sarrsha",i,"col", sep="")))
    # pict should now take on the data from shape info
    eval(parse(text=paste("pict <- p",z,"sarrsha",i,"sha", sep="")))
    eval(parse(text=paste("png('p",z,"sarrsha",i,".png')", sep="")))
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
  # SAME ARRANGEMENT + SHAPE + COLOR (~recolor)
  for (i in 1:4){
    # arrangement is from prototypeX
    eval(parse(text=paste("p",z,"sarrshacol",i," <- prototype",z, sep="")))
    # shapes are the same as prototype
    eval(parse(text=paste("p",z,"sarrshacol",i,"sha <- prototype",z,"sha", sep="")))
    # color generation needs to be controlled
    # these stimuli use the same colors as the prototype, but not on the same places
    eval(parse(text=paste("sarrshacol_colors",i," <- unique(prototype",z,"col)", sep="")))
    eval(parse(text=paste("sarrshacol_colors",i," <- sarrshacol_colors",i,"[!sarrshacol_colors",i," == 0]", sep="")))
    eval(parse(text=paste("p",z,"sarrshacol",i,"col <- prototype",z, sep="")))
    for (g in 1:9){
      eval(parse(text=paste("if (p",z,"sarrshacol",i,"col[g] > 0){p",z,"sarrshacol",i,"col[g] <- sample(sarrshacol_colors",i,",1)}", sep="")))
    }
    # generate sarr pict
    # coloring always contains the current col info
    eval(parse(text=paste("coloring <- p",z,"sarrshacol",i,"col", sep="")))
    # pict should now take on the data from shape info
    eval(parse(text=paste("pict <- p",z,"sarrshacol",i,"sha", sep="")))
    eval(parse(text=paste("png('p",z,"sarrshacol",i,".png')", sep="")))
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


# generata fully novels from prototypes 6-10 ---------------------------------------------------

for (z in 6:10){
  for (i in 1:4){
    # arrangement is from prototypeX
    eval(parse(text=paste("p",z,"sarr",i," <- prototype",z, sep="")))
    # shape generation needs to be controlled
    ok <- FALSE
    while (ok == FALSE){
      # generate shapes
      eval(parse(text=paste("p",z,"sarr",i,"sha <- p",z,"sarr",i, sep="")))
      for (g in 1:9){
        eval(parse(text=paste("if (p",z,"sarr",i,"sha[g] > 0){p",z,"sarr",i,"sha[g] <- sample(1:3,1)}", sep="")))
      }
      # compare shapes to prototype and other stim in grp
      check <- 0
      for (j in 1:i){
        if (j == 1){
          eval(parse(text=paste("if (all(prototype",z,"sha == p",z,"sarr",i,"sha)){}else{check <- check + 1}", sep="")))
        } else {
          eval(parse(text=paste("if (all(p",z,"sarr",i,"sha == p",z,"sarr",j-1,"sha)){}else{check <- check + 1}", sep="")))
        }
        if (check == i){
          ok <- TRUE
        }
      }
    }
    # color generation also needs to be controlled
    ok <- FALSE
    while (ok == FALSE){
      # generate colors
      eval(parse(text=paste("p",z,"sarr",i,"col <- p",z,"sarr",i, sep="")))
      for (g in 1:9){
        eval(parse(text=paste("if (p",z,"sarr",i,"col[g] > 0){p",z,"sarr",i,"col[g] <- sample(1:6,1)}", sep="")))
      }
      # compare colors to prototype and other stim in grp
      check <- 0
      for (j in 1:i){
        if (j == 1){
          eval(parse(text=paste("if (all(prototype",z,"col == p",z,"sarr",i,"col)){}else{check <- check + 1}", sep="")))
        } else {
          eval(parse(text=paste("if (all(p",z,"sarr",i,"col == p",z,"sarr",j-1,"col)){}else{check <- check + 1}", sep="")))
        }
        if (check == i){
          ok <- TRUE
        }
      }
    }
    # generate sarr pict
    # coloring always contains the current col info
    eval(parse(text=paste("coloring <- p",z,"sarr",i,"col", sep="")))
    # pict should now take on the data from shape info
    eval(parse(text=paste("pict <- p",z,"sarr",i,"sha", sep="")))
    eval(parse(text=paste("png('p",z,"sarr",i,".png')", sep="")))
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


# stimdata to csv ---------------------------------------------------------
# create a csv which contains all the data of the pictures

remove(cent_coords_x, cent_coords_y, corn_coords_x, corn_coords_y, check, coloring, i, j, k, g,ok,x,y,z,szinek,sarrshacol_colors1, sarrshacol_colors2, sarrshacol_colors3, sarrshacol_colors4, pict)
data <- ls()

dt <- data.table(pict = "start",
                 V1 = 1,
                 V2 = 1,
                 V3 = 1,
                 V4 = 1,
                 V5 = 1,
                 V6 = 1,
                 V7 = 1,
                 V8 = 1,
                 V9 = 1)
temp <- dt
for (i in 1:329){
  dt <- rbind(dt, temp)
}

for (i in 1:length(data)){
  eval(parse(text=paste("dt[i,1] <- data[i]", sep="")))
  for (j in 2:10){
    eval(parse(text=paste("dt[i,j] <- ",data[i],"[j-1]", sep="")))
  }
}

write.csv(dt,"picturedata.csv", row.names = FALSE)