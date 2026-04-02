require(grDevices)
library(data.table)
library(plotrix)

# Modifications: setting RECALL_DATA as revised table + directing files to image_pool folder
#Set recall data here!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!######

# RECALL_DATA <- gnet4s_delay_recall
RECALL_DATA <- rcll_revised

########################################################################################


# prep data extraction ####
cells <- c("cell1","cell2","cell3",
           "cell4","cell5","cell6",
           "cell7","cell8","cell9")
setnafill(RECALL_DATA, cols = cells, fill = 0)
seqnr <- 0
arr <- c()
col <- c()
sha <- c()
#####

# coordinates and colors ####
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
szinek <- c("red","blue","green","yellow","orange","purple")
#####

for (i in 1:nrow(RECALL_DATA)){
  # this extracts the recall data from each row
  for (j in 1:9){
    if (eval(parse(text=paste("RECALL_DATA$",cells[j],"[i] > 0",sep="")))){
      arr[j] <- 1
      eval(parse(text=paste("seqnr <- RECALL_DATA$",cells[j],"[i]",sep="")))
      eval(parse(text=paste("col[j] <- RECALL_DATA$plc",seqnr,"c[i]",sep="")))
      eval(parse(text=paste("sha[j] <- RECALL_DATA$plc",seqnr,"sh[i]",sep="")))
    } else {
      arr[j] <- 0
      col[j] <- 0
      sha[j] <- 0
    }
  }
  for (k in 1:9){
    if (sha[k] == "sqa"){
      sha[k] <- 1
    } else if (sha[k] == "cir"){
      sha[k] <- 3
    } else if (sha[k] == "tri"){
      sha[k] <- 2
    }
  }
  sha <- as.numeric(sha)
  sha <- c(sha[7:9],sha[4:6],sha[1:3])
  col <- c(col[7:9],col[4:6],col[1:3])
  # generate the picture here
  eval(parse(text=paste("png(file = './image_pool/",RECALL_DATA$multichar_knowresp[i],"-",RECALL_DATA$rtnr[i],".png')", sep="")))
  plot(c(0, 150), c(0, 150), type = "n", xlab = "", ylab = "", main = "", axes=FALSE)
  for (l in 1:9){
    if (sha[l] == 1){
      rect(corn_coords_x[l], corn_coords_y[l], corn_coords_x[l]+46, corn_coords_y[l]+46, col = szinek[col[l]], lwd = 2)
    }
    if (sha[l] == 2){
      polygon(c(corn_coords_x[l],corn_coords_x[l]+23,corn_coords_x[l]+46,corn_coords_x[l]),
              c(corn_coords_y[l],corn_coords_y[l]+46, corn_coords_y[l], corn_coords_y[l]),
              xpd = TRUE, col = szinek[col[l]], lty = 1, lwd = 2, border = "black")
    }
    if (sha[l] == 3){
      draw.circle(cent_coords_x[l],cent_coords_y[l], radius = 21, col = szinek[col[l]], border="black",lty=1,lwd=2)
    }
  }
  dev.off()
}
