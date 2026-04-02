# visualizing the comparisons
# this script draws the "original_stimulus" - generated in 05-comparator...R

require(grDevices)
library(plotrix)
library(data.table)
library(tidyverse)

# koordináták & szinek ----------------------------------------------------------
# kozepso koordinatak
x <- 25
y <- 275

cent_coords_x <- matrix(nrow = 3, ncol = 3)
cent_coords_y <- matrix(nrow = 3, ncol = 3)

for (i in 1:3){
  for (j in 1:3){
    cent_coords_x[i,j] <- x
    cent_coords_y[i,j] <- y
    x <- x + 50
  }
  y <- y - 50
  x <- 25
}
# szelso koordinatak
x <- 2
y <- 252
corn_coords_x <- matrix(nrow = 3, ncol = 3)
corn_coords_y <- matrix(nrow = 3, ncol = 3)
for (i in 1:3){
  for (j in 1:3){
    corn_coords_x[i,j] <- x
    corn_coords_y[i,j] <- y
    x <- x + 50
  }
  y <- y - 50
  x <- 2
}
rm(i,j,x,y)


# szinek
szinek <- c("red","blue","green","yellow","orange","purple")

# kép elkészítése

png('original_stimulus_pairs_visualization.png',width = 1400, height = 900)
plot(c(0, 700), c(0, 450), type = "n", xlab = "", ylab = "", main = "", axes=FALSE)

rect(-10, 140, 160, 310, col = "white", lwd = 1)

for (i in 1:5){
  # kép adatok forrásának függvényében változtatni!
  current_shape <- as.numeric(original_stimulus[i,"shape"])
  current_row <- as.numeric(original_stimulus[i,"yrow"])
  current_col <- as.numeric(original_stimulus[i,"xcol"])
  current_color <- as.numeric(original_stimulus[i,"color"])
  
  if (current_shape == 1){
    rect(corn_coords_x[current_row,current_col], corn_coords_y[current_row,current_col],
         corn_coords_x[current_row,current_col]+46, corn_coords_y[current_row,current_col]+46,
         col = szinek[current_color], lwd = 2)
  }
  if (current_shape == 2){
    polygon(c(corn_coords_x[current_row,current_col],corn_coords_x[current_row,current_col]+23,corn_coords_x[current_row,current_col]+46,corn_coords_x[current_row,current_col]),
            c(corn_coords_y[current_row,current_col],corn_coords_y[current_row,current_col]+46, corn_coords_y[current_row,current_col], corn_coords_y[current_row,current_col]),
            xpd = TRUE, col = szinek[current_color], lty = 1, lwd = 2, border = "black")
  }
  if (current_shape == 3){
    draw.circle(cent_coords_x[current_row,current_col],cent_coords_y[current_row,current_col], radius = 21, col = szinek[current_color], border="black",lty=1,lwd=2)
  }
}

miniatures_x <- c(190,320,450,570,190,320,450,190,320,450)
miniatures_y <- c(390,390,390,390,240,240,240,90,90,90)

for (k in 1:nrow(original_stim_pairs)){
  # kozepso koordinatak
  x <- miniatures_x[k]+15
  y <- miniatures_y[k]+15
  
  cent_coords_x <- matrix(nrow = 3, ncol = 3)
  cent_coords_y <- matrix(nrow = 3, ncol = 3)
  
  for (i in 1:3){
    for (j in 1:3){
      cent_coords_x[i,j] <- x
      cent_coords_y[i,j] <- y
      x <- x + 30
    }
    y <- y - 30
    x <- miniatures_x[k]+15
  }
  # szelso koordinatak
  x <- miniatures_x[k]
  y <- miniatures_y[k]
  
  corn_coords_x <- matrix(nrow = 3, ncol = 3)
  corn_coords_y <- matrix(nrow = 3, ncol = 3)
  
  for (i in 1:3){
    for (j in 1:3){
      corn_coords_x[i,j] <- x
      corn_coords_y[i,j] <- y
      x <- x + 30
    }
    y <- y - 30
    x <- miniatures_x[k]
  }
  
  rect(miniatures_x[k]-10, miniatures_y[k]-70, miniatures_x[k]+100,  miniatures_y[k]+40, col = "white", lwd = 1)
  
  current_shape <- as.numeric(original_stim_pairs[k,"shape"])
  current_row <- as.numeric(original_stim_pairs[k,"yrow"])
  current_col <- as.numeric(original_stim_pairs[k,"xcol"])
  current_color <- as.numeric(original_stim_pairs[k,"color"])
  
  if (current_shape == 1){
    rect(corn_coords_x[current_row,current_col], corn_coords_y[current_row,current_col],
         corn_coords_x[current_row,current_col]+26, corn_coords_y[current_row,current_col]+26,
         col = szinek[current_color], lwd = 2)
  }
  if (current_shape == 2){
    polygon(c(corn_coords_x[current_row,current_col],corn_coords_x[current_row,current_col]+13,corn_coords_x[current_row,current_col]+26,corn_coords_x[current_row,current_col]),
            c(corn_coords_y[current_row,current_col],corn_coords_y[current_row,current_col]+26, corn_coords_y[current_row,current_col], corn_coords_y[current_row,current_col]),
            xpd = TRUE, col = szinek[current_color], lty = 1, lwd = 2, border = "black")
  }
  if (current_shape == 3){
    draw.circle(cent_coords_x[current_row,current_col],cent_coords_y[current_row,current_col], radius = 11, col = szinek[current_color], border="black",lty=1,lwd=2)
  }
  
  current_shape <- as.numeric(original_stim_pairs[k,"shape2"])
  current_row <- as.numeric(original_stim_pairs[k,"yrow2"])
  current_col <- as.numeric(original_stim_pairs[k,"xcol2"])
  current_color <- as.numeric(original_stim_pairs[k,"color2"])
  
  if (current_shape == 1){
    rect(corn_coords_x[current_row,current_col], corn_coords_y[current_row,current_col],
         corn_coords_x[current_row,current_col]+26, corn_coords_y[current_row,current_col]+26,
         col = szinek[current_color], lwd = 2)
  }
  if (current_shape == 2){
    polygon(c(corn_coords_x[current_row,current_col],corn_coords_x[current_row,current_col]+13,corn_coords_x[current_row,current_col]+26,corn_coords_x[current_row,current_col]),
            c(corn_coords_y[current_row,current_col],corn_coords_y[current_row,current_col]+26, corn_coords_y[current_row,current_col], corn_coords_y[current_row,current_col]),
            xpd = TRUE, col = szinek[current_color], lty = 1, lwd = 2, border = "black")
  }
  if (current_shape == 3){
    draw.circle(cent_coords_x[current_row,current_col],cent_coords_y[current_row,current_col], radius = 11, col = szinek[current_color], border="black",lty=1,lwd=2)
  }
    
  
}
dev.off()