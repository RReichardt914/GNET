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
szinek <- c("red","blue","green","yellow","orange","purple")

# prototípus generalas ----------------------------------------------------

# Load the itertools library
library(itertools)

# Generate all combinations of placing 4 items in 9 cells
combinations <- combn(9,4)

# Convert combinations to numeric vectors
vectors <- apply(combinations,2, function(combination) {
  vector <- rep(0, 9)
  vector[combination] <- 1
  return(vector)
})

# Randomly select 95 out of the 126 combinations
set.seed(123) # Setting seed for reproducibility
selected_indices <- sample(1:ncol(combinations), 95)
selected_vectors <- vectors[, selected_indices]

# Generate shapes
set.seed(123) # Setting seed for reproducibility of random numbers
shape_vectors <- apply(selected_vectors, 2, function(vector) {
  vector[vector == 1] <- sample(1:3, sum(vector == 1), replace = TRUE)
  return(vector)
})

# Generate colors
set.seed(123) # Setting seed for reproducibility of random numbers
color_vectors <- apply(selected_vectors, 2, function(vector) {
  vector[vector == 1] <- sample(1:6, sum(vector == 1), replace = TRUE)
  return(vector)
})

# generate familiars and fillers
for (j in 1:ncol(selected_vectors)){
  pict <- shape_vectors[,j]
  
  if (j < 6){
    eval(parse(text=paste("png('familiar",j,".png')", sep="")))
  } else {
    eval(parse(text=paste("png('filler",j-5,".png')", sep="")))
  }
  
  plot(c(0, 150), c(0, 150), type = "n", xlab = "", ylab = "", main = "", axes=FALSE)
  for (i in 1:9){
    if (pict[i] == 1){
      rect(corn_coords_x[i], corn_coords_y[i], corn_coords_x[i]+46, corn_coords_y[i]+46, col = szinek[color_vectors[i,j]], lwd = 2)
    }
    if (pict[i] == 2){
      polygon(c(corn_coords_x[i],corn_coords_x[i]+23,corn_coords_x[i]+46,corn_coords_x[i]),
              c(corn_coords_y[i],corn_coords_y[i]+46, corn_coords_y[i], corn_coords_y[i]),
              xpd = TRUE, col = szinek[color_vectors[i,j]], lty = 1, lwd = 2, border = "black")
    }
    if (pict[i] == 3){
      draw.circle(cent_coords_x[i],cent_coords_y[i], radius = 21, col = szinek[color_vectors[i,j]], border="black",lty=1,lwd=2)
    }
  }
  dev.off()
}

# Function to modify vectors with linked modifications
modify_vectors_linked <- function(vectors_1_to_3, vectors_1_to_6, num_modifications, value_range_1_to_3, value_range_1_to_6) {
  modified_vectors_1_to_3 <- list()
  modified_vectors_1_to_6 <- list()
  
  for (i in 1:5) {
    vector_1_to_3 <- vectors_1_to_3[, i]
    vector_1_to_6 <- vectors_1_to_6[, i]
    
    new_vector_1_to_3 <- vector_1_to_3
    new_vector_1_to_6 <- vector_1_to_6
    
    indices <- which(vector_1_to_3 != 0)
    random_indices <- sample(indices, num_modifications)
    
    for (idx in random_indices) {
      new_value_1_to_3 <- sample(value_range_1_to_3, 1)
      while (new_value_1_to_3 == vector_1_to_3[idx]) {
        new_value_1_to_3 <- sample(value_range_1_to_3, 1)
      }
      
      new_value_1_to_6 <- sample(value_range_1_to_6, 1)
      while (new_value_1_to_6 == vector_1_to_6[idx]) {
        new_value_1_to_6 <- sample(value_range_1_to_6, 1)
      }
      
      new_vector_1_to_3[idx] <- new_value_1_to_3
      new_vector_1_to_6[idx] <- new_value_1_to_6
    }
    
    modified_vectors_1_to_3 <- append(modified_vectors_1_to_3, list(new_vector_1_to_3))
    modified_vectors_1_to_6 <- append(modified_vectors_1_to_6, list(new_vector_1_to_6))
  }
  
  return(list(modified_vectors_1_to_3, modified_vectors_1_to_6))
}

# Generate modified vectors with one modification
set.seed(123) # Setting seed for reproducibility
modified_vectors_one_mod <- modify_vectors_linked(shape_vectors[,1:5], color_vectors[,1:5], num_modifications = 1, value_range_1_to_3 = 1:3, value_range_1_to_6 = 1:6)

# Generate modified vectors with two modifications
set.seed(123) # Setting seed for reproducibility
modified_vectors_two_mods <- modify_vectors_linked(shape_vectors[,1:5], color_vectors[,1:5], num_modifications = 2, value_range_1_to_3 = 1:3, value_range_1_to_6 = 1:6)

# Generate modified vectors with three modifications
set.seed(123) # Setting seed for reproducibility
modified_vectors_three_mods <- modify_vectors_linked(shape_vectors[,1:5], color_vectors[,1:5], num_modifications = 3, value_range_1_to_3 = 1:3, value_range_1_to_6 = 1:6)

# Generate modified vectors with four modifications
set.seed(123) # Setting seed for reproducibility
modified_vectors_four_mods <- modify_vectors_linked(shape_vectors[,1:5], color_vectors[,1:5], num_modifications = 4, value_range_1_to_3 = 1:3, value_range_1_to_6 = 1:6)

# generate targets

all_modified_vectors <- list(
  modified_vectors_one_mod,
  modified_vectors_two_mods,
  modified_vectors_three_mods,
  modified_vectors_four_mods
)

# Iterate through the list in a for loop
for (set_index in 1:length(all_modified_vectors)) {
  current_set <- all_modified_vectors[[set_index]]
  
  cat("Processing set", set_index, ":\n")
  
  for (vector_index in 1:length(current_set[[1]])) {
    pict <- current_set[[1]][[vector_index]]
    if (set_index == 1){
      eval(parse(text=paste("png('target_",vector_index,"_25.png')", sep="")))
    } else if (set_index == 2) {
      eval(parse(text=paste("png('target_",vector_index,"_50.png')", sep="")))
    } else if (set_index == 3) {
      eval(parse(text=paste("png('target_",vector_index,"_75.png')", sep="")))
    } else if (set_index == 4) {
      eval(parse(text=paste("png('target_",vector_index,"_100.png')", sep="")))
    }
    
    plot(c(0, 150), c(0, 150), type = "n", xlab = "", ylab = "", main = "", axes=FALSE)
    for (i in 1:9){
      if (pict[i] == 1){
        rect(corn_coords_x[i], corn_coords_y[i], corn_coords_x[i]+46, corn_coords_y[i]+46, col = szinek[current_set[[2]][[vector_index]][i]], lwd = 2)
      }
      if (pict[i] == 2){
        polygon(c(corn_coords_x[i],corn_coords_x[i]+23,corn_coords_x[i]+46,corn_coords_x[i]),
                c(corn_coords_y[i],corn_coords_y[i]+46, corn_coords_y[i], corn_coords_y[i]),
                xpd = TRUE, col = szinek[current_set[[2]][[vector_index]][i]], lty = 1, lwd = 2, border = "black")
      }
      if (pict[i] == 3){
        draw.circle(cent_coords_x[i],cent_coords_y[i], radius = 21, col = szinek[current_set[[2]][[vector_index]][i]], border="black",lty=1,lwd=2)
      }
    }
    dev.off()
  }
}
