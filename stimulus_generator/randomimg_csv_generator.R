location_vct <- t(selected_vectors)
color_vct <- t(color_vectors)
shape_vct <- t(shape_vectors)

# Get the number of rows in each data table
n_rows <- nrow(location_vct)

# Create an empty data frame to store the result
combined_vct <- data.frame(matrix(ncol = ncol(location_vct), nrow = 3 * n_rows))

# Fill the combined_vct with alternating rows
for (i in 1:n_rows) {
  combined_vct[(3 * i - 2), ] <- location_vct[i, ]
  combined_vct[(3 * i - 1), ] <- color_vct[i, ]
  combined_vct[(3 * i), ] <- shape_vct[i, ]
}

write.csv(combined_vct, "random_images.csv")

