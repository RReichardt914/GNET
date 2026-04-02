library(magick)
logo <- image_read('agykut-logo.png')
logo <- image_scale(logo, "324")
pict <- image_read('hatter.png')
kesz <- c(pict, logo)
kesz <- image_mosaic(kesz)
library(data.table)
texts <- read.csv("texts.csv", header = FALSE, fileEncoding = "UTF-8")
texts$V2 <- as.character(texts$V2)
i <- 1
image_annotate(kesz, texts$V2[i], size = 32, color = "red", gravity = "center")

print(kesz)

