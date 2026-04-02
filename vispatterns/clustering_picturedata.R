library(data.table)
library(stringr)
picdat <- as.data.table(read.csv("picturedata.csv"))

arrng <- picdat[!(endsWith(pict, "col")) & !(endsWith(pict, "sha")),]
color <- picdat[endsWith(pict, "col"),]
color$pict <- color[,gsub('.{3}$', '', pict)]
shape <- picdat[endsWith(pict, "sha"),]
shape$pict <- shape[,gsub('.{3}$', '', pict)]
new <- merge(arrng, color,by=c("pict"))
new <- merge(new, shape,by=c("pict"))

set.seed(20)
clusters <- kmeans(new[,2:28], 10)
