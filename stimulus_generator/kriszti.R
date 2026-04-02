library(data.table)
library(RecordLinkage)

szepk_dat <- as.data.table(read.csv("szepk_fp.csv", sep = ";", encoding = "UTF-8"))
colnames(szepk_dat) <- c("Wolt","Venues.Venue.ID","Purchases.Ttv.Local.Total","X","X.1","FP")
szepk_dat$melyik <- ""

levensteind <- c()
ldwhich <- szepk_dat$Wolt
k <- 0

for (i in (1:672)){
  for (j in 1:nrow(szepk_dat)){
    levensteind <- c(levensteind, levenshteinSim(szepk_dat$FP[i], szepk_dat$Wolt[j]))
  }
  szepk_dat$X[i] <- (max(levensteind))
  szepk_dat$melyik[i] <- ldwhich[match(max(levensteind),levensteind)]
  print(match(max(levensteind),levensteind))
  print(ldwhich[match(max(levensteind),levensteind)])
  if (max(levensteind) < 0.3){
    k <- k + 1
    szepk_dat$X.1[k] <- szepk_dat$FP[i]
  }
  levensteind <- c()
}

colnames(szepk_dat) <- c("Wolt","Venues.Venue.ID","Purchases.Ttv.Local.Total","Difference metric [levenstein]","missing from Wolt","FP","melyik")

write.csv(szepk_dat, "kriszti3.csv")
