corr <- t2lmm[,.N,by=c("difference_from_fams", "correct_response")]

corr_1 <- corr[correct_response == 1]
colnames(corr_1) <- c("difference_from_fams","correct_response","correct")
corr_2 <- corr[correct_response == 0]
colnames(corr_2) <- c("difference_from_fams","correct_response","miss")
corr <- merge(corr_1,corr_2, by=c("difference_from_fams"))

corr[,rate := correct/(correct+miss),]

library(ggplot2)
ggplot(corr, aes(x=difference_from_fams, y=rate)) +
  geom_point(size=2, shape=23)

ggplot(t3, aes(x=df, y=rate)) +
  geom_point(size=2, shape=23)
library(Hmisc)
x <- t3$df
y <- t3$rate
cor.test(x, y, method="spearman")
