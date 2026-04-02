types <- c("diffarr","sarr","sarrsha","sarrcol","sarrshacol")
for (i in 1:5){
  normalornot <- shapiro.test(Test_ResponsesByType[type == types[i],CorrRecog])
  if (normalornot$p.value < 0.05){
    wtest <- wilcox.test(Test_ResponsesByType[type == types[i],CorrRecog], mu = 0, alternative = "greater")
    print(wtest)
    print(qnorm(wtest$p.value))
  } else {
    print(t.test(Test_ResponsesByType[type == types[i],CorrRecog], mu = 0, alternative = "greater"))
  }
}




