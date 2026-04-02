library(corrplot)
x <- cor(difference_indices[,3:6])
corrplot(x, type="upper", method = "number", sig.level = 0.0000001, insig = "blank")
p.mat <- cor.mtest(difference_indices[,3:6])
corrplot(x, type="upper", 
         p.mat = p.mat, sig.level = 0.01)


plot_model(logmr2, type = "int") +
  scale_y_continuous(limits = c(0.4, 1), 
                     labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Predicted probability of 'Not seen' response") + 
  ylab("") +
  scale_color_discrete(name = "Trial quality", labels=c("new" = "Distractor", "old" = "Prev. pres."))
