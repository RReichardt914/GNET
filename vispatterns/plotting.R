plot_model(logmr2, type="eff")$DTF

plot_model(logmr2, type = "eff")$DTF +
  ggtitle("A) Predicted probability of 'already seen' response") + 
  ylab("") + 
  xlab("Difference To the corresponding familiar (DTF)") + 
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.text.y=element_text(size=15)) +
  theme(axis.title.x=element_text(size=15)) + 
  theme(title=element_text(size=18))

plot_model(logmr2, type = "eff")$tq +
  scale_y_continuous(limits = c(0.25, 0.45), 
                     labels = scales::percent_format(accuracy = 1)) +
  ggtitle("B) Predicted probability of 'already seen' response") + 
  ylab("") + 
  xlab("") +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.text.y=element_text(size=15)) +
  theme(axis.title.x=element_text(size=15)) + 
  theme(title=element_text(size=18)) +
  scale_x_discrete(limits=c("new" = "distractor", "old" = "previously presented"))

plot_model(logmr2, type = "int") +
  scale_y_continuous(limits = c(0, 0.7), 
                     labels = scales::percent_format(accuracy = 1)) +
  ggtitle("C) Predicted probability of 'already seen' response") + 
  ylab("") +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.text.y=element_text(size=15)) +
  theme(axis.title.x=element_text(size=15)) + 
  theme(title=element_text(size=18)) +
  theme(legend.text=element_text(size=15)) +
  scale_color_discrete(name = "Trial type", labels=c("new" = "distractor", "old" = "prev. pres")) 
