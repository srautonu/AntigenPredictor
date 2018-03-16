
xlsFile  = c("VaxiJen_IT.xlsx", "AntigenPRO_IT.xlsx",
xlsSheet = c("Score"          , "Score"             , 

resultsFileName = "IndependentTestResults.csv"

workBook = loadWorkbook(xlsFile)
data = readWorksheet(workBook, xlsSheet);


pred = prediction(as.numeric(data$Score), data$protection);

rocCurve = ROCR::performance(pred,"tpr", "fpr");
rocCurvePoints = data.frame(
  x = unlist(rocCurve@x.values), 
  y = unlist(rocCurve@y.values)
);

prCurve  = ROCR::performance(pred,"prec", "rec");
prCurvePoints = data.frame(
  x = unlist(prCurve@x.values), 
  y = unlist(prCurve@y.values)
);

rocPlot = ggplot(rocCurvePoints,aes(x, y)) + 
  #theme_bw(base_size = 36, base_family = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  #theme(aspect.ratio = 0.7) +
  geom_line(aes(),size = 3) +
  labs(x = "False Positive Rate", y = "True Positive Rate");

prPlot = ggplot(prCurvePoints,aes(x, y)) + 
  #theme_bw(base_size = 36, base_family = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  #theme(aspect.ratio = 0.7) +
  geom_line(aes(),size = 3) +
  labs(x = "Precision", y = "Recall");
