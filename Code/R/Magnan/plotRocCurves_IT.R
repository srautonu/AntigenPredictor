
xlsFile  = c("VaxiJen_IT.xlsx", "AntigenPRO_IT.xlsx", "PerfSearch_RF_SvmRFE2_comb.xlsx", "PerfSearch_RF_SvmRFE2_comb.xlsx");
xlsSheet = c("Score"          , "Score"             , "ITScore_2_490"                  , "ITScore_1_500");

toolName = c("Vaxijen", "AntigenPRO", "Antigenic",  "Antigenic*")

rocCurveFile = "ROCCurve_IT.eps"
prCurveFile  = "PRCurve_IT.eps"

rocCurvePoints = NULL;
prCurvePoints = NULL;
for (i in 1:length(xlsFile)) {

  workBook = loadWorkbook(xlsFile[i])
  data = readWorksheet(workBook, xlsSheet[i]);
  
  pred = prediction(as.numeric(data$Score), data$protection);
  
  rocCurve = ROCR::performance(pred,"tpr", "fpr");
  df = data.frame(
    x = unlist(rocCurve@x.values), 
    y = unlist(rocCurve@y.values),
    tool = toolName[i]
  );
  rocCurvePoints = rbind(rocCurvePoints, df);
  
  prCurve  = ROCR::performance(pred,"prec", "rec");
  df = data.frame(
    x = unlist(prCurve@x.values), 
    y = unlist(prCurve@y.values),
    tool = toolName[i]
  );
  prCurvePoints = rbind(prCurvePoints, df);
}


rocPlot = ggplot(rocCurvePoints,aes(x, y)) + 
  theme_bw(base_size = 36, base_family = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  geom_line(aes(colour = tool),size = 3) +
  labs(x = "False Positive Rate", y = "True Positive Rate");
postscript(file = rocCurveFile, paper = "letter");
rocPlot;
dev.off();

prPlot = ggplot(prCurvePoints,aes(x, y)) + 
  theme_bw(base_size = 36, base_family = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  geom_line(aes(colour=tool),size = 3) +
  labs(x = "Recall", y = "Precision");
postscript(file = prCurveFile, paper = "letter");
prPlot;
dev.off();
