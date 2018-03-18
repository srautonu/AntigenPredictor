library(ggplot2)

rocCurveFile = "ROCCurve_CV.eps";
#prCurveFile  = "PRCurve_CV.eps";

rocCurvePoints = readRDS("rocData_Balanced.rds");
#prCurvePoints  = readRDS("prData_Balanced.rds");

t = which(rocCurvePoints$Features %in% c(100, 300, 500));
rocCurvePoints = rocCurvePoints[t,]

#t = which(prCurvePoints$Features %in% c(100, 500, 700));
#prCurvePoints = prCurvePoints[t,]

rocPlot = ggplot(rocCurvePoints,aes(x, y)) + 
  theme_bw(base_size = 36, base_family = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  geom_line(aes(colour=Features),size = 2) +
  labs(x = "False Positive Rate", y = "True Positive Rate");
  
postscript(file = rocCurveFile, paper = "letter");
rocPlot;
dev.off();

# prPlot = ggplot(prCurvePoints,aes(x, y)) + 
#   theme_bw(base_size = 36, base_family = "") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
#   theme(legend.title = element_blank()) +
#   theme(legend.position = "top") +
#   geom_line(aes(colour=tool),size = 3) +
#   labs(x = "Recall", y = "Precision");
# postscript(file = prCurveFile, paper = "letter");
# prPlot;
# dev.off();
