#########################################################
# For writing to R window comment out the following line
# in each graph
# theme_bw(base_size = 36, base_family = "") +
#########################################################

library("ggplot2")
library("XLConnect")
library("reshape2")

xlsFile  = c(
  "VaxiJen_IT.xlsx",
  "AntigenPro_IT.xlsx",
  "PerfSearch_RF_SvmRFE2_comb.xlsx",
  "PerfSearch_RF_SvmRFE2_comb.xlsx"
);
xlsSheet = c(
  "Performance",
  "Performance",
  "ITPerf_2_490",
  "ITPerf_1_500"
);

toolName = c("Vaxijen", "AntigenPRO", "Antigenic",  "AntigenicU")

for (i in 1:length(xlsFile)) {
  workBook = loadWorkbook(xlsFile[i]);
  data = readWorksheet(workBook, xlsSheet[i]);
  
  data = data[, c("Threshold", "Accuracy", "Sensitivity", "Specificity")];
  data = subset(data, Threshold >= 0.1 & Threshold <= 0.85);

  df <- melt(data,  id.vars = "Threshold", variable.name = 'Metric');
  
  perfPlot = ggplot(df,aes(x=Threshold,y=value)) +
    theme_bw(base_size = 36, base_family = "") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.title = element_blank()) +
    theme(legend.position = "top") +
    geom_line(aes(colour=Metric),size =3) +
    labs(x = "Threshold", y = "Perf. Score x 100");
  
  postscript(file = paste0("ITPerf_", toolName[i], ".eps"), paper = "letter");
  print(perfPlot);
  dev.off();
}
