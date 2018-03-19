#########################################################
# For writing to R window comment out the following line
# in each graph
# theme_bw(base_size = 36, base_family = "") +
#########################################################

library("ggplot2")
library("XLConnect")
library("reshape2")

###### Accuracy/MCC etc. vs. choice of nFeatures  ############

type = "Unbalanced_";

# Use the appropriate data file here:
xlsFile  = paste0("PerfSearch_RF_", type, "SvmRFE2_comb.xlsx");
workBook = loadWorkbook(xlsFile);

for (xlsSheet in c("Tenfold_Coarse", "Tenfold_MidGrain", "Tenfold_Avg")) {

  data = readWorksheet(workBook, xlsSheet);
  
  data = data[, c("nF", "AUCROC", "AUCPR", "Accuracy", "Sensitivity", "Specificity")];
  colnames(data) = c("nF", "auROC", "auPR", "Acc", "Sn", "Sp");
  
  df <- melt(data,  id.vars = "nF", variable.name = 'Metric');
  
  nFeatureTuning = ggplot(df,aes(x=nF,y=value)) +
    theme_bw(base_size = 48, base_family = "") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.title = element_blank()) +
    theme(legend.position = "top") +
    geom_line(aes(colour=Metric),size =3) +
    labs(x = "Num. of Features", y = "Perf. Score x 100");
  
  postscript(file = paste0(type, xlsSheet, ".eps"), paper = "letter");
  print(nFeatureTuning);
  dev.off();
}