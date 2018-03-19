library("XLConnect")
library("ggplot2")
library("ROCR")

# Use the appropriate data file here:
xlsFile  = "Vaxijen_IT.xlsx"
xlsSheet = "Score"

resultsFileName = "IndependentTestResults.csv"

workBook = loadWorkbook(xlsFile)
data = readWorksheet(workBook, xlsSheet);

cat(as.character(Sys.time()),">> Recording performance in ", resultsFileName, "  ...\n");
itData = data.frame(matrix(ncol = 5, nrow = 0))
for (threshold in seq(from=0.01, to=0.99, by=0.01)) {
  
  pred = prediction(as.numeric(data$Score >= threshold), data$protection);
  acc  = unlist(ROCR::performance(pred,"acc")@y.values)[2];
  sens = unlist(ROCR::performance(pred,"sens")@y.values)[2];
  spec = unlist(ROCR::performance(pred,"spec")@y.values)[2];
  mcc  = unlist(ROCR::performance(pred,"mat")@y.values)[2];
  prec = unlist(ROCR::performance(pred,"prec")@y.values)[2];
  
  itData = rbind(itData, c(threshold, acc, sens, spec, mcc, prec));
}

colnames(itData) = c("Threshold", "Accuracy", "Sensitivity", "Specificity", "MCC", "Precision");
write.csv(itData, resultsFileName);
cat(as.character(Sys.time()),">> Done.\n");

pred = prediction(data$Score, data$protection);
AUCROC  = ROCR::performance(pred,"auc")@y.values[[1]];

prCurve  = ROCR::performance(pred,"prec", "rec");
x = unlist(prCurve@x.values);
y = unlist(prCurve@y.values);
df = data.frame(x = x[2:length(x)], y = y[2:length(y)]);
AUCPR  = trapz(df$x, df$y)

cat("AUCROC: ", AUCROC, " AUCPR: ", AUCPR, "\n");

cat(as.character(Sys.time()),">> Calculating enrichment ...\n");
sortOrder = order(-data$Score);
for (rank in seq(from=1, to=100, by=1)) {
  
  topRanked = round(rank * length(data[,1]) / 100, 0);
  num = sum(data[sortOrder[1:topRanked],"protection"])/topRanked;
  den = sum(data$protection) / length(data[,1]);
  cat(round(num / den, 1), "\n")
}
