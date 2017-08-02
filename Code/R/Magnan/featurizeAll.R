library(e1071)
library(ROCR)
library(randomForest)

source('./featurization.R');
source('./featurefiltering.R');

timestamp();

set.seed(10);

fScheme = "_ngrams";

amins = c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y");

antigens = read.csv("antigens.csv");
antigens$protection = 1;

nonAntigens = read.csv("nonAntigens.csv");
nonAntigens$protection = 0;

data = rbind(antigens, nonAntigens);
nData = length(data[,1]);

featureFile = paste("featurized_", as.character(nData), fScheme, ".rds", sep = "");
svmFile     = paste("svm_", as.character(nData), fScheme, ".rds", sep = "");

cat(as.character(Sys.time()),">> Featurizing ...\n");
if (!file.exists(featureFile)) {
  features = featurization(data$Sequence, data$protection, amins, nGramOrder = 3, gap = 0, posorder = 0);
  saveRDS(features, featureFile);
  cat(as.character(Sys.time()),">> Done.\n");
} else {
  features = readRDS(featureFile);
  cat(as.character(Sys.time()),">> Done ( from cached file:", featureFile, ")\n");
}
cat(as.character(Sys.time()),">> Total features: ", length(features[1,]), "\n");
