library(e1071)
library(ROCR)
library(randomForest)

source('./featurization/featurization.R');
source('./featurization/countpattern.R');
source('./featurization/findposition.R');
source('./featurefiltering.R');

rngSeed = 10;
nData = 200;

schemes = c("trimer","posTrimer", "gappedDPC");

imp_combined = NULL;

for (fScheme in schemes) {
  # File names #
  rfmodelFile = paste("rfmodel_", as.character(nData), fScheme, ".rds", sep = "");
  rfmodel = readRDS(rfmodelFile);
  imp_combined = c(imp_combined, rfmodel$importance[,3]);
  
  featureFile = paste("featurized_", as.character(nData), fScheme, ".rds", sep = "");
  features = readRDS(featureFile);
  cat(as.character(Sys.time()),">> Done ( from cached file:", featureFile, ")\n");
  
  
  
  
}

rankedFeatures = rownames(imp_combined[order(-imp_combined)]);

# svmFile     = paste("svm_", as.character(nData), fScheme, ".rds", sep = "");
# featureFile = paste("featurized_", as.character(nData), fScheme, ".rds", sep = "");
# outFile     = paste("out_", as.character(nData), fScheme, ".csv", sep = "");
# lcFile      = paste("lc_", as.character(nData), fScheme, ".csv", sep = "");     




imp = rf_posTrimer$importance[order(-rf_posTrimer$importance[,3]),3];


origData = read.csv("viralTrainingSet.csv");
amins = c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y");

# randomly permutate the data
set.seed(rngSeed);
data = origData[sample(1:nData),];

timestamp();
cat(as.character(Sys.time()),">> Featurizing ...\n");
if (!file.exists(featureFile)) {
  features = featurization(data$Sequence, data$protection, amins, seqorder=0, gap = 25, posorder = 0);
  saveRDS(features, featureFile);
  write.csv(features, "featurized.csv");
  cat(as.character(Sys.time()),">> Done.\n");
} else {
  features = readRDS(featureFile);
  cat(as.character(Sys.time()),">> Done ( from cached file:", featureFile, ")\n");
}
cat(as.character(Sys.time()),">> Total features: ", length(features[1,]), "\n");

# split the data in training (LOO-CV), and test sets
nTrainingSet = floor(nData * 0.75);
if ((nData - nTrainingSet) %% 2 != 0)
  nTrainingSet = nTrainingSet + 1;
nTestSet = (nData - nTrainingSet);

cat(as.character(Sys.time()),">> Computing random forest ...\n");
if (!file.exists(rfmodelFile)) {
  rfmodel = randomForest(protection ~ ., features[1:nTrainingSet,], importance=TRUE);
  saveRDS(rfmodel, rfmodelFile);
  cat(as.character(Sys.time()),">> Done.\n");
} else {
  rfmodel = readRDS(rfmodelFile);
  cat(as.character(Sys.time()),">> Done ( from cached file:", rfmodelFile, ")\n");
}

rankedFeatures = rownames(rfmodel$importance[order(-rfmodel$importance[,3]),]);
