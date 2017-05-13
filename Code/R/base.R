library(e1071)
library(ROCR)
library(randomForest)

source('./featurization/featurization.R');
source('./featurization/countpattern.R');
source('./featurization/findposition.R');
source('./featurefiltering.R');

rngSeed = 10;


amins = c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y");

antigens = read.csv("viralAntigens.csv");
nonAntigens = read.csv("viralNonAntigens.csv");
data = rbind(antigens, nonAntigens[1:length(antigens[,1]),]);

timestamp();

# randomly permutate the data
cat(as.character(Sys.time()),">> Random permutation of data:\n");
set.seed(rngSeed);
nData = length(data[,1]);
data = data[sample(1:nData),];
# split the data in training (LOO-CV), and test sets
nTrainingSet = floor(nData * 0.75);
if ((nData - nTrainingSet) %% 2 != 0)
  nTrainingSet = nTrainingSet + 1;
nTestSet = (nData - nTrainingSet);
cat(as.character(Sys.time()),">> Training set antigen ratio", sum(data[1:nTrainingSet, "protection"])/nTrainingSet, "\n");
cat(as.character(Sys.time()),">> Test set antigen ratio", sum(data[(nTrainingSet+1):nData, "protection"])/nTestSet, "\n");

# File names #
fScheme = "_posTrimer";
rfmodelFile = paste("rfmodel_", as.character(nData), fScheme, ".rds", sep = "");
svmFile     = paste("svm_", as.character(nData), fScheme, ".rds", sep = "");
featureFile = paste("featurized_", as.character(nData), fScheme, ".rds", sep = "");
outFile     = paste("out_", as.character(nData), fScheme, ".csv", sep = "");
lcFile      = paste("lc_", as.character(nData), fScheme, ".csv", sep = "");     

cat(as.character(Sys.time()),">> Featurizing ...\n");
if (!file.exists(featureFile)) {
  #features = featurization(data$Sequence, data$protection, amins, seqorder=0, gap = 25, posorder = 0);
  features = featurization(data$Sequence, data$protection, amins, seqorder = 0, gap = 0, posorder = 3);
  saveRDS(features, featureFile);
  write.csv(features, "featurized.csv");
  cat(as.character(Sys.time()),">> Done.\n");
} else {
  features = readRDS(featureFile);
  cat(as.character(Sys.time()),">> Done ( from cached file:", featureFile, ")\n");
}
cat(as.character(Sys.time()),">> Total features: ", length(features[1,]), "\n");

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
