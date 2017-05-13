library(e1071)
library(ROCR)
library(randomForest)

source('./featurefiltering.R');

rngSeed = 10;
nData = 200;

featureFile = paste("featurized_", as.character(nData), "_hybrid", ".rds", sep = "");
svmFile     = paste("svm_", as.character(nData), "_hybrid", ".rds", sep = "");
fFilterFile = paste("ff_",  as.character(nData), "_hybrid", ".rds", sep = ""); 
outFile     = paste("out_", as.character(nData), "_hybrid", ".csv", sep = "");
lcFile      = paste("lc_",  as.character(nData), "_hybrid", ".csv", sep = "");   

schemes = c("_trimer","_posTrimer", "_gappedDPC");

if (file.exists(featureFile)) {
  features = readRDS(featureFile);
  cat(as.character(Sys.time()),">> Features loaded from:", featureFile, "\n");
} else {
  
  cat(as.character(Sys.time()),">> Combining features ...\n");
  features = NULL;
  features$Serial = seq.int(nData);
  for (fScheme in schemes) {
    curFeatureFile = paste("featurized_", as.character(nData), fScheme, ".rds", sep = "");
    #for (colname in colnames(curFea))
    curFeatures = readRDS(curFeatureFile);
    curFeatures$Serial <- seq.int(nrow(curFeatures))
    cat(as.character(Sys.time()),">> Read file: ", curFeatureFile, "\n");
    features = merge(features, curFeatures, by="Serial");
  }
  features$Serial = NULL;
  saveRDS(features, featureFile);
  cat(as.character(Sys.time()),">> Combining features is Done.\n");
}
cat(as.character(Sys.time()),">> Total features: ", length(features[1,]), "\n");

if (file.exists(fFilterFile)) {
  rankedFeatures = readRDS(fFilterFile);
  cat(as.character(Sys.time()),">> Feature filter loaded from:", fFilterFile, "\n");
} else {
  rankedFeatures = NULL;
  cat(as.character(Sys.time()),">> Combining random forests ...\n");
  for (fScheme in schemes) {
    rfmodelFile = paste("rfmodel_", as.character(nData), fScheme, ".rds", sep = "");
    rfmodel = readRDS(rfmodelFile);
    cat(as.character(Sys.time()),">> Read file: ", rfmodelFile, "\n");
    rankedFeatures = rbind(rankedFeatures, rfmodel$importance);
  }

  rankedFeatures = rownames(rankedFeatures[order(-rankedFeatures[,3]),]);
  saveRDS(rankedFeatures, fFilterFile);
  cat(as.character(Sys.time()),">> Combining random forests is Done.\n");
}

# split the data in training (LOO-CV), and test sets
nTrainingSet = floor(nData * 0.75);
if ((nData - nTrainingSet) %% 2 != 0)
  nTrainingSet = nTrainingSet + 1;
nTestSet = (nData - nTrainingSet);