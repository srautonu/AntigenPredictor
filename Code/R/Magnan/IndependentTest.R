library(e1071)
library(ROCR)

source('featurefiltering.R');

timestamp();

set.seed(10);

fScheme = "_PSF10";
maxFeatureCount = 800;
svmC = 0.3;
resultsFileName = "IndependentTestResults.csv"

# File names #
rankedFeaturesFile = paste("ff"            , fScheme, ".rds", sep = "");
featureFile        = paste("featurized"    , fScheme, ".rds", sep = "");
testFeatureFile    = paste("testFeaturized", fScheme, ".rds", sep = "");
svmFile            = paste("svm"           , fScheme, ".rds", sep = "");

cat(as.character(Sys.time()),">> Loading feature ranking ...\n");
rankedFeatures = readRDS(rankedFeaturesFile);
cat(as.character(Sys.time()),">> Done ( from cached file:", rankedFeaturesFile, ")\n");

if (!file.exists(svmFile)) {
  cat(as.character(Sys.time()),">> Building SVM model for <maxFeatureCount, svmC> = <", maxFeatureCount, ", ", svmC, "> ...\n");

  features = readRDS(featureFile);
  features$ID = NULL;
  features$Type = NULL;
  features$protection = as.numeric(features$protection) - 1;
  # random shuffle of features
  features <- features[sample(nrow(features)),]
  cat(as.character(Sys.time()),">> Training set features loaded from", featureFile, "\n");
  
  trainingSet = featurefiltering(features, rankedFeatures, maxFeatureCount);
  cat(as.character(Sys.time()),">> Training set Features filtered.\n");
  
  svmModel = svm(protection ~ ., trainingSet, kernel = "linear", cost = svmC, scale = TRUE);
  saveRDS(svmModel, svmFile);
  cat(as.character(Sys.time()),">> Model built.\n");
} else {
  cat(as.character(Sys.time()),">> Loading SVM model from ", svmFile, " ... \n");
  svmModel = readRDS(svmFile);
  maxFeatureCount = length(svmModel$SV[1,]);
  svmC = svmModel$cost;
  cat(as.character(Sys.time()),">> Done.  <maxFeatureCount, svmC> = <", maxFeatureCount, ", ", svmC, ">\n");
}

cat(as.character(Sys.time()),">> Loading Test set features ...\n");
features = readRDS(testFeatureFile);
features$ID = NULL;
features$Type = NULL;
features$protection = as.numeric(features$protection) - 1;
cat(as.character(Sys.time()),">> Done ( from cached file:", testFeatureFile, ")\n");

cat(as.character(Sys.time()),">> Filtering Test set features ...\n");
testSet = featurefiltering(features, rankedFeatures, maxFeatureCount);
cat(as.character(Sys.time()),">> Test set Features filtered.\n");

cat(as.character(Sys.time()),">> Predicting ...\n");
svmpred = predict(svmModel, testSet);
svmprediction = prediction(svmpred, testSet$protection);
cat(as.character(Sys.time()),">> Done.\n");

cat(as.character(Sys.time()),">> Recording performance in ", resultsFileName, "  ...\n");
itData = data.frame(matrix(ncol = 5, nrow = 0))
for (threshold in seq(from=0.01, to=0.99, by=0.01)) {

  svmprediction = prediction(as.numeric(svmpred >= threshold), testSet$protection);
  acc  = unlist(ROCR::performance(svmprediction,"acc")@y.values)[2];
  sens = unlist(ROCR::performance(svmprediction,"sens")@y.values)[2];
  spec = unlist(ROCR::performance(svmprediction,"spec")@y.values)[2];
  mcc  = unlist(ROCR::performance(svmprediction,"mat")@y.values)[2];
  prec = unlist(ROCR::performance(svmprediction,"prec")@y.values)[2];

  itData = rbind(itData, c(threshold, acc, sens, spec, mcc, prec));
}

colnames(itData) = c("Threshold", "Accuracy", "Sensitivity", "Specificity", "MCC", "Precision");
write.csv(t(itData), resultsFileName);
cat(as.character(Sys.time()),">> Done.\n");

cat(as.character(Sys.time()),">> Calculating enrichment ...\n");
sortOrder = order(-svmpred);

#for (rank in c(2,5,10,25)) {
for (rank in seq(from=1, to=100, by=1)) {

  topRanked = round(rank * length(testSet[,1]) / 100, 0);
  num = sum(testSet[sortOrder[1:topRanked],"protection"])/topRanked;
  den = sum(testSet$protection) / length(testSet[,1]);
  cat(round(num / den, 1), "\n")
}
