library(e1071)
library(ROCR)

source('featurefiltering.R');

timestamp();

set.seed(10);

fScheme = "_comb";
featureCountList = seq(from=10, to=600, by=10);
DoBalancing = FALSE;

# File names #
outFile     = "IndependentTestResults.csv";

RDSFolder          = "RDSFiles/"
rankedFeaturesFile = paste(RDSFolder, "ff_SvmRFE2"            , fScheme, ".rds", sep = "");
featureFile        = paste(RDSFolder, "featurized"    , fScheme, ".rds", sep = "");
testFeatureFile    = paste(RDSFolder, "testFeaturized", fScheme, ".rds", sep = "");

cat(as.character(Sys.time()),">> Reading feature ranking from", rankedFeaturesFile, "...\n");
rankedFeatures = readRDS(rankedFeaturesFile);
cat(as.character(Sys.time()),">> Done\n");

cat(as.character(Sys.time()),">> Reading training set features from", featureFile, "...\n");
features = readRDS(featureFile);
features$protection = as.numeric(features$protection) - 1;
features = featurefiltering(features, rankedFeatures, max(featureCountList));
if (DoBalancing) {
  
  #
  # Balance the dataset (576+576) by undersampling the negative (larger) set
  #
  positiveSet = features[sample(1:576),]
  negativeSetInd = sample(577:length(features[,1]))[1:576]
  negativeSetInd = negativeSetInd[order(negativeSetInd)]
  features = rbind(features[1:576,], features[negativeSetInd,])
}
# random shuffle of features
features <- features[sample(nrow(features)),]
cat(as.character(Sys.time()),">> Done\n");

cat(as.character(Sys.time()),">> Reading test set features from", testFeatureFile, "...\n");
testSet = readRDS(testFeatureFile);
testSet$protection = as.numeric(testSet$protection) - 1;
testSet = featurefiltering(testSet, rankedFeatures, max(featureCountList));
cat(as.character(Sys.time()),">> Done\n");

bestPerf = NULL;
bestParams = NULL;
accData = NULL;

cat(as.character(Sys.time()),">> Entering independet testing ...\n");

for (maxFeatureCount in featureCountList) 
{
  trainingSet = featurefiltering(features, rankedFeatures, maxFeatureCount);
  
  rfModel = randomForest(protection ~ ., trainingSet);
  
  rfPred = predict(rfModel, testSet);
  rfPrediction = prediction(rfPred, testSet$protection);
  
  # Find the AUCROC
  auc  = ROCR::performance(rfPrediction,"auc")@y.values[[1]];
  
  # Find optimal threshold based on F1 score or MCC
  # (Due to imbalance in the test data)
  accSeries = ROCR::performance(rfPrediction,"f");
  threshold = unlist(accSeries@x.values)[[which.max(unlist(accSeries@y.values))]];
  
  rfPrediction = prediction(as.numeric(rfPred >= threshold), testSet$protection);
  acc  = unlist(ROCR::performance(rfPrediction,"acc")@y.values)[2];
  sens = unlist(ROCR::performance(rfPrediction,"sens")@y.values)[2];
  spec = unlist(ROCR::performance(rfPrediction,"spec")@y.values)[2];
  mcc  = unlist(ROCR::performance(rfPrediction,"mat")@y.values)[2];
  prec = unlist(ROCR::performance(rfPrediction,"prec")@y.values)[2];
  f1   = unlist(ROCR::performance(rfPrediction,"f")@y.values)[2];
  
  enrichment = NULL;
  sortOrder = order(-rfPred);
  for (rank in c(2,5,10,25)) {
    
    topRanked = round(rank * length(testSet[,1]) / 100, 0);
    num = sum(testSet[sortOrder[1:topRanked],"protection"])/topRanked;
    den = sum(testSet$protection) / length(testSet[,1]);
    enrichment = c(enrichment, round(num / den, 1));
  }

  cat(
    maxFeatureCount, 
    ",", round(auc, 2),
    ",", round(threshold, 2),
    ",", round(acc, 2),
    ",", round(sens, 2),
    ",", round(spec, 2),
    ",", round(prec, 2),
    ",", round(f1, 2),
    ",", round(mcc, 2),
    ",", enrichment[1],
    ",", enrichment[2],
    ",", enrichment[3],
    ",", enrichment[4],
    "\n"
  );
  
  accData = rbind(accData, c(maxFeatureCount, auc, threshold, acc, sens, spec, prec, f1, mcc, enrichment[1], enrichment[2], enrichment[3], enrichment[4]));
  colnames(accData) = c("nF", "AUCROC", "Threshold", "Accuracy", "Sensitivity", "Specificity", "Precision", "F1" , "MCC", "%2", "%5", "%10", "%25");
  write.csv(accData, outFile);
}