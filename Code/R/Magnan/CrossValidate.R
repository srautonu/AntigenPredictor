library(e1071)
library(ROCR)

source('./featurefiltering.R');
source('./svmCV.R')

timestamp();

set.seed(10);

DoRegression = TRUE;
svmCostList = c(0.3, 1, 3, 10, 30, 100);
featureCountList = seq(from=2800, to=1500, by=-50);

# 10 fold CV
nFolds = 10

fScheme = "_comb";

rankedFeaturesFile = paste("ff"         , fScheme, ".rds", sep = "");
featureFile        = paste("featurized" , fScheme, ".rds", sep = "");
outFile            = paste("out"        , fScheme, ".csv", sep = "");

cat(as.character(Sys.time()),">> Reading training set features from", featureFile, "...\n");
features = readRDS(featureFile);
cat(as.character(Sys.time()),">> Done\n");

cat(as.character(Sys.time()),">> Reading feature ranking from", rankedFeaturesFile, "...\n");
rankedFeatures = readRDS(rankedFeaturesFile);
cat(as.character(Sys.time()),">> Done\n");

# jackknife
if (nFolds < 0) {
  nFolds = length(features[,1])
}

#
# Balance the dataset (576+576) by undersampling the negative (larger) set
#
positiveSet = features[sample(1:576),]
negativeSetInd = sample(577:length(features[,1]))[1:576]
negativeSetInd = negativeSetInd[order(negativeSetInd)]
features = rbind(features[1:576,], features[negativeSetInd,])

# random shuffle of features
features <- features[sample(nrow(features)),]

bestPerf = NULL;
bestParams = NULL;
accData = NULL;

cat(as.character(Sys.time()),">> Entering cross validation. Folds = ", nFolds, " ...\n");

# Reduce the feature vectors to the max size that we will be testing.
# This way the filtering cost in the loop below will be reduced.
features = featurefiltering(features, rankedFeatures, max(featureCountList));

# For regression study, we need to 'unfactor' the dependent var.
#
if (DoRegression) {
  # When converting from factor to numeric, Antigens becomes 2 and Non-antigens becomes 1.
  # So we need to deduct 1.
  features$protection = as.numeric(features$protection) - 1;
}

for (maxFeatureCount in featureCountList) 
{
  trainingSet = featurefiltering(features, rankedFeatures, maxFeatureCount);

  for (svmC in svmCostList) 
  {
    perf = svmCV(protection ~ ., trainingSet, svmCost = svmC, cross = nFolds);
    if (DoRegression) {
      cat(maxFeatureCount, ",", svmC, ",", perf$auc,  ",",  perf$threshold, ",", perf$acc, ",", perf$spec, ",", perf$sens, ",", perf$mcc);
      accData = rbind(accData, c(maxFeatureCount, svmC, perf$auc, perf$threshold, perf$acc, perf$spec, perf$sens, perf$mcc));
    } else {
      
      cat(maxFeatureCount, ",", svmC, ",", perf$acc, ",", perf$sens, ",", perf$spec, ",", perf$mcc);
      accData = rbind(accData, c(maxFeatureCount, svmC, perf$acc, perf$sens, perf$spec, perf$mcc));
    }
    
    write.csv(accData, outFile);
    
    if (is.null(bestPerf) || bestPerf$acc < perf$acc) {
      bestPerf = perf;
      bestParams = list(
        "maxFeatureCount" = maxFeatureCount,
        "svmC" = svmC
      )
      cat(",<-- BEST");
    }
    
    cat("\n");
  }
}

cat("Best Result for <nF, C> = ", bestParams$maxFeatureCount, bestParams$svmC, "\n");
if (DoRegression) {
  cat("AUCROC      : ", bestPerf$auc, "\n");
  cat("Threshold   : ", bestPerf$threshold, "\n");
}

cat("Accuracy    : ", bestPerf$acc, "\n");
cat("Sensitivity : ", bestPerf$sens, "\n");
cat("Specificity : ", bestPerf$spec, "\n");
cat("MCC         : ", bestPerf$mcc, "\n");