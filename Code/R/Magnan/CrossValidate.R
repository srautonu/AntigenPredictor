library(e1071)
library(ROCR)

source('./featurefiltering.R');
source('./svmCV.R')

timestamp();

set.seed(10);

nData = 1152;
fScheme = "_ngrams";

rankedFeaturesFile = paste("ff_", as.character(nData), fScheme, ".rds", sep = "");
featureFile = paste("featurized_", as.character(nData), fScheme, ".rds", sep = "");
outFile     = paste("out_", as.character(nData), fScheme, ".csv", sep = "");

# 10 fold CV
nFolds = 10

cat(as.character(Sys.time()),">> Loading feature file ...\n");
features = readRDS(featureFile);
cat(as.character(Sys.time()),">> Done ( from cached file:", featureFile, ")\n");

features$ID = NULL;
features$Type = NULL;
# The featurization makes the protection column as factor.
# so for regression study, we need to convert it to numeric. However
# as.numeric creates values 1 and 2 for the levels 0 and 1 respectively.
# As such, we need to deduct 1.
features$protection = as.numeric(features$protection) - 1;

cat(as.character(Sys.time()),">> Total features: ", length(features[1,]) - 1, "\n");

cat(as.character(Sys.time()),">> Loading feature ranking ...\n");
rankedFeatures = readRDS(rankedFeaturesFile);
cat(as.character(Sys.time()),">> Done ( from cached file:", rankedFeaturesFile, ")\n");

# random shuffle of features
features <- features[sample(nrow(features)),]

bestPerf = NULL;
bestParams = NULL;
accData = NULL;

svmCostList = c(0.01, 0.1, 1, 10, 100);
featureCountList = seq(from=3000, to=500, by=-100); 

cat(as.character(Sys.time()),">> Entering cross validation. Folds = ", nFolds, " ...\n");

for (maxFeatureCount in featureCountList) 
{
  filteringRes = featurefiltering(features, NULL, rankedFeatures, maxFeatureCount);
  trainingSet = filteringRes$trainingSet;

  for (svmC in svmCostList) 
  {
    perf = svmCV(protection ~ ., trainingSet, svmCost = svmC, cross = nFolds);

    cat(maxFeatureCount, ",", svmC, ",", perf$auc, ",", perf$acc, ",", perf$sens, ",", perf$spec, ",", perf$mcc);
    
    accData = rbind(accData, c(maxFeatureCount, svmC, perf$auc, perf$acc, perf$sens, perf$spec, perf$mcc));
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
cat("AUCROC     : ", bestPerf$auc, "\n");
cat("Accuracy   : ", bestPerf$acc, "\n");
cat("Sensitivity: ", bestPerf$sens, "\n");
cat("Specificity: ", bestPerf$spec, "\n")
cat("MCC        : ", bestPerf$mcc, "\n")
