library(e1071)
library(ROCR)

source('./featurefiltering.R');
source('./learnWithCV.R')

timestamp();

set.seed(10);
DoBalancing = TRUE;

featureCountList = seq(from=10, to=600, by=10);

# 10 fold CV
nFolds = 10

fScheme = "_comb";

RDSFolder          = "RDSFiles/"

rankedFeaturesFile = paste(RDSFolder, "ff_SvmRFE2"         , fScheme, ".rds", sep = "");
featureFile        = paste(RDSFolder, "featurized" , fScheme, ".rds", sep = "");
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

bestPerf = NULL;
bestParams = NULL;
accData = NULL;

cat(as.character(Sys.time()),">> Entering cross validation. Folds = ", nFolds, " ...\n");

# Reduce the feature vectors to the max size that we will be testing.
# This way the filtering cost in the loop below will be reduced.
features = featurefiltering(features, rankedFeatures, max(featureCountList));

# For regression study, we need to 'unfactor' the dependent var.
# When converting from factor to numeric, Antigens becomes 2 and Non-antigens becomes 1.
# So we need to deduct 1.
features$protection = as.numeric(features$protection) - 1;

rocCurvePoints = NULL;
prCurvePoints = NULL;

for (maxFeatureCount in featureCountList) 
{
  trainingSet = featurefiltering(features, rankedFeatures, maxFeatureCount);

  perf = learnWithCV(protection ~ ., trainingSet, cross = nFolds, "rf");
  
  df = data.frame(
    x = unlist(perf$rocCurve@x.values), 
    y = unlist(perf$rocCurve@y.values), 
    Features = as.character(maxFeatureCount)
  );
  rocCurvePoints = rbind(rocCurvePoints, df);
  
  df = data.frame(
    x = unlist(perf$prCurve@x.values), 
    y = unlist(perf$prCurve@y.values), 
    Features = as.character(maxFeatureCount)
  );
  prCurvePoints = rbind(prCurvePoints, df);
    
  cat(
      maxFeatureCount,
      ",", round(perf$AUCROC, 2),
      ",", round(perf$AUCPR, 2),
      ",", round(perf$acc, 2),
      ",", round(perf$sens, 2),
      ",", round(perf$spec, 2),
      ",", round(perf$prec, 2),
      ",", round(perf$f1, 2),
      ",", round(perf$mcc, 2)
      );
  accData = rbind(
    accData, 
    c(
      maxFeatureCount 
      , perf$AUCROC
      , perf$AUCPR
      , perf$acc
      , perf$sens
      , perf$spec
      , perf$prec
      , perf$f1
      , perf$mcc
      )
    );
  colnames(accData) = c(
    "nF"
    , "AUCROC"
    , "AUCPR"
    , "Accuracy"
    , "Sensitivity"
    , "Specificity"
    , "Precision"
    , "F1"
    , "MCC"
    );
  write.csv(accData, outFile);
  
  if (is.null(bestPerf) || bestPerf$mcc < perf$mcc) {
    bestPerf = perf;
    bestParams = list(
      "maxFeatureCount" = maxFeatureCount
    )
    cat(",<-- BEST");
  }
  
  cat("\n");
}

saveRDS(rocCurvePoints, "rocData.rds");
saveRDS(prCurvePoints , "prData.rds");

cat("Best Result for nF = ", bestParams$maxFeatureCount, "\n");
cat("AUCROC      : ", bestPerf$auc, "\n");
cat("Threshold   : ", bestPerf$threshold, "\n");
cat("Accuracy    : ", bestPerf$acc, "\n");
cat("Sensitivity : ", bestPerf$sens, "\n");
cat("Specificity : ", bestPerf$spec, "\n");
cat("MCC         : ", bestPerf$mcc, "\n");
