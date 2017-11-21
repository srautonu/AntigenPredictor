library(e1071)
library(ROCR)

source('./featurefiltering.R');
source('./learnWithCV.R')

timestamp();

set.seed(10);
DoBalancing = TRUE;
TestSetType = "PAN";

featureCountList = seq(from=10, to=600, by=10);

fScheme = "_heu_comb";

RDSFolder          = "RDSFiles/"

rankedFeaturesFile = paste(RDSFolder, "ff_SvmRFE2" , fScheme, ".rds", sep = "");
featureFile        = paste(RDSFolder, "featurized" , fScheme, ".rds", sep = "");
outFile            = paste("out", fScheme, ".csv", sep = "");

cat(as.character(Sys.time()),">> Reading training set features from", featureFile, "...\n");
features = readRDS(featureFile);
cat(as.character(Sys.time()),">> Done\n");

cat(as.character(Sys.time()),">> Reading feature ranking from", rankedFeaturesFile, "...\n");
rankedFeatures = readRDS(rankedFeaturesFile);
cat(as.character(Sys.time()),">> Done\n");

trainingSet = features[-which (features$Type == TestSetType),]
testSet = features[which (features$Type == TestSetType),]

bestPerf = NULL;
bestParams = NULL;
accData = NULL;

# Reduce the feature vectors to the max size that we will be testing.
# This way the filtering cost in the loop below will be reduced.
trainingSet = featurefiltering(trainingSet, rankedFeatures, max(featureCountList));
testSet = featurefiltering(testSet, rankedFeatures, max(featureCountList));

if (DoBalancing) {
  
  lastPositiveIndex = sum(as.numeric(trainingSet$protection) - 1);
  
  #
  # Balance the dataset by undersampling the negative (larger) set
  #
  negativeSetInd = sample((lastPositiveIndex+1):length(trainingSet[,1]))[1:lastPositiveIndex]
  negativeSetInd = negativeSetInd[order(negativeSetInd)]
  trainingSet    = rbind(trainingSet[1:lastPositiveIndex,], trainingSet[negativeSetInd,])
}

# random shuffle of features
trainingSet <- trainingSet[sample(nrow(trainingSet)),]

cat(as.character(Sys.time()),">> Entering cross validation. TestFold = ", TestSetType, " ...\n");

# For regression study, we need to 'unfactor' the dependent var.
# When converting from factor to numeric, Antigens becomes 2 and Non-antigens becomes 1.
# So we need to deduct 1.
features$protection = as.numeric(features$protection) - 1;

for (maxFeatureCount in featureCountList) 
{
  trainingSet = featurefiltering(features, rankedFeatures, maxFeatureCount);

  model = learn(protection ~ ., trainingSet, "rf");
  
  mlPrediction = prediction(predVector, testSet$protection);
  
  # Find the ROC curve and AUCROC
  AUCROC  = ROCR::performance(mlPrediction,"auc")@y.values[[1]];
  rocCurve = ROCR::performance(mlPrediction,"tpr", "fpr");
  
  # Find the PR curve and AUCPR
  prCurve  = ROCR::performance(mlPrediction,"prec", "rec");
  x = unlist(prCurve@x.values);
  y = unlist(prCurve@y.values);
  df = data.frame(x = x[2:length(x)], y = y[2:length(y)]);
  AUCPR  = trapz(df$x, df$y)
  
  # Find optimal threshold based on accuracy
  # accSeries = ROCR::performance(mlPrediction,"acc");
  # threshold = unlist(accSeries@x.values)[[which.max(unlist(accSeries@y.values))]];
  
  # Use a fixed threshold of 0.5
  threshold = 0.5;
  
  mlPrediction = prediction(as.numeric(predVector >= threshold), data[, dependentVar]);
  
  acc = unlist(ROCR::performance(mlPrediction,"acc")@y.values)[2]
  sensitivity = unlist(ROCR::performance(mlPrediction,"sens")@y.values)[2];
  specificity = unlist(ROCR::performance(mlPrediction,"spec")@y.values)[2];
  precision   = unlist(ROCR::performance(mlPrediction,"prec")@y.values)[2];
  mcc = unlist(ROCR::performance(mlPrediction,"mat")@y.values)[2];
  f1  = unlist(ROCR::performance(mlPrediction,"f")@y.values)[2];
  
    
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

cat("Best Result for nF = ", bestParams$maxFeatureCount, "\n");
cat("AUCROC      : ", bestPerf$auc, "\n");
cat("Threshold   : ", bestPerf$threshold, "\n");
cat("Accuracy    : ", bestPerf$acc, "\n");
cat("Sensitivity : ", bestPerf$sens, "\n");
cat("Specificity : ", bestPerf$spec, "\n");
cat("MCC         : ", bestPerf$mcc, "\n");
