library(ROCR)

source('./featurefiltering.R');
source('./learn.R')

timestamp();

set.seed(10);

TestSetType = "TUB";

maxFeatureCount = 490;
BalanceTrainingSet = TRUE;

fScheme = "_comb";

RDSFolder          = "RDSFiles/"

rankedFeaturesFile = paste(RDSFolder, "ff_SvmRFE2" , fScheme, ".rds", sep = "");
featureFile        = paste(RDSFolder, "featurized" , fScheme, ".rds", sep = "");
outFile            = paste("out", fScheme, ".csv", sep = "");

cat(as.character(Sys.time()),">> Reading training set features from", featureFile, "...\n");
features = readRDS(featureFile);
# For regression study, we need to 'unfactor' the dependent var.
# When converting from factor to numeric, Antigens becomes 2 and Non-antigens becomes 1.
# So we need to deduct 1.
features$protection = as.numeric(features$protection) - 1;
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
trainingSet = featurefiltering(trainingSet, rankedFeatures, maxFeatureCount);
testSet = featurefiltering(testSet, rankedFeatures, maxFeatureCount);

for (seed in seq(from=10, to=50, by=10)) 
{
  set.seed(seed);
  
  if (BalanceTrainingSet) {
  
    #
    # Balance the training set by undersampling the larger set
    #
    nPositive = sum(as.numeric(trainingSet$protection));
    nNegative = length(trainingSet[,1]) - nPositive;
    nBalanced = min(nPositive, nNegative);
    
    positiveSetInd = sample(1:nPositive)[1:nBalanced];
    negativeSetInd = sample((nPositive+1):length(trainingSet[,1]))[1:nBalanced];

    curTrainingSet = rbind(trainingSet[positiveSetInd,], trainingSet[negativeSetInd,])
  } else {
    curTrainingSet =  trainingSet;
  }
  
  # random shuffle of features
  curTrainingSet <- curTrainingSet[sample(nrow(curTrainingSet)),]
  
  #
  # Balance the test set (except for the "PAN" test set)
  #
  if (TestSetType != "PAN") {
    lastPositiveIndex = sum(as.numeric(testSet$protection));
    negativeSetInd = sample((lastPositiveIndex+1):length(testSet[,1]))[1:lastPositiveIndex]
    negativeSetInd = negativeSetInd[order(negativeSetInd)]
    curTestSet = rbind(testSet[1:lastPositiveIndex,], testSet[negativeSetInd,])
  } else {
    curTestSet = testSet;
  }

  cat(as.character(Sys.time()),">> Entering cross validation. TestFold = ", TestSetType, " ...\n");

  model = learn(protection ~ ., curTrainingSet, "rf");
  mlPred = predict(model, curTestSet);

  # Use a fixed threshold of 0.5
  threshold = 0.5;
  
  mlPrediction = prediction(
    as.numeric(mlPred >= threshold),
    factor(curTestSet$protection, c(0,1))
    );
  acc = unlist(ROCR::performance(mlPrediction,"acc")@y.values)[2]
  sens = unlist(ROCR::performance(mlPrediction,"sens")@y.values)[2];
  spec = unlist(ROCR::performance(mlPrediction,"spec")@y.values)[2];
  prec = unlist(ROCR::performance(mlPrediction,"prec")@y.values)[2];
  mcc = unlist(ROCR::performance(mlPrediction,"mat")@y.values)[2];
  f1  = unlist(ROCR::performance(mlPrediction,"f")@y.values)[2];
  
  cat(
      maxFeatureCount,
      ",", round(acc, 2),
      ",", round(sens, 2),
      ",", round(spec, 2),
      ",", round(prec, 2),
      ",", round(f1, 2),
      ",", round(mcc, 2)
      );
  accData = rbind(
    accData, 
    c(
      maxFeatureCount 
      , acc
      , sens
      , spec
      , prec
      , f1
      , mcc
      )
    );
  colnames(accData) = c(
    "nF"
  , "Accuracy"
  , "Sensitivity"
  , "Specificity"
  , "Precision"
  , "F1"
  , "MCC"
    );
  write.csv(accData, outFile);
  
  cat("\n");
}
