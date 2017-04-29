library(e1071)
library(ROCR)
library(randomForest)

source('./featurization/featurization.R');
source('./featurization/countpattern.R');
source('./featurization/findposition.R');
source('./featurefiltering.R');

origData = read.csv("viralTrainingSet.csv");
amins = c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "Y")

# LOOPING SHOULD START HERE ON VALUES OF SEQORDER AND POSORDER
# Parameter values to be validated through validation testing:
# 1. n-peptide
# 2. Position specific featurization
# 3. Maximum feature count
# 4. SVM Cost parameter (regularization)
# 5. Threshold

bestPrec = 0;
bestRecall = 0;
bestF1 = 0;
bestParams = NULL;

cat("seed, seq, pos, nFeatures, costSVM, Threshold, F1-Score\n")

#for (rngSeed in seq(from=10, to=100, by=10)) {
for (rngSeed in seq(from=30, to=30, by=10)) {
  # randomly permutate the data
  set.seed(rngSeed);
  data = origData[sample(nrow(origData)),];
  nData = length(data[,1]);
  
  timestamp();
  features = featurization(data$Sequence, amins, seqorder=1);
  write.csv(features, "featurized.csv", row.names=FALSE);
  #features = read.csv("featurized.csv");

  features$protection = as.factor(data$protection);
  timestamp();
  cat("Featurization is done\n");
  
  # split the data in training, validation and test sets
  nTrainingSet = floor(nData * 0.6);
  if ((nData - nTrainingSet) %% 2 != 0)
    nTrainingSet = nTrainingSet + 1;
  nValidationSet = (nData - nTrainingSet) / 2;
  nTestSet = nValidationSet;
  
  #timestamp();
  #rfmodel = randomForest(protection ~ ., features[1:nTrainingSet,], importance=TRUE);
  #saveRDS(rfmodel, "rfmodel.rds");
  #rfmodel = readRDS("rfmodel.rds");
  #timestamp();
  #cat("Random forest is done\n");
  
  # for SVM we would like to do regression
  features$protection = data$protection;
  
  #for (maxFeatureCount in seq(from=nTrainingSet, to=10*nTrainingSet, by= nTrainingSet)) {
  {
    trainingSet = features[1:nTrainingSet,];
    validationSet = features[(nTrainingSet + 1) : (nTrainingSet + nValidationSet),];
    testSet = features[(nTrainingSet + nValidationSet + 1) : (nTrainingSet + nValidationSet + nTestSet),];
    
    #filteringRes = featurefiltering(trainingSet, validationSet, testSet, rfmodel, maxFeatureCount);
    #trainingSet = filteringRes$trainingSet;
    #validationSet = filteringRes$validationSet;
    #testSet = filteringRes$testSet;
    #cat("Feature filtering is done\n");
    
    
    svmCostList = c(0.01, 0.03, 0.10, 0.30, 1, 3, 10, 30, 100);
    for (svmC in svmCostList) {
      svmmodel = svm(protection ~ ., trainingSet, kernel = "linear", cost = svmC)
      svmpred = as.vector(predict(svmmodel, validationSet));
      cat("SVM learning is done.\n");
      
      for (threshold in seq(from=0.05, to=0.95, by=0.05)) {
        svmprediction = prediction(1 * (svmpred >= threshold), validationSet[,"protection"]);
        # F1-score
        F1 = unlist(performance(svmprediction,"f")@y.values)[2]
        prec = unlist(performance(svmprediction,"prec")@y.values)[2]
        recall = unlist(performance(svmprediction,"rec")@y.values)[2]

        cat(rngSeed, maxFeatureCount, svmC, threshold, prec, recall, F1);
        if (F1 > bestF1
            || (F1 == bestF1 && prec > bestPrec)
            || (F1 == bestF1 && prec == bestPrec && recall > bestRecall)) {
          cat(", <-- BEST");

          bestF1 = F1;
          bestPrec = prec;
          bestRecall = recall;
          bestParams = list(
            "rngSeed" = rngSeed,
            "maxFeatureCount" = maxFeatureCount,
            "svmC" = svmC,
            "threshold" = threshold
          )
        }
        cat("\n")
      }
    }
  }
}

cat("Best Result:\n");
cat("<seed, nF, C, T, prec, recall, F1> = ", 
    bestParams$rngSeed, 
    bestParams$maxFeatureCount, 
    bestParams$svmC, 
    bestParams$threshold,
    bestPrec,
    bestRecall,
    bestF1,
    "\n"
    );