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

bestF1 = 0;
bestParams = NULL;

cat("seed, seq, pos, nFeatures, costSVM, Threshold, F1-Score\n")

#for (rngSeed in seq(from=10, to=100, by=10)) {
for (rngSeed in seq(from=30, to=30, by=10)) {
  # randomly permutate the data
  set.seed(rngSeed);
  data = origData[sample(nrow(origData)),];
  nData = length(data[,1]);
  
  for (posorder in 0:0) {
    for (seqorder in 3:3) {
      #features = featurization(data$Sequence, amins, seq = (seqorder != 0), seqorder, pos = (posorder != 0), posorder);
      #write.csv(features, "featurized.csv", row.names=FALSE);
      #features$protection = data$protection;
      features = read.csv("featurized.csv");
      features$protection = as.factor(data$protection);
      
      # split the data in training, validation and test sets
      nTrainingSet = floor(nData * 0.6);
      if ((nData - nTrainingSet) %% 2 != 0)
        nTrainingSet = nTrainingSet + 1;
      nValidationSet = (nData - nTrainingSet) / 2;
      nTestSet = nValidationSet;
      
      trainingSet = features[1:nTrainingSet,];
      validationSet = features[(nTrainingSet + 1) : (nTrainingSet + nValidationSet),];
      testSet = features[(nTrainingSet + nValidationSet + 1) : (nTrainingSet + nValidationSet + nTestSet),];
      
      # we will take featuers no more than 10 * trainingSet size
      temp1 = 10 * nrow(trainingSet);
      temp2 = nrow(trainingSet);
      
      cat("Line 56\n");
      
      for (maxFeatureCount in seq(from=temp2, to=temp1, by= 2 * temp2)) {
        filteringRes = featurefiltering(trainingSet, validationSet, testSet, protection ~ ., maxFeatureCount);
        
        cat("Line 61\n");
        
        trainingSet = filteringRes$trainingSet;
        validationSet = filteringRes$validationSet;
        testSet = filteringRes$testSet;
        
        
        svmCostList = c(0.01, 0.03, 0.10, 0.30, 1, 3, 10, 30, 100);
        for (svmC in svmCostList) {
          svmmodel = svm(protection ~ ., trainingSet, kernel = "linear", cost = svmC)
          svmpred = as.vector(predict(svmmodel, validationSet));
          
          for (threshold in seq(from=0.05, to=0.95, by=0.05)) {
            svmprediction = prediction(1 * (svmpred >= threshold), validationSet[,"protection"]);
            # F1-score
            F1 = unlist(performance(svmprediction,"f")@y.values)[2]
            cat(rngSeed, seqorder, posorder, maxFeatureCount, svmC, threshold, F1);
            if (F1 > bestF1) {
              cat(", <-- BEST");

              bestF1 = F1;
              bestParams = list(
                "rngSeed" = rngSeed,
                "seqorder" = seqorder,
                "posorder" = posorder,
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
  }
}

cat("Best Result:\n");
cat("<seed, seq, pos, nF, C, T, F1> = ", 
    bestParams$rngSeed, 
    bestParams$seqorder, 
    bestParams$posorder, 
    bestParams$maxFeatureCount, 
    bestParams$svmC, 
    bestParams$threshold, 
    bestF1,
    "\n"
    );