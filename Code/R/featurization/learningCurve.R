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
rngSeed = 30;

cat("seed, seq, pos, nFeatures, costSVM, Threshold, F1-Score\n")

{
  # randomly permutate the data
  set.seed(rngSeed);
  data = origData[sample(nrow(origData)),];
  nData = length(data[,1]);
  
  timestamp();
  #features = featurization(data$Sequence, amins, seqorder=3);
  #write.csv(features, "featurized.csv", row.names=FALSE);
  features = read.csv("featurized.csv");
  
  features$protection = as.factor(data$protection);
  timestamp();
  cat("Featurization is done\n");
  
  # split the data in training (LOO-CV), and test sets
  nTrainingSet = floor(nData * 0.75);
  if ((nData - nTrainingSet) %% 2 != 0)
    nTrainingSet = nTrainingSet + 1;
  nTestSet = (nData - nTrainingSet);

  # for SVM we would like to do regression
  features$protection = data$protection;
  
  for (learningSet in seq(from=10, to=nTrainingSet, by=10)) {
    trainingSet = features[1:learningSet,];
    testSet = features[(nTrainingSet + 1) : (nTrainingSet + nTestSet),];
    
    svmmodel = svm(protection ~ ., trainingSet, kernel = "linear", cross = learningSet);
    cat(learningSet, "," , svmmodel$tot.MSE, "\n");
  }
}