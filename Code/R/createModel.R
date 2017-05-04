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

bestMSE = Inf;
bestPrec = 0;
bestRecall = 0;
bestF1 = 0;
bestParams = NULL;

rngSeed = 10;

# randomly permutate the data
set.seed(rngSeed);
data = origData[sample(nrow(origData)),];
nData = length(data[,1]);

timestamp();
#features = featurization(data$Sequence, amins, seqorder=3);
#write.csv(features, "featurized.csv", row.names=FALSE);
features = read.csv("featurized_1000_RNG10_tripeptide.csv");
cat(as.character(Sys.time()),">> Featurization is done. Total features: ", length(features[1,]), "\n");

# eliminate columns that do not contain data other than 0
featureNames = colnames(features);
for (featureName in featureNames) {
  if (sum(features[,featureName]) == 0) {
    features[,featureName] = NULL;
  }
}
cat(as.character(Sys.time()),">> Removed empty features. Total features: ", length(features[1,]), "\n");

features = 1 / (1 + exp(-features));
cat(as.character(Sys.time()),">> Converted to sigmoid\n");


# split the data in training (LOO-CV), and test sets
nTrainingSet = floor(nData * 0.75);
if ((nData - nTrainingSet) %% 2 != 0)
  nTrainingSet = nTrainingSet + 1;
nTestSet = (nData - nTrainingSet);

features$protection = as.factor(data$protection);
#rfmodel = randomForest(protection ~ ., features[1:nTrainingSet,], importance=TRUE);
#saveRDS(rfmodel, "rfmodel.rds");
rfmodel = readRDS("rfmodel.rds");
cat(as.character(Sys.time()),">> Random forest is done\n");

mseData = NULL;

# for SVM we would like to do regression
features$protection = data$protection;
for (maxFeatureCount in seq(from=10, to=1000, by=10)) {
  trainingSet = features[1:nTrainingSet,];
  testSet = features[(nTrainingSet + 1) : (nTrainingSet + nTestSet),];
  
  filteringRes = featurefiltering(trainingSet, testSet, rfmodel, maxFeatureCount);
  trainingSet = filteringRes$trainingSet;
  testSet = filteringRes$testSet;
  #cat(as.character(Sys.time()),">> Feature filtering is done\n");
  
  svmCostList = c(0.01, 0.03, 0.10, 0.30, 1, 3, 10, 30, 100);
  for (svmC in svmCostList) {
    svmmodel = svm(protection ~ ., trainingSet, kernel = "linear", cost = svmC, cross = 10, scale = FALSE);
    #svmmodel = svm(protection ~ ., trainingSet, kernel = "linear", cross = 10, scale = FALSE);
    cat(maxFeatureCount, ",", svmC, ",", svmmodel$tot.MSE);
    #cat(maxFeatureCount, ",", svmmodel$tot.MSE);
    
    mseData = rbind(mseData, c(maxFeatureCount,  svmC, svmmodel$tot.MSE));
    write.csv(data, "featureSize.csv");

    if (bestMSE > svmmodel$tot.MSE) {
      bestMSE = svmmodel$tot.MSE;
      bestParams = list(
        "maxFeatureCount" = maxFeatureCount,
        "svmC" = svmC
      )
      cat(",<-- BEST");
    }
    
    cat("\n");
  }
}

# for (threshold in seq(from=0.05, to=0.95, by=0.05)) {
#   svmprediction = prediction(1 * (svmpred >= threshold), validationSet[,"protection"]);
#   # F1-score
#   F1 = unlist(performance(svmprediction,"f")@y.values)[2]
#   prec = unlist(performance(svmprediction,"prec")@y.values)[2]
#   recall = unlist(performance(svmprediction,"rec")@y.values)[2]
#   
#   
#   if (F1 > bestF1
#       || (F1 == bestF1 && prec > bestPrec)
#       || (F1 == bestF1 && prec == bestPrec && recall > bestRecall)) {
#     cat(", <-- BEST");
#     
#     bestF1 = F1;
#     bestPrec = prec;
#     bestRecall = recall;
#     bestParams = list(
#       "rngSeed" = rngSeed,
#       "maxFeatureCount" = maxFeatureCount,
#       "svmC" = svmC,
#       "threshold" = threshold
#     )
    
cat("Best Result:\n");
cat("<nF, C, MSE> = ", bestParams$maxFeatureCount, bestParams$svmC, bestMSE, "\n");
