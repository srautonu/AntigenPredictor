library(e1071)
library(ROCR)
library(randomForest)

source('./featurization/featurization.R');
source('./featurization/countpattern.R');
source('./featurization/findposition.R');
source('./featurefiltering.R');

origData = read.csv("viralTrainingSet.csv");
amins = c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "Y")

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

# for SVM we would like to do regression
features$protection = data$protection;
data = NULL;
cat("learningSet, in Sample error, out of sample error\n");

for (learningSet in seq(from=10, to=nTrainingSet, by=10)) {
  trainingSet = features[1:learningSet,];
  testSet = features[(nTrainingSet + 1) : (nTrainingSet + nTestSet),];
  
  if (sum(trainingSet$protection) == 0) {
    next;
  }
  
  svmmodel = svm(protection ~ ., trainingSet, kernel = "linear", cross = 10, scale = FALSE);

  # out of sample error
  svmpred = as.vector(predict(svmmodel, testSet));
  temp = svmpred - testSet$protection;
  temp = temp * temp;
  temp = sum(temp) / nTestSet;
  
  data = rbind(data, c(learningSet,  svmmodel$tot.MSE, temp));
  cat(learningSet, "," , svmmodel$tot.MSE, ",", temp, "\n");
}

write.csv(data, "learningCurve_1000_RNG10_tripeptide.csv");
cat(as.character(Sys.time()), ">> Done.\n");
