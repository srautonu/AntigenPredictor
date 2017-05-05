library(e1071)
library(ROCR)
library(randomForest)

source('./featurization/featurization.R');
source('./featurization/countpattern.R');
source('./featurization/findposition.R');
source('./featurefiltering.R');

origData = read.csv("viralTrainingSet.csv");
amins = c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y");

rngSeed = 10;
nFeatures = 600;
svmC = 1;

# randomly permutate the data
set.seed(rngSeed);
data = origData[sample(nrow(origData)),];
nData = length(data[,1]);
  
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
      
# split the data in training, validation and test sets
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

# for SVM we would like to do regression
features$protection = data$protection;
trainingSet = features[1:nTrainingSet,];
testSet = features[(nTrainingSet + 1) : (nTrainingSet + nTestSet),];

filteringRes = featurefiltering(trainingSet, testSet, rfmodel, nFeatures);
trainingSet = filteringRes$trainingSet;
testSet = filteringRes$testSet;

svmmodel = svm(protection ~ ., trainingSet, kernel = "linear", cost = svmC, cross = 10, scale = FALSE);
saveRDS(svmmodel, "svmmodel.rds");
#svmmodel = readRDS("svmmodel.rds");
svmpred = as.vector(predict(svmmodel, testSet));

svmROC = performance(prediction(svmpred, testSet[,"protection"]),"tpr","fpr")
svmPerf= performance(prediction(svmpred, testSet[,"protection"]), "sens", "spec")
df = data.frame(cut = svmPerf@alpha.values[[1]], sens = svmPerf@x.values[[1]], spec = svmPerf@y.values[[1]])
bestThreshold = df[which.max(df$sens + df$spec), "cut"]

plot(
  svmROC, main = "ROC-Curve", cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis =
    1.3,yaxis.cex.axis = 1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1
)

svmprediction = prediction(1 * (svmpred >= bestThreshold), testSet[,"protection"]);
# F1-score
F1 = unlist(performance(svmprediction,"f")@y.values)[2]
prec = unlist(performance(svmprediction,"prec")@y.values)[2]
recall = unlist(performance(svmprediction,"rec")@y.values)[2]
sensitiviy = unlist(performance(svmprediction,"sens")@y.values)[2];
specificity = unlist(performance(svmprediction,"spec")@y.values)[2];

cat("Model: <seed, seq, pos, nF, C, T, F1> = ", 
    rngSeed, seqorder, posorder, nFeatures, svmC, threshold, "\n");
cat("Precision(Test set): ", precision, "\n");
cat("Recall   (Test set): ", recall, "\n");
cat("F1-Score (Test set): ", F1, "\n");
cat("Sensitivity(Test set): ", sensitiviy, "\n");
cat("Specificity(Test set): ", specificity, "\n");