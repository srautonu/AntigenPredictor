library(e1071)
library(ROCR)
library(randomForest)

source('./featurization/featurization.R');
source('./featurization/countpattern.R');
source('./featurization/findposition.R');
source('./featurefiltering.R');

origData = read.csv("viralTrainingSet.csv");
amins = c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "Y")

rngSeed = 30;
seqorder = 3;
posorder = 0;
nFeatures = 1260;
svmC = 0.01;
threshold = 0.35;

# randomly permutate the data
set.seed(rngSeed);
data = origData[sample(nrow(origData)),];
nData = length(data[,1]);
  
#features = featurization(data$Sequence, amins, seq = (seqorder != 0), seqorder, pos = (posorder != 0), posorder);
features = read.csv("featurized.csv");
features$protection = data$protection;
      
# split the data in training, validation and test sets
nTrainingSet = floor(nData * 0.6);
if ((nData - nTrainingSet) %% 2 != 0)
  nTrainingSet = nTrainingSet + 1;
nValidationSet = (nData - nTrainingSet) / 2;
nTestSet = nValidationSet;
      
trainingSet = features[1:nTrainingSet,];
validationSet = features[(nTrainingSet + 1) : (nTrainingSet + nValidationSet),];
testSet = features[(nTrainingSet + nValidationSet + 1) : (nTrainingSet + nValidationSet + nTestSet),];

rfmodel = readRDS("rfmodel.rds");
filteringRes = featurefiltering(trainingSet, validationSet, testSet, rfmodel, nFeatures);
trainingSet = filteringRes$trainingSet;
validationSet = filteringRes$validationSet;
testSet = filteringRes$testSet;

svmmodel = svm(protection ~ ., trainingSet, kernel = "linear", cost = svmC)
svmpred = as.vector(predict(svmmodel, validationSet));
        

svmprediction = prediction(1 * (svmpred >= threshold), testSet[,"protection"]);
# F1-score
F1 = unlist(performance(svmprediction,"f")@y.values)[2];
precision = unlist(performance(svmprediction,"prec")@y.values)[2];
recall = unlist(performance(svmprediction,"rec")@y.values)[2];
sensitiviy = unlist(performance(svmprediction,"sens")@y.values)[2];
specificity = unlist(performance(svmprediction,"spec")@y.values)[2];

cat("Model: <seed, seq, pos, nF, C, T, F1> = ", 
    rngSeed, seqorder, posorder, nFeatures, svmC, threshold, "\n");
cat("Precision(Test set): ", precision, "\n");
cat("Recall   (Test set): ", recall, "\n");
cat("F1-Score (Test set): ", F1, "\n");
cat("Sensitivity(Test set): ", sensitiviy, "\n");
cat("Specificity(Test set): ", specificity, "\n");


svmROC = performance(prediction(svmpred, testSet[,"protection"]),"tpr","fpr")
svmPerf= performance(prediction(svmpred, testSet[,"protection"]), "sens", "spec")
df = data.frame(cut = svmPerf@alpha.values[[1]], sens = svmPerf@x.values[[1]], spec = svmPerf@y.values[[1]])
bestThreshold = df[which.max(df$sens + df$spec), "cut"]

plot(
  svmROC, main = "ROC-Curve", cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis =
    1.3,yaxis.cex.axis = 1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1
)
