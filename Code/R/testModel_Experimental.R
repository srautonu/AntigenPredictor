source('./base.R');
source('./filteredFeaturization.R');

cat(as.character(Sys.time()),">> Reading SVM model from", svmFile, "...\n");
svmmodel = readRDS(svmFile);
cat(as.character(Sys.time()),">> Done\n");

nFeatures = length(svmmodel$SV[1,]);

trainingSet = features[1:nTrainingSet,];
testSet = features[(nTrainingSet + 1) : (nTrainingSet + nTestSet),];

filteringRes = featurefiltering(trainingSet, testSet, rankedFeatures, nFeatures);
testSet = filteringRes$testSet;

testSet2 = filteredFeaturization(
  data[(nTrainingSet + 1) : (nTrainingSet + nTestSet), "Sequence"], 
  colnames(svmmodel$SV)
);

svmpred = predict(svmmodel, testSet);
svmprediction = prediction(as.numeric(svmpred), testSet$protection);

svmpred2 = predict(svmmodel, testSet2);
svmprediction2 = prediction(as.numeric(svmpred2), testSet$protection);

acc = unlist(performance(svmprediction,"acc")@y.values)[2]
F1 = unlist(performance(svmprediction,"f")@y.values)[2]
prec = unlist(performance(svmprediction,"prec")@y.values)[2]
recall = unlist(performance(svmprediction,"rec")@y.values)[2]
sensitiviy = unlist(performance(svmprediction,"sens")@y.values)[2];
specificity = unlist(performance(svmprediction,"spec")@y.values)[2];

cat("Accuracy(Test set): ", prec, "\n");
cat("F1-Score (Test set): ", F1, "\n");
cat("Precision(Test set): ", prec, "\n");
cat("Recall   (Test set): ", recall, "\n");
cat("Sensitivity(Test set): ", sensitiviy, "\n");
cat("Specificity(Test set): ", specificity, "\n");

acc = unlist(performance(svmprediction2,"acc")@y.values)[2]
F1 = unlist(performance(svmprediction2,"f")@y.values)[2]
prec = unlist(performance(svmprediction2,"prec")@y.values)[2]
recall = unlist(performance(svmprediction2,"rec")@y.values)[2]
sensitiviy = unlist(performance(svmprediction2,"sens")@y.values)[2];
specificity = unlist(performance(svmprediction2,"spec")@y.values)[2];

cat("Accuracy(Test set): ", prec, "\n");
cat("F1-Score (Test set): ", F1, "\n");
cat("Precision(Test set): ", prec, "\n");
cat("Recall   (Test set): ", recall, "\n");
cat("Sensitivity(Test set): ", sensitiviy, "\n");
cat("Specificity(Test set): ", specificity, "\n");