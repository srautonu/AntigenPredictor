source('./base.R');

cat(as.character(Sys.time()),">> Reading SVM model from", svmFile, "...\n");
svmmodel = readRDS(svmFile);
cat(as.character(Sys.time()),">> Done\n");

nFeatures = length(svmmodel$SV[1,]);

trainingSet = features[1:nTrainingSet,];
testSet = features[(nTrainingSet + 1) : (nTrainingSet + nTestSet),];

filteringRes = featurefiltering(trainingSet, testSet, rankedFeatures, nFeatures);
trainingSet = filteringRes$trainingSet;
testSet = filteringRes$testSet;

# The featurization makes the protection column as factor.
# so for regression study, we need to convert it to numeric. However
# as.numeric creates values 1 and 2 for the levels 0 and 1 respectively.
# As such, we need to deduct 1.
trainingSet$protection = as.numeric(trainingSet$protection) - 1;
testSet$protection = as.numeric(testSet$protection) - 1;

svmpred = predict(svmmodel, testSet);

# Find optimal threshold based on accuracy
# Also find the AUCROC
svmprediction = prediction(svmpred, testSet$protection);
auc  = ROCR::performance(svmprediction,"auc")@y.values[[1]];

accSeries = ROCR::performance(prediction(svmpred, testSet$protection),"acc");
threshold = unlist(accSeries@x.values)[[which.max(unlist(accSeries@y.values))]];

svmprediction = prediction(as.numeric(svmpred >= threshold), testSet$protection);

acc  = unlist(ROCR::performance(svmprediction,"acc")@y.values)[2];
sens = unlist(ROCR::performance(svmprediction,"sens")@y.values)[2];
spec = unlist(ROCR::performance(svmprediction,"spec")@y.values)[2];
mcc = unlist(ROCR::performance(svmprediction,"mat")@y.values)[2];

cat("Optimal threshold    : ", threshold, "\n");
cat("Accuracy(Test set)   : ", acc, "\n");
cat("AUC(Test set)        : ", auc, "\n");
cat("Sensitivity(Test set): ", sens, "\n");
cat("Specificity(Test set): ", spec, "\n");
cat("MCC(Test set)        : ", mcc, "\n");