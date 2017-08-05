source('./base.R');

bestMSE = Inf;
bestSVM = NULL;
bestParams = NULL;
accData = NULL;

maxFeatureCount = 1470;
svmC = 0.03

cat(as.character(Sys.time()),">> Entering to create SVM regression model ...\n");

trainingSet = features[1:nTrainingSet,];
testSet = features[(nTrainingSet + 1) : (nTrainingSet + nTestSet),];

filteringRes = featurefiltering(trainingSet, testSet, rankedFeatures, maxFeatureCount);
trainingSet = filteringRes$trainingSet;
testSet = filteringRes$testSet;

# The featurization makes the protection column as factor.
# so for regression study, we need to convert it to numeric. However
# as.numeric creates values 1 and 2 for the levels 0 and 1 respectively.
# As such, we need to deduct 1.
trainingSet$protection = as.numeric(trainingSet$protection) - 1;
testSet$protection = as.numeric(testSet$protection) - 1;

svmmodel = svm(protection ~ ., trainingSet, kernel = "linear", cost = svmC);
saveRDS(svmmodel, svmFile)

