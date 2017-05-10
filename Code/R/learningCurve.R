source('./base.R');

nFeatures = 110;
svmC = 0.01;
trainingSet = features[1:nTrainingSet,];
testSet = features[(nTrainingSet + 1) : (nTrainingSet + nTestSet),];

filteringRes = featurefiltering(trainingSet, testSet, rankedFeatures, nFeatures);
testSet = filteringRes$testSet;

lcData = c("learningSet","ISE","OSE");
cat("learningSet,ISE,OSE\n");

for (learningSet in seq(from=2, to=nTrainingSet, by=1)) {
  trainingSet = filteringRes$trainingSet[1:learningSet,];

  # if (sum(trainingSet$protection) == 0) {
  #   next;
  # }
  
  svmmodel = svm(protection ~ ., trainingSet, kernel = "linear", cost = 0.01, cross = learningSet, scale = TRUE);

  # out of sample accuracy
  svmpred = predict(svmmodel, testSet);
  svmprediction = prediction(as.numeric(svmpred), testSet$protection);
  acc = unlist(performance(svmprediction,"acc")@y.values)[2];
  
  lcData = rbind(lcData, c(learningSet,  1 - svmmodel$tot.accuracy/100, 1 - acc));
  write.csv(lcData, lcFile);
  cat(learningSet, "," , 1 - svmmodel$tot.accuracy/100, ",", 1 - acc, "\n");
}

cat(as.character(Sys.time()), ">> Done.\n");
