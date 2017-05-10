source('./base.R');

bestAcc = 0;
bestSVM = NULL;
bestParams = NULL;
accData = NULL;

for (maxFeatureCount in seq(from=2000, to=10, by=-10)) 
{
  trainingSet = features[1:nTrainingSet,];
  testSet = features[(nTrainingSet + 1) : (nTrainingSet + nTestSet),];
  
  filteringRes = featurefiltering(trainingSet, testSet, rankedFeatures, maxFeatureCount);
  trainingSet = filteringRes$trainingSet;
  testSet = filteringRes$testSet;

  svmCostList = c(0.01, 0.03, 0.10, 0.30, 1, 3, 10, 30, 100);
  for (svmC in svmCostList) 
  {
    svmmodel = svm(protection ~ ., trainingSet, kernel = "linear", cost = svmC, cross = nTrainingSet, scale = TRUE);
    perf = svmmodel$tot.accuracy;
    
    cat(maxFeatureCount, ",", svmC, ",", perf);

    accData = rbind(accData, c(maxFeatureCount, svmC, perf));
    write.csv(accData, outFile);

    if (bestAcc < perf) {
      bestAcc = perf;
      bestSVM = svmmodel;
      bestParams = list(
        "maxFeatureCount" = maxFeatureCount,
        "svmC" = svmC
      )
      cat(",<-- BEST");
    }
    
    cat("\n");
  }
}

cat("Best Result:\n");
cat("<nF, C, Acc> = ", bestParams$maxFeatureCount, bestParams$svmC, bestAcc, "\n");
saveRDS(bestSVM, svmFile);