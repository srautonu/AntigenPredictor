featurefiltering <-
  function(trainingSet, testSet, rankedFeatures, maxFeatureCount = Inf) {
    if (ncol(trainingSet) - 1 > maxFeatureCount) {
      featureFilter = rankedFeatures[1:maxFeatureCount];
      featureFilter[length(featureFilter) + 1] = "protection"
      
      columns = colnames(trainingSet)
      for(i in 1:length(columns)){
        if(!columns[i] %in% featureFilter){
          trainingSet[columns[i]] = NULL
          testSet[columns[i]] = NULL
        }
      }
    }    
    return(list(
      "trainingSet" = trainingSet,
      "testSet" = testSet
      ))
  }