featurefiltering <-
  function(trainingSet, testSet, rfmodel, maxFeatureCount = Inf) {
    if (ncol(trainingSet) - 1 > maxFeatureCount) {
      randomforestfeatures = rfmodel$importance
      # sort descending based on IncNodePurity
      randomforestfeatures = randomforestfeatures[order(-randomforestfeatures[,2]),]

      featureFilter = c()
      for(i in 1:maxFeatureCount) {
        featureFilter = c(featureFilter,rownames(randomforestfeatures)[i])
      }
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