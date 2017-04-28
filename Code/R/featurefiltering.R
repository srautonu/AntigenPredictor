featurefiltering <-
  function(trainingSet, validationSet, testSet, rfformula, maxFeatureCount = Inf) {
    if (ncol(trainingSet) - 1 > maxFeatureCount) {
      rfmodel = randomForest(rfformula, trainingSet, importance=TRUE)
  
      randomforestfeatures = rfmodel$importance
      # sort descending based on IncNodePurity
      randomforestfeatures = randomforestfeatures[order(-randomforestfeatures[,2]),]
      #write.csv(randomforestfeatures, "rfmodel.csv");
      
      featureFilter = c()
      for(i in 1:maxFeatureCount) {
        featureFilter = c(featureFilter,rownames(randomforestfeatures)[i])
      }
      featureFilter[length(featureFilter) + 1] = "protection"
      columns = colnames(trainingSet)
      for(i in 1:length(columns)){
        if(!columns[i] %in% featureFilter){
          trainingSet[columns[i]] = NULL
          validationSet[columns[i]] = NULL
          testSet[columns[i]] = NULL
        }
      }
    }    
    return(list(
      "trainingSet" = trainingSet,
      "validationSet" = validationSet,
      "testSet" = testSet
      ))
  }