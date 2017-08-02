featurefiltering <-
  function(trainingSet, testSet, rankedFeatures, maxFeatureCount = Inf) {
    fcVector = c()
    if (ncol(trainingSet) - 1 > maxFeatureCount) {
      columns = colnames(trainingSet)
      featureFilter = rankedFeatures[1:maxFeatureCount];
      fcVector = match(featureFilter, columns)
      fcVector = fcVector[!is.na(fcVector)]
      fcVector = sort(fcVector)
      featureFilter[length(featureFilter) + 1] = "protection"
      
      for(i in 1:length(columns)){
        if(!columns[i] %in% featureFilter){
          trainingSet[columns[i]] = NULL
          testSet[columns[i]] = NULL
        }
      }
    }  
    columns = colnames(testSet)
    traincolumns = colnames(trainingSet)

    for(i in 1:length(traincolumns)){
        if(!traincolumns[i] %in% columns){
          trainingSet[traincolumns[i]] = NULL
        }
    }  
    
    return(list(
      "trainingSet" = trainingSet,
      "testSet" = testSet,
      "fcVector" = fcVector
      ))
  }