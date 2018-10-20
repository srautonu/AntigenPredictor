require(mRMRe)

mRMR <-
  function(features, labelIndex, rankedFeatureCount) {
  
    features[[labelIndex]] <- as.numeric(features[[labelIndex]])
    df <- mRMR.data(data = data.frame(features))
    results <- mRMR.classic("mRMRe.Filter", data = df, target_indices = labelIndex,
                            feature_count = rankedFeatureCount)
    return(colnames(features)[unlist(solutions(results))])
  }