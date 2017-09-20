
svmCV <-
  function(formula, data, svmCost, cross, svmScale = TRUE) {
    N = length(data[, 1])
    folds = seq(from=1,to=N, by=round(N/cross))
    folds[cross+1] = N+1
    predVector = c()
    for (i in 1:cross) {
      trainFolds = data
      testFold = data[(folds[i]:(folds[i+1]-1)),]
      trainFolds = data[-(folds[i]:(folds[i+1]-1)),]
      svmmodel = svm(formula, trainFolds, kernel = "linear", cost = svmCost, scale = svmScale)
      
      svmpred = predict(svmmodel, testFold)
      predVector = c(predVector, as.numeric(svmpred))
      i = i + 1
    }
    
    dependentVar = all.vars(formula)[1];
    
    # perform classification based perf. measures
    if (is.factor(data[,dependentVar])) {
      svmprediction = prediction(as.numeric(predVector), as.numeric(data[,dependentVar]))
      
      acc = unlist(ROCR::performance(svmprediction,"acc")@y.values)[2]
      sensitivity = unlist(ROCR::performance(svmprediction,"sens")@y.values)[2];
      specificity = unlist(ROCR::performance(svmprediction,"spec")@y.values)[2];
      mcc = unlist(ROCR::performance(svmprediction,"mat")@y.values)[2];
    
      return(list(
        "acc"  = acc,
        "sens" = sensitivity,
        "spec" = specificity,
        "mcc"  = mcc
        ))
    }
    else {
      # perform regression based perf. measurements
      
      # Find optimal threshold based on accuracy
      # Also find the AUCROC
      svmprediction = prediction(predVector, data[, dependentVar]);
      auc  = ROCR::performance(svmprediction,"auc")@y.values[[1]];
      rocCurve = ROCR::performance(svmprediction,"tpr", "fpr");
      prCurve  = ROCR::performance(svmprediction,"prec", "rec");
      
      accSeries = ROCR::performance(svmprediction,"acc");
      threshold = unlist(accSeries@x.values)[[which.max(unlist(accSeries@y.values))]];
      
      svmprediction = prediction(as.numeric(predVector >= threshold), data[, dependentVar]);
      
      acc = unlist(ROCR::performance(svmprediction,"acc")@y.values)[2]
      sensitivity = unlist(ROCR::performance(svmprediction,"sens")@y.values)[2];
      specificity = unlist(ROCR::performance(svmprediction,"spec")@y.values)[2];
      mcc = unlist(ROCR::performance(svmprediction,"mat")@y.values)[2];
      
      return(list(
        "threshold" = threshold,
        "rocCurve" = rocCurve,
        "prCurve"  = prCurve,
        "auc" = auc,
        "acc" = acc,
        "sens" = sensitivity,
        "spec" = specificity,
        "mcc" = mcc
      ))
    }
  }