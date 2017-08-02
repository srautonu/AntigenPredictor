
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

    # Find optimal threshold based on accuracy
    # Also find the AUCROC
    svmprediction = prediction(predVector, data$protection);
    auc  = ROCR::performance(svmprediction,"auc")@y.values[[1]];
    
    accSeries = ROCR::performance(prediction(svmpred, data$protection),"acc");
    threshold = unlist(accSeries@x.values)[[which.max(unlist(accSeries@y.values))]];
    
    svmprediction = prediction(as.numeric(predVector >= threshold), data$protection);
    
    acc = unlist(ROCR::performance(svmprediction,"acc")@y.values)[2]
    sensitivity = unlist(ROCR::performance(svmprediction,"sens")@y.values)[2];
    specificity = unlist(ROCR::performance(svmprediction,"spec")@y.values)[2];
    mccv = unlist(ROCR::performance(svmprediction,"mat")@y.values)[2];
    
    return(list(
      "auc" = auc,
      "acc" = acc,
      "sens" = sensitivity,
      "spec" = specificity,
      "mcc" = mccv
      ))
  }