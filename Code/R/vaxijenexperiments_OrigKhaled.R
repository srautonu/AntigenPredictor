#amins = c('A','C','D','E','F','G','H','I','K','L','M','N','P','Q','R','S','T','V','W','Y')
library(e1071)
library(ROCR)
nfold = 10
threshold = 0.4
randomforestfeatures = read.csv('rfmodelsvaxijen.csv')
features = c()
for(i in 1:length(randomforestfeatures[,2])){
  if(randomforestfeatures[i,3] > 0.0058){
    features = c(features,toString(randomforestfeatures[i,1]))
  }
}
features[length(features) + 1] = "score"
cat('Reading features complete: total',  length(features), ' features ...\n')
vaxijendata = read.csv('featurizedvaxijendata.csv')
columns = names(vaxijendata)
for(i in 1:length(columns)){
  if(!columns[i] %in% features){
    vaxijendata[columns[i]] = NULL
  }
}
data = vaxijendata
predValue = c()
tV = c()
len = length(data[,1]) / nfold
j = 1
total = 1:length(data[,1])
for (i in 1:nfold){
  test = j:(i*len)
  train = unlist(lapply(total, function(x)if(!x %in% test){x}))
  j = i * len + 1
  training = data[train,]
  testing = data[test,]
  bformula = featureformula(features)
  svmmodel = svm(
    as.formula(bformula), training
  )
  svmpred = predict(svmmodel, testing)
  svmpred = as.vector(svmpred)
  predValue = c(predValue, svmpred)
  t = testing$score;
  b = unlist(lapply(t, function(x)if(x>threshold){1}else{0}))
  tV = c(tV, b)
  cat("Running SVM iteration no. = ",i,"  ...\n")
  cat("training",train[1],"\n")
  cat("test",test[1],"\n")
}
  svmpred = predValue
  trueV= tV
  svmprediction = prediction(svmpred, trueV)
  svmROC = performance(svmprediction,"tpr","fpr")
  svmACC = performance(svmprediction, "acc")
  svmAUC = performance(svmprediction, "auc")
  svmPR = performance(svmprediction,"prec","rec")
  svmSS = performance(svmprediction,"sens","spec")
  svmMAT = performance(svmprediction,"mat")
  perf  <- performance(svmprediction, "prec", "rec")
  xy    <-
    data.frame(recall = perf@x.values[[1]], precision = perf@y.values[[1]])
  xy <- subset(xy,!is.nan(xy$precision))
  xy <- rbind(c(0, 0), xy)
  svmAUPR  <- trapz(xy$recall, xy$precision)
  par(mar = c(4, 5, 1.98, 1))
  plot(
    svmROC, main = "ROC-Curve", cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis =
      1.3,yaxis.cex.axis = 1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1
  )
  
