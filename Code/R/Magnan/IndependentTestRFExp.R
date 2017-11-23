library(e1071)
library(ROCR)
library(randomForest)

source('featurefiltering.R');

timestamp();

fScheme         = "_heu_comb";
maxFeatureCount = 360;
seed            = 10;
DoBalancing     = TRUE;

set.seed(seed);

RDSFolder          = "RDSFiles/"

# File names #
rankedFeaturesFile = paste(RDSFolder, "ff_SvmRFE2"            , fScheme, ".rds", sep = "");
featureFile        = paste(RDSFolder, "featurized"    , fScheme, ".rds", sep = "");
testFeatureFile    = paste(RDSFolder, "testFeaturized", fScheme, ".rds", sep = "");
rfFile             = paste("rf_", maxFeatureCount, "_", seed, fScheme, ".rds", sep = "");
outFile            = paste("out_", maxFeatureCount, "_", seed, fScheme, ".csv", sep = "");

cat(as.character(Sys.time()),">> Loading feature ranking ...\n");
rankedFeatures = readRDS(rankedFeaturesFile);
cat(as.character(Sys.time()),">> Done ( from cached file:", rankedFeaturesFile, ")\n");

cat(as.character(Sys.time()),">> Loading Test set features ...\n");
features = readRDS(testFeatureFile);
features$ID = NULL;
features$Type = NULL;
features$protection = as.numeric(features$protection) - 1;
cat(as.character(Sys.time()),">> Done ( from cached file:", testFeatureFile, ")\n");

cat(as.character(Sys.time()),">> Filtering Test set features ...\n");
testSet = featurefiltering(features, rankedFeatures, maxFeatureCount);
cat(as.character(Sys.time()),">> Test set Features filtered.\n");

cat(as.character(Sys.time()),">> Loading Training set features ...\n");
features = readRDS(featureFile);
features$ID = NULL;
features$Type = NULL;
features$protection = as.numeric(features$protection) - 1;

trainingSet = featurefiltering(features, rankedFeatures, maxFeatureCount);
cat(as.character(Sys.time()),">> Training set Features filtered.\n");

if (DoBalancing) {
  #
  # Balance the dataset (576+576) by undersampling the negative (larger) set
  #
  positiveSet = features[sample(1:576),]
  negativeSetInd = sample(577:length(features[,1]))[1:576]
  negativeSetInd = negativeSetInd[order(negativeSetInd)]
  features = rbind(features[1:576,], features[negativeSetInd,])
}

# random shuffle of features
features <- features[sample(nrow(features)),]
cat(as.character(Sys.time()),">> Training set features loaded from", featureFile, "\n");

bestResult = NULL;
accData = NULL;

while (TRUE) {
  rfModel = randomForest(protection ~ ., trainingSet);
  rfPred = predict(rfModel, testSet);
  rfPrediction = prediction(as.numeric(rfPred >= 0.5), testSet$protection);
  acc  = unlist(ROCR::performance(rfPrediction,"acc")@y.values)[2];
  sens = unlist(ROCR::performance(rfPrediction,"sens")@y.values)[2];
  spec = unlist(ROCR::performance(rfPrediction,"spec")@y.values)[2];
  mcc  = unlist(ROCR::performance(rfPrediction,"mat")@y.values)[2];
  prec = unlist(ROCR::performance(rfPrediction,"prec")@y.values)[2];
  f1   = unlist(ROCR::performance(rfPrediction,"f")@y.values)[2];
  
  
  enrichment = NULL;
  sortOrder = order(-rfPred);
  for (rank in c(2,5,10,25)) {
    
    topRanked = round(rank * length(testSet[,1]) / 100, 0);
    num = sum(testSet[sortOrder[1:topRanked],"protection"])/topRanked;
    den = sum(testSet$protection) / length(testSet[,1]);
    enrichment = c(enrichment, round(num / den, 1));
  }
  
  cat(
    maxFeatureCount, 
    ",", round(acc, 2),
    ",", round(sens, 2),
    ",", round(spec, 2),
    ",", round(prec, 2),
    ",", round(f1, 2),
    ",", round(mcc, 2),
    ",", enrichment[1],
    ",", enrichment[2],
    ",", enrichment[3],
    ",", enrichment[4],
    "\n"
  );
  
  if (is.null(bestResult)
      || (bestResult$enrich1 < enrichment[1])
      || (bestResult$enrich1 == enrichment[1] && bestResult$enrich2 < enrichment[2])
      || (bestResult$enrich1 == enrichment[1] && bestResult$enrich2 == enrichment[2] && bestResult$enrich3 < enrichment[3])
      || (bestResult$enrich1 == enrichment[1] && bestResult$enrich2 == enrichment[2] && bestResult$enrich3 == enrichment[3] && bestResult$enrich4 < enrichment[4])
      ) {
    bestResult$model = rfModel;
    bestResult$enrich1 = enrichment[1];
    bestResult$enrich2 = enrichment[2];
    bestResult$enrich3 = enrichment[3];
    bestResult$enrich4 = enrichment[4];
  }
  
  accData = rbind(accData, c(maxFeatureCount, acc, sens, spec, prec, f1, mcc, enrichment[1], enrichment[2], enrichment[3], enrichment[4]));
  colnames(accData) = c("nF", "Accuracy", "Sensitivity", "Specificity", "Precision", "F1" , "MCC", "%2", "%5", "%10", "%25");
  write.csv(accData, outFile);
  saveRDS(bestResult$model, rfFile);
}  
  
