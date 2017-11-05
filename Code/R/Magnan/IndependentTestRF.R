library(e1071)
library(ROCR)

source('featurefiltering.R');

timestamp();

set.seed(20);

fScheme = "_heu_comb";
maxFeatureCount = 340;
DoBalancing = TRUE;

itPerfFileName       = "IT_Perf.csv";
itEnrichmentFileName = "IT_Enrichment.csv";

RDSFolder          = "RDSFiles/"

# File names #
rankedFeaturesFile = paste(RDSFolder, "ff_SvmRFE2"            , fScheme, ".rds", sep = "");
featureFile        = paste(RDSFolder, "featurized"    , fScheme, ".rds", sep = "");
testFeatureFile    = paste(RDSFolder, "testFeaturized", fScheme, ".rds", sep = "");
rfFile             = paste("rf_", maxFeatureCount     , fScheme, ".rds", sep = "");

cat(as.character(Sys.time()),">> Loading feature ranking ...\n");
rankedFeatures = readRDS(rankedFeaturesFile);
cat(as.character(Sys.time()),">> Done ( from cached file:", rankedFeaturesFile, ")\n");

if (!file.exists(rfFile)) {
  cat(as.character(Sys.time()),">> Building RF model for maxFeatureCount = ", maxFeatureCount, " ...\n");

  features = readRDS(featureFile);
  features$ID = NULL;
  features$Type = NULL;
  features$protection = as.numeric(features$protection) - 1;
  
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
  
  trainingSet = featurefiltering(features, rankedFeatures, maxFeatureCount);
  cat(as.character(Sys.time()),">> Training set Features filtered.\n");
  
  rfModel = randomForest(protection ~ ., trainingSet);
  saveRDS(rfModel, rfFile);
  cat(as.character(Sys.time()),">> Model built.\n");
} else {
  cat(as.character(Sys.time()),">> Loading RF model from ", rfFile, " ... \n");
  rfModel = readRDS(rfFile);
  cat(as.character(Sys.time()),">> Done.\n");
}

cat(as.character(Sys.time()),">> Loading Test set features ...\n");
features = readRDS(testFeatureFile);
features$ID = NULL;
features$Type = NULL;
features$protection = as.numeric(features$protection) - 1;
cat(as.character(Sys.time()),">> Done ( from cached file:", testFeatureFile, ")\n");

cat(as.character(Sys.time()),">> Filtering Test set features ...\n");
testSet = featurefiltering(features, rankedFeatures, maxFeatureCount);
cat(as.character(Sys.time()),">> Test set Features filtered.\n");

cat(as.character(Sys.time()),">> Predicting ...\n");
rfPred = predict(rfModel, testSet);
rfPrediction = prediction(rfPred, testSet$protection);
cat(as.character(Sys.time()),">> Done.\n");

cat(as.character(Sys.time()),">> Recording performance in ", itPerfFileName, "  ...\n");
itPerf = data.frame(matrix(ncol = 6, nrow = 0))
for (threshold in seq(from=0.1, to=0.9, by=0.01)) {

  rfPrediction = prediction(as.numeric(rfPred >= threshold), testSet$protection);
  acc  = unlist(ROCR::performance(rfPrediction,"acc")@y.values)[2];
  sens = unlist(ROCR::performance(rfPrediction,"sens")@y.values)[2];
  spec = unlist(ROCR::performance(rfPrediction,"spec")@y.values)[2];
  mcc  = unlist(ROCR::performance(rfPrediction,"mat")@y.values)[2];
  prec = unlist(ROCR::performance(rfPrediction,"prec")@y.values)[2];

  itPerf = rbind(itPerf, c(threshold, acc, sens, spec, mcc, prec));
}

colnames(itPerf) = c("Threshold", "Accuracy", "Sensitivity", "Specificity", "MCC", "Precision");
write.csv(itPerf, itPerfFileName);
cat(as.character(Sys.time()),">> Done.\n");

cat(as.character(Sys.time()),">> Calculating enrichment ...\n");
sortOrder = order(-rfPred);

cat(as.character(Sys.time()),">> Recording enrichment in ", itEnrichmentFileName, "  ...\n");
itEnrichment = data.frame(matrix(ncol = 2, nrow = 0))
for (rank in seq(from=1, to=100, by=1)) {

  topRanked = round(rank * length(testSet[,1]) / 100, 0);
  num = sum(testSet[sortOrder[1:topRanked],"protection"])/topRanked;
  den = sum(testSet$protection) / length(testSet[,1]);
  enrichment = round(num / den, 1);

  itEnrichment = rbind(itEnrichment, c(rank, enrichment));
}
colnames(itEnrichment) = c("TopRank%", "Enrichment");
write.csv(itEnrichment, itEnrichmentFileName);
cat(as.character(Sys.time()),">> Done.\n");


