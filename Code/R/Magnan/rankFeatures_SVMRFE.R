library(e1071)

source("svmRFE.R")
source("featurefiltering.R")

timestamp();

set.seed(10);

fScheme = "_heu_comb";

RDSFolder = "RDSFiles/"

fileNameSuffix = paste(fScheme, ".rds", sep = "");

InitialRankedFeaturesFile = paste(RDSFolder, "ff_SvmRFE" , fileNameSuffix, sep = "");
FinalRankedFeaturesFile   = paste(RDSFolder, "ff_SvmRFE2", fileNameSuffix, sep = "");
featureFile               = paste(RDSFolder, "featurized", fileNameSuffix, sep = "");

if (!file.exists(FinalRankedFeaturesFile)) {
  cat(as.character(Sys.time()),">> Loading feature file ...\n");
  features = readRDS(featureFile);
  cat(as.character(Sys.time()),">> Done ( from cached file:", featureFile, ")\n");
 
  features$ID = NULL;
  features$Type = NULL;
  cat(as.character(Sys.time()),">> Total features: ", length(features[1,]) - 1, "\n");

  cat(as.character(Sys.time()),">> Loading initial feature ranking ...\n");
  rankedFeatures = readRDS(InitialRankedFeaturesFile);
  cat(as.character(Sys.time()),">> Done ( from cached file:", InitialRankedFeaturesFile, ")\n");
  
  # Reduce the feature vectors to the max size that we will be testing.
  # This way the filtering cost in the loop below will be reduced.
  features = featurefiltering(features, rankedFeatures, 600);
  
  #
  # Balance the dataset (576+576) by undersampling the negative (larger) set
  #
  positiveSet = features[sample(1:576),]
  negativeSetInd = sample(577:length(features[,1]))[1:576]
  negativeSetInd = negativeSetInd[order(negativeSetInd)]
  features = rbind(features[1:576,], features[negativeSetInd,])
  
  # random shuffle of features
  features <- features[sample(nrow(features)),]
  
  cat(as.character(Sys.time()),">> Computing feature ranking ...\n");
  
  labelCol = which(colnames(features) == "protection");
  
  rankedFeatures = svmRFE(features[,-labelCol], features$protection, 1);
  saveRDS(rankedFeatures, FinalRankedFeaturesFile);
  cat(as.character(Sys.time()),">> Done\n");
} else {
  cat(as.character(Sys.time()),">> Computing feature ranking ...\n");
  rankedFeatures = readRDS(FinalRankedFeaturesFile);
  cat(as.character(Sys.time()),">> Done ( from cached file:", FinalRankedFeaturesFile, ")\n");
}