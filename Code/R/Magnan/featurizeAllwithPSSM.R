library(e1071)
library(ROCR)

#source('./featurizationWithPSSM.R');

timestamp();

pssmMapFile       = "mapPSSM_Sigmoid.rds"

antigensFile      = "antigens.csv";
nonAntigensFile   = "nonAntigens.csv";
featureFilePrefix = "featurized";

#
# Uncomment for test set featurization
#
# antigensFile      = "Bartonella_Antigen.csv";
# nonAntigensFile   = "Bartonella_NonAntigen.csv";
# featureFilePrefix = "testFeaturized";

fScheme = "_pssm_nGrams";

featureFile = paste(featureFilePrefix, fScheme, ".rds", sep = "");

amins = c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y");

cat(as.character(Sys.time()),">> Featurizing ...\n");
if (!file.exists(featureFile)) {
  cat(as.character(Sys.time()),">> Reading antigens file (", antigensFile, ") ...\n");
  antigens = read.csv(antigensFile);
  antigens$protection = 1;
  cat(as.character(Sys.time()),">> Done\n");
  
  cat(as.character(Sys.time()),">> Reading non-antigens file (", nonAntigensFile, ") ...\n");
  nonAntigens = read.csv(nonAntigensFile);
  nonAntigens$protection = 0;
  cat(as.character(Sys.time()),">> Done\n");
  
  data = rbind(antigens, nonAntigens);
  nData = length(data[,1]);
  
  cat(as.character(Sys.time()),">> Reading PSSM map file (", pssmMapFile, ") ...\n");
  pssmMap = readRDS(pssmMapFile);
  cat(as.character(Sys.time()),">> Done\n");
  
  features = featurization(
                data$ID, 
                data$Sequence, 
                data$protection, 
                pssmMap, 
                amins, 
                nGramOrder = 3,
                nGDipOrder = 0
                );
  features$ID = data$ID
  features$Type = data$Type;
  saveRDS(features, featureFile);
  cat(as.character(Sys.time()),">> Featurizing Done.\n");
} else {
  features = readRDS(featureFile);
  cat(as.character(Sys.time()),">> Done ( from cached file:", featureFile, ")\n");
}
cat(as.character(Sys.time()),">> Total features: ", length(features[1,]), "\n");
