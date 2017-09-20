#
# Reduce the tier/order of a feature space
#
inputFile  = "featurized_1324_PSF10.rds"
outputFile = "featurized_1324_PSF.rds"
prefOrder  = 5;

data = data.frame(Feature = character(), Order = numeric());

features = readRDS(inputFile);
featureNames = colnames(features);

for (featureName in featureNames) {
  splits = strsplit(toString(featureName), "_")[[1]];
  data = rbind(data, data.frame(Feature = featureName, Order = as.numeric(splits[2])));  
}

data = data[order(data$Order),]
index = which(data$Order > prefOrder)[1] - 1;
data = data[1:index,]
featureSubset = c("ID", "Type", as.character(data$Feature), "protection");

spaceReducedFeatures = features[,featureSubset]
saveRDS(spaceReducedFeatures, outputFile)

