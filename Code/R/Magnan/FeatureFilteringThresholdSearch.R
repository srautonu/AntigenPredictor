library(e1071)
library(ROCR)
#library(pracma)
library(ggplot2)

source('featurefiltering.R');
source('svmCV.R')

timestamp();

set.seed(10);

featureCountList = seq(from = 6000, to = 500, by = -500); 

nFolds = 10

fScheme   = "_nGDip25";

# File names #
rankedFeaturesFile = paste("ff"         , fScheme, ".rds", sep = "");
featureFile        = paste("featurized" , fScheme, ".rds", sep = "");
outFile            = paste("out"        , fScheme, ".csv", sep = "");

cat(as.character(Sys.time()),">> Reading training set features from", featureFile, "...\n");
features = readRDS(featureFile);
cat(as.character(Sys.time()),">> Done\n");

cat(as.character(Sys.time()),">> Reading feature ranking from", rankedFeaturesFile, "...\n");
rankedFeatures = readRDS(rankedFeaturesFile);
cat(as.character(Sys.time()),">> Done\n");

#
# Balance the dataset (576+576) by undersampling the negative (larger) set
#
positiveSet = features[sample(1:576),]
negativeSetInd = sample(577:length(features[,1]))[1:576]
negativeSetInd = negativeSetInd[order(negativeSetInd)]
features = rbind(features[1:576,], features[negativeSetInd,])

# random shuffle of features
features <- features[sample(nrow(features)),]

bestPerf = NULL;
accData  = NULL;

cat(as.character(Sys.time()),">> Entering cross validation. Folds = ", nFolds, " ...\n");

# Reduce the feature vectors to the max size that we will be testing.
# This way the filtering cost in the loop below will be reduced.
features = featurefiltering(features, rankedFeatures, max(featureCountList));

# For regression study, we need to 'unfactor' the dependent var.
features$protection = as.numeric(features$protection) - 1;

rocCurvePoints = NULL;
prCurvePoints = NULL;

for (maxFeatureCount in featureCountList) 
{
  trainingSet = featurefiltering(features, rankedFeatures, maxFeatureCount);

  perf = svmCV(protection ~ ., trainingSet, svmCost = 1, cross = nFolds);
  
  AUCROC = perf$auc;
  df = data.frame(
        x = unlist(perf$rocCurve@x.values), 
        y = unlist(perf$rocCurve@y.values), 
        Features = as.character(maxFeatureCount)
        );
  rocCurvePoints = rbind(rocCurvePoints, df);
  
  
  # AUCPR calculation. y[1] is NaN. So, we exclude the first point
  
  x = unlist(perf$prCurve@x.values);
  y = unlist(perf$prCurve@y.values);
  df = data.frame(x = x[2:length(x)], y = y[2:length(y)], Features = as.character(maxFeatureCount));
  prCurvePoints = rbind(prCurvePoints, df);
  AUCPR  = trapz(df$x, df$y)
  
  cat(maxFeatureCount, ",", AUCROC,  ",",  AUCPR, "\n");
  accData = rbind(accData, c(maxFeatureCount, AUCROC, AUCPR));
  colnames(accData) = c("nF", "AUCROC", "AUCPR");

  write.csv(accData, outFile);
    
  cat("\n");
}


rocCurveFile = paste("ROCCurve", balancing, ".eps", sep = "");
prCurveFile  = paste("PRCurve",  balancing, ".eps", sep = "");

rocPlot = ggplot(rocCurvePoints,aes(x, y)) + 
  theme_bw(base_size = 36, base_family = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  theme(aspect.ratio = 0.7) +
  geom_line(aes(colour=Features),size = 3) +
  labs(x = "False Positive Rate", y = "True Positive Rate");
  
postscript(file = rocCurveFile, paper = "letter");
rocPlot;
dev.off();

prPlot = ggplot(prCurvePoints,aes(x, y)) + 
  theme_bw(base_size = 36, base_family = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  theme(aspect.ratio = 0.7) +
  geom_line(aes(colour=Features),size = 3) +
  labs(x = "Recall", y = "Precision");

postscript(file = prCurveFile, paper = "letter");
prPlot;
dev.off();

