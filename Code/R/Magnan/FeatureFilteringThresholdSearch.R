library(e1071)
library(ROCR)
library(pracma)
library(ggplot2)

source('featurefiltering.R');
source('svmCV.R')

timestamp();

set.seed(10);

nFolds = 10

fScheme   = "_pssm_nGrams";

#featureCountList = NULL;
featureCountList = seq(from = 10000, to = 5500, by = -500);

# File names #
rdsFilesFolder     = "RDSFiles/";
rankedFeaturesFile = paste(rdsFilesFolder, "ff"         , fScheme, ".rds", sep = "");
featureFile        = paste(rdsFilesFolder, "featurized" , fScheme, ".rds", sep = "");
outFile            = paste("./"          , "out"        , fScheme, ".csv", sep = "");

cat(as.character(Sys.time()),">> Reading training set features from", featureFile, "...\n");
features = readRDS(featureFile);
cat(as.character(Sys.time()),">> Done\n");

cat(as.character(Sys.time()),">> Reading feature ranking from", rankedFeaturesFile, "...\n");
rankedFeatures = readRDS(rankedFeaturesFile);
cat(as.character(Sys.time()),">> Done\n");

rfmodelFile        = paste(rdsFilesFolder, "rfmodel"    , fScheme, ".rds", sep = "");
cat(as.character(Sys.time()),">> Reading random forest model from", rfmodelFile, "...\n");
rfmodel = readRDS(rfmodelFile);
cat(as.character(Sys.time()),">> Done\n");

if (is.null(featureCountList)) {
  imp = rfmodel$importance[order(-rfmodel$importance[,3]),];
  lastImpFeatureInd = which(imp[,3] <= 0)[1] - 1;
  lastImpFeatureInd = lastImpFeatureInd - (lastImpFeatureInd %% 500)
  featureCountList = seq(from = lastImpFeatureInd, to = 500, by = -500);
}
cat(as.character(Sys.time()),
    ">> Searching from", featureCountList[1], "to",  
    featureCountList[length(featureCountList)], "by -500\n"
    );

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

  perf = svmCV(protection ~ ., trainingSet, cross = nFolds, kernel = "linear", svmCost = 1);
  
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


rocCurveFile = "ROCCurve.eps";
prCurveFile  = "PRCurve.eps";

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

