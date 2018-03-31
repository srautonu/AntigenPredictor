library(randomForest)
source('./filteredFeaturization.R');

# Customize parameters here. Predictions will be written to
# predicted.csv file.
#
# sequenceFile: 
#   Where the query sequences are given. The sequences must be in
#   a column called "Sequence". Also, there should be an "ID" column
# threshold:
#   Class discriminating threshold.
sequenceFile = "testSet.csv";
threshold    = 0.5;

predictionFile = "predicted.csv";
rfModelFile    = "Balanced_RFModel.rds";

cat(as.character(Sys.time()),">> Reading model from", rfModelFile, "...\n");
rfmodel = readRDS(rfModelFile);
cat(as.character(Sys.time()),">> Done.\n");

cat(as.character(Sys.time()),">> Reading sequence file from", sequenceFile, "...\n");
data = read.csv(sequenceFile);
cat(as.character(Sys.time()),">> Done.\n");

cat(as.character(Sys.time()),">> Generating features ...\n");
querySet = filteredFeaturization(data$Sequence, rownames(rfmodel$importance));
cat(as.character(Sys.time()),">> Done.\n");
rfpred = predict(rfmodel, querySet);

predictions = data.frame(
                Id = data$ID, 
                Score = rfpred, 
                Protection = ifelse (rfpred >= threshold, "Yes", "No")
                );

write.csv(predictions, predictionFile, row.names = FALSE);
cat(as.character(Sys.time()),">> Done. Predictions saved to", predictionFile, "\n");
