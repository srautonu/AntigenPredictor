source('./filteredFeaturization.R');

svmFile = "svm_830_cpnmer_heuristic.rds";
sequenceFile = "VirusData_FromArif.csv";
featureFile = "featurized_VirusData_FromArif.rds"

cat(as.character(Sys.time()),">> Reading SVM model from", svmFile, "...\n");
svmmodel = readRDS(svmFile);
cat(as.character(Sys.time()),">> Done.\n");

cat(as.character(Sys.time()),">> Reading sequence file from", sequenceFile, "...\n");
data = read.csv(sequenceFile);
cat(as.character(Sys.time()),">> Done.\n");

if (file.exists(featureFile)) {
  cat(as.character(Sys.time()),">> Reading features from", featureFile, "...\n");
  querySet = readRDS(featureFile);
} else {
  cat(as.character(Sys.time()),">> Generating features ...\n");
  querySet = filteredFeaturization(data$Sequence, colnames(svmmodel$SV));
}
cat(as.character(Sys.time()),">> Done.\n");

cat(as.character(Sys.time()),">> Predicting antigenicity ...\n");
svmpred = predict(svmmodel, querySet);
cat(as.character(Sys.time()),">> Done.\n");
