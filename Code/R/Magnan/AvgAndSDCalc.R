
dataFilePrefix = "out_heu_comb_";
rngSeeds = c(10, 20, 30, 40, 50);

runs = vector(mode = "list", length = 5);
for(i in 1:length(runs)) {
  dataFile = paste0(dataFilePrefix, rngSeeds[i], ".csv");
  runs[[i]] = read.csv(dataFile);
}

nRows = length(runs[[1]][,1]);
nCols = length(runs[[1]][1,]);

avg = matrix(nrow = nRows, ncol = nCols);
sd  = matrix(nrow = nRows, ncol = nCols);

for (i in 1:nRows) {
  for (j in 1:nCols) {
    x = c();
    for (k in 1:length(runs)) {
      x = c(x, runs[[k]][i,j]);
    }
    avg[i,j] = mean(x);
    sd[i,j] = sd(x)
  }
}

colnames(avg) = colnames(runs[[1]]);
colnames(sd)  = colnames(runs[[1]]);

write.csv(avg, paste0(dataFilePrefix, "avg.csv"));
write.csv(sd, paste0(dataFilePrefix, "sd.csv"));
