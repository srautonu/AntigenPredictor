#########################################################
# For writing to R window comment out the following line
# in each graph
# theme_bw(base_size = 36, base_family = "") +
#########################################################

library("ggplot2")
library("reshape2")

###### feature Space Perf Comparisons ###################

# Data for different feature types each using 500 features
epsFile = "featureSpacePerfCmp.eps";
data = NULL;
data = rbind(data, c(0.64, 0.52, 0.76, 0.28));
data = rbind(data, c(0.73, 0.76, 0.70, 0.46));
data = rbind(data, c(0.79, 0.80, 0.78, 0.57));
data = rbind(data, c(0.80, 0.81, 0.79, 0.60));
rownames(data) = c("PSN", "n-grams", "nGDip", "COMB");
colnames(data) = c("Accuracy", "Sensitivity", "Specificity", "MCC");

# Data for subset combinations of top 500 features
epsFile = "featureSpacePerfCmp2.eps";
data = NULL;
data = rbind(data, c(0.79, 0.80, 0.78, 0.59));
data = rbind(data, c(0.73, 0.76, 0.71, 0.47));
data = rbind(data, c(0.78, 0.80, 0.76, 0.57));
data = rbind(data, c(0.80, 0.81, 0.79, 0.60));
  # COMB1 = nGrams(258) + nGDip(242)
  # COMB2 = nGrams(283) + PSF(217)
  # COMB3 = nGDip(269) + PSF(231)
  # COMB  = nGrams() + nGDip() + PSF()

rownames(data) = c("COMB1", "COMB2", "COMB3", "COMB");
colnames(data) = c("Accuracy", "Sensitivity", "Specificity", "MCC");

# Draw the graph
df <- melt(data);
featureSpacePerfCmp = ggplot(df,aes(x=Var1, y = value)) +
  theme_bw(base_size = 36, base_family = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  theme(aspect.ratio = 0.6) +
  geom_bar(aes(fill = Var2),stat = "identity", position = "dodge") +
  labs(x = "Feature Extraction Technique", y = "Performance score") +
  ylim(0,1)

postscript(file = epsFile, paper = "letter");
featureSpacePerfCmp;
dev.off();

