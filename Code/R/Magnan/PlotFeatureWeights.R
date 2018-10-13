#########################################################
# For writing to R window comment out the following line
# in each graph
# theme_bw(base_size = 36, base_family = "") +
#########################################################

library("ggplot2")
library("reshape2")

# Feature weights bar plot
rfmodel = readRDS("Balanced_RFModel.rds");
ranked = rfmodel$importance[order(-rfmodel$importance),]

df = melt(ranked[1:25])

featureWeightsBar = ggplot(df,aes(x=reorder(rownames(df), -value), y = value)) +
  theme_bw(base_size = 18, base_family = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  theme(aspect.ratio = 0.6) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #theme(axis.text.x = element_blank()) +
  geom_bar(aes(fill=(value > 0)),stat = "identity", position = "dodge") +
  labs(x = "Features", y = "Importance score")

postscript("featureWeightsBar.eps", paper = "letter");
featureWeightsBar + scale_fill_grey(start = 0, end = 0);
dev.off();
