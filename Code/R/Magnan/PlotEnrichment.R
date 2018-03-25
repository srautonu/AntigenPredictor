#########################################################
# For writing to R window comment out the following line
# in each graph
# theme_bw(base_size = 36, base_family = "") +
#########################################################
library("ggplot2")
library("XLConnect")

xlsFile  = c(
  "VaxiJen_IT.xlsx",
  "AntigenPRO_IT.xlsx",
  "PerfSearch_RF_SvmRFE2_comb.xlsx",
  "PerfSearch_RF_SvmRFE2_comb.xlsx"
  );
xlsSheet = c(
  "Enrichment",
  "Enrichment",
  "ITEnrichment_2_490",
  "ITEnrichment_1_500"
  );

toolName = c("VaxiJen", "ANTIGENpro", "Antigenic",  "Antigenic*")

enrichmentFile = "enrichment.eps"

data = NULL;
for (i in 1:length(xlsFile)) {
  
  workBook = loadWorkbook(xlsFile[i])
  df = readWorksheet(workBook, xlsSheet[i]);
  df$Tool = toolName[i];

  data = rbind(data, df);
}

enrichment = ggplot(data,aes(x=TopRank, y=Enrichment)) + 
  theme_bw(base_size = 36, base_family = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  geom_line(aes(colour = Tool),size = 2) +
  labs(x = "Top ranked %", y = "Enrichment");

postscript(file = enrichmentFile, paper = "letter");
print(enrichment);
dev.off();
