data = read.csv("temp.csv")

# Sn  = c(61.64, 57.53, 54.79, 53.42, 52.05)
# Sp  = c(55.32, 60.79, 65.76, 71.44, 76.19)
# Acc = c(55.64, 60.63, 65.21, 70.54, 74.98)
Sn  = data$Sensitivity
Sp  = data$Specificity
Acc = data$Accuracy

P   = 576;
N   = 576;
# P = 73;
# N = 1390;

mcc =c();
prec = c();


for (i in 1:length(Sn)) {
  TP = Sn[i] * P;
  TN = Sp[i] * N;
  
  FP = N - TN;
  FN = P - TP;
  
  curMcc = (TP * TN - FP * FN)/sqrt((TP + FP)*(TP + FN)*(TN + FN)*(TN + FP))
  curPrec = TP / (TP + FP)
  
  cat("DiffCheck:", TP + TN - Acc[i] * (P + N), "mcc:", curMcc, "prec:", curPrec, "\n")
  
  mcc  = c(mcc, curMcc);
  prec = c(prec, curPrec);
}
  
