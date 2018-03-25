data = read.csv("temp.csv")

 Sn  = c(89.69)
 Sp  = c(25.85)
 Acc = c(59.48)
# Sn  = data$Sensitivity
# Sp  = data$Specificity
# Acc = data$Accuracy

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
  
