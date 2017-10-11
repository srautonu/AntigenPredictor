Sn  = c(61.64, 57.53, 54.79, 53.42, 52.05)
Sp  = c(55.32, 60.79, 65.76, 71.44, 76.19)
Acc = c(55.64, 60.63, 65.21, 70.54, 74.98)

P = 73;
N = 1390;

for (i in 1:length(Sens)) {
  TP = Sn[i] * P / 100;
  TN = Sp[i] * N / 100;
  
  FP = N - TN;
  FN = P - TP;
  
  mcc = (TP * TN - FP * FN)/sqrt((TP + FP)*(TP + FN)*(TN + FN)*(TN + FP))
  prec = TP / (TP + FP) * 100
  
  cat("DiffCheck:", TP + TN - Acc[i] * (P + N) / 100, "mcc:", mcc, "prec:", prec, "\n")
}
  
