# lipiji@baidu.com
# ROC Curve
library(ROCR)

a=read.table("E:\\Project\\mpp\\personal\\lipiji\\pripub\\roc_pr\\toy.txt")
a <- as.matrix(a)

pred <- prediction(a[,1], a[,2])
perf <- performance(pred,"tpr","fpr")

auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc <- round(auc, 4)

plot(perf,colorize=TRUE,lwd=5,xlab="FPR",ylab="TPR", main=paste("AUC=",auc*100,"%",sep=""))

grid(5, 5, lwd = 1)
lines(par()$usr[1:2], par()$usr[3:4], lty=2, lwd=2, col="grey")

