if(!requireNamespace("GenOrd", quietly = TRUE)) {
  stop("Package 'GenOrd' is required for this simulated data set. Please install it.")
}

set.seed(1)

Pt_v2<-stats::rbinom(50, 1, 0.4)
Pt_v5<-stats::rbinom(50, 1, 0.6)
Pt_v6<-stats::rbinom(50, 1, 0.8)

V9<-c(0.4,0.7,0.9)
V11<-c(0.4,0.7,0.9)


marg_1<-list(V9, V11)

R_check<-matrix(c(1, 0.95, 0.95, 1), nrow=2, ncol=2, byrow=TRUE)

library(methods)
Pt_v9v11<-GenOrd::ordsample(50, marg_1, R_check)

Pts<-data.frame(as.factor(Pt_v2), as.factor(Pt_v5), as.factor(Pt_v6), as.factor(Pt_v9v11[,1]), as.factor(Pt_v9v11[,2]))
colnames(Pts)<-c("V2", "V5", "V6", "V9", "V11")



rm(Pt_v9v11)
rm(R_check)
rm(Pt_v2)
rm(Pt_v5)
rm(Pt_v6)
rm(V11)
rm(V9)
rm(marg_1)


