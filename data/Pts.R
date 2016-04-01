if(!requireNamespace("GenOrd", quietly = TRUE)) {
  stop("Package 'GenOrd' is required for this simulated data set. Please install it.")
}

set.seed(1)

Pt_v2<-stats::rbinom(250, 1, 0.4)
Pt_v5<-stats::rbinom(250, 1, 0.6)
Pt_v6<-stats::rbinom(250, 1, 0.8)

V9<-c(0.4,0.7,0.9)
V11<-c(0.4,0.7,0.9)


marg_1<-list(V9, V11)

R_check<-matrix(c(1, 0.95, 0.95, 1), nrow=2, ncol=2, byrow=TRUE)

library(methods)
Pt_v9v11<-GenOrd::ordsample(250, marg_1, R_check)

Pts<-data.frame(as.factor(Pt_v2), as.factor(Pt_v5), as.factor(Pt_v6), as.factor(Pt_v9v11[,1]), as.factor(Pt_v9v11[,2]))
colnames(Pts)<-c("V2", "V5", "V6", "V9", "V11")

Medoids<-matrix(c(0,1,0,2,2,0,0,1,3,3,1,1,1,3,3,1,0,1,4,4,1,1,1,1,1,0,1,1,2,2,0,1,1,1,1,0,1,1,3,3,1,0,1,2,2,0,0,1,1,1,1,0,1,1,1,1,1,1,2,2), nrow=12, ncol=5, byrow=TRUE)
colnames(Medoids)<-c("V2", "V5", "V6", "V9", "V11")
rownames(Medoids)<-c("2", "976", "40", "18", "927", "995", "946", "923", "702", "958", "857", "987")
Medoids<-data.frame(as.factor(Medoids[,1]), as.factor(Medoids[,2]), as.factor(Medoids[,3]), as.factor(Medoids[,4]), as.factor(Medoids[,5]))

meds<-c(1, 2, 5,18,19,22,31,34,41,43,45,50,57,59,63,67,74,76,81,82,85,90,91,93, 95,103,110,115,119,127,134,141,147,150,155,158,164,165,176,177,192,198,203,212,217,228,229,240,244,245,246)
med_samp<-sample(meds, 30, replace=FALSE)



rm(Pt_v9v11)
rm(R_check)
rm(Pt_v2)
rm(Pt_v5)
rm(Pt_v6)
rm(V11)
rm(V9)
rm(marg_1)
rm(med_samp)
rm(meds)
rm(Medoids)

