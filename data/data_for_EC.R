if(!requireNamespace("GenOrd", quietly = TRUE)) {
  stop("Package 'GenOrd' is required for this simulated data set. Please install it.")
}

set.seed(1)

V7<-c(0.4,0.7,0.9)
V8<-c(0.4,0.7,0.9)
V9<-c(0.4,0.7,0.9)
V10<-c(0.4,0.7,0.9)
V11<-c(0.4,0.7,0.9)
V12<-c(0.4,0.7,0.9)
V13<-c(0.4,0.7,0.9)


marg_1<-list(V9, V11)

R_check<-matrix(c(1, 0.95, 0.95, 1), nrow=2, ncol=2, byrow=TRUE)

library(methods)
data<-GenOrd::ordsample(100, marg_1, R_check)

V1<-stats::rbinom(100, 1, 0.05)
V2<-stats::rbinom(100, 1, 0.4)
V3<-stats::rbinom(100, 1, 0.05)
V4<-stats::rbinom(100, 1, 0.05)
V5<-stats::rbinom(100, 1, 0.6)
V6<-stats::rbinom(100, 1, 0.8)

marg_cat<-list(V7, V8, V10, V12, V13)
R_cat<-matrix(rep(0, 25), nrow=5, ncol=5)

for (i in 1:nrow(R_cat)){
  for (a in 1:ncol(R_cat)){
    if (i==a){
      R_cat[i,a]=1.0
    } 
  }
}


data_cat<-GenOrd::ordsample(100, marg_cat, R_cat)

assets_fullset<-data.frame(as.factor(V1), as.factor(V2), as.factor(V3), as.factor(V4), as.factor(V5), as.factor(V6), as.factor(data_cat[,1]), as.factor(data_cat[,2]), as.factor(data[,1]), as.factor(data_cat[,3]), as.factor(data[,2]), as.factor(data_cat[,4]), as.factor(data_cat[,5]))
colnames(assets_fullset)<-c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13")

rm(V1)
rm(V2)
rm(V3)
rm(V4)
rm(V5)
rm(V6)
rm(V7)
rm(V8)
rm(V9)
rm(V10)
rm(V11)
rm(V12)
rm(V13)
rm(R_check)
rm(R_cat)
rm(data)
rm(data_cat)
rm(marg_1)
rm(marg_cat)
rm(i)
rm(a)


assets<-assets_fullset[,-1]
assets<-assets[,-(2:3)]

rm(assets_fullset)

set.seed(1)
hv012<-sample(0:10, 100, replace=TRUE)
hv013<-sample(1:10, 100, replace=TRUE)
hv005<-sample(60000:120000, 100, replace=TRUE)


for (i in 1:length(hv012)) {
  if(hv012[i] ==0) {
    hv012[i] = hv013[i]
  } 
}
Weights = (hv012 * hv005) / 1000000
data_for_EC<-data.frame(Weights,assets)


rm(hv012)
rm(hv013)
rm(hv005)
rm(Weights)
rm(assets)
rm(i)



