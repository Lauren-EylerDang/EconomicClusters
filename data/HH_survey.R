set.seed(1)
dejure<-sample(0:10, 1000, replace=TRUE)
defacto<-sample(1:10, 1000, replace=TRUE)
HHwt<-sample(60000:120000, 1000, replace=TRUE)

HH_survey<-cbind(HHwt, dejure, defacto)
HH_survey<-as.data.frame(HH_survey)

rm(dejure)
rm(defacto)
rm(HHwt)



