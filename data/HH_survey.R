set.seed(1)
dejure<-sample(0:10, 100, replace=TRUE)
defacto<-sample(1:10, 100, replace=TRUE)
HHwt<-sample(60000:120000, 100, replace=TRUE)

HH_survey<-cbind(HHwt, dejure, defacto)
HH_survey<-as.data.frame(HH_survey)

rm(dejure)
rm(defacto)
rm(HHwt)



