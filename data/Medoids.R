Medoids<-matrix(c(1,0,1,1,1,0,0,0,2,2,1,1,1,3,3,1,1,1,1,1,0,1,1,2,2,1,0,1,2,2,0,0,1,1,1,0,0,1,3,3,1,1,1,4,4,0,1,1,1,1), nrow=10, ncol=5, byrow=TRUE)
Medoids<-data.frame(as.factor(Medoids[,1]), as.factor(Medoids[,2]), as.factor(Medoids[,3]), as.factor(Medoids[,4]), as.factor(Medoids[,5]))
colnames(Medoids)<-c("V2", "V5", "V6", "V9", "V11")
rownames(Medoids)<-c("14", "23", "43", "77", "78", "82", "84", "87", "95", "99")
