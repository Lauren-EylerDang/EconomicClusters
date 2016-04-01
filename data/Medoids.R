Medoids<-matrix(c(0,1,1,1,1,0,1,1,1,2,0,1,1,1,3,0,1,1,1,4,0,1,1,2,1,0,1,1,2,3,0,1,1,2,2,0,1,1,2,4,0,1,1,3,1,0,1,1,3,2,0,1,1,3,4,0,1,1,3,3), nrow=12, ncol=5, byrow=TRUE)
Medoids<-data.frame(as.factor(Medoids[,1]), as.factor(Medoids[,2]), as.factor(Medoids[,3]), as.factor(Medoids[,4]), as.factor(Medoids[,5]))
colnames(Medoids)<-c("V2", "V5", "V6", "V9", "V11")
rownames(Medoids)<-c("2", "976", "40", "18", "927", "995", "946", "923", "702", "958", "857", "987")
