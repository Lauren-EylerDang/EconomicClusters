EC_patient<-function(Pts, Medoids, Pop=NULL, PopClusters=NULL){
  ptmed<-matrix(nrow=2,ncol=ncol(Pts))
  dizzy<-matrix(nrow=nrow(Pts),ncol=nrow(Medoids))
  for (i in 1:nrow(Pts)){
    for (a in 1:nrow(Medoids)){
      ptmed<-rbind(Pts[i,], Medoids[a,])
      dizzy[i,a]<-cluster::daisy(ptmed, metric="gower")
      rm(ptmed)
    }
  }
  colnames(dizzy)<-rownames(Medoids)
  group<-list()
  for (i in 1:nrow(Pts)){
    group[[i]]<-colnames(dizzy)[which(dizzy[i,] == min(dizzy[i,]))]
  }
  for (i in 1:nrow(Pts)){
    if (length(group[[i]])>1){
      group[[i]]<-NA
    }
  }
  group<-unlist(group)
  group<-as.factor(group)
  PatientsNAClustwhich<-which(is.na(group)==TRUE)
  PatientsNAClust<-Pts[PatientsNAClustwhich,]
  Pop_naclust <- rep(NA, nrow(PatientsNAClust))
  for(i in 1:nrow(PatientsNAClust)){
    if(length(unique(PopClusters[which(Pop[i,]==PatientsNAClust[i,])]))==1){
      Pop_naclust[i]<-unique(PopClusters[which(Pop[i,]==PatientsNAClust[i,])])
    }
  }
  group[which(is.na(group)==TRUE)]<-Pop_naclust
  return(group)
  #' @export
}