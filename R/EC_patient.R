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
  group<-as.numeric(group)
  
  unclassified <- Pts[which(is.na(group)==TRUE),]
  equalrows <- function(df, pt){
    equal <- all(df == pt)
    return(equal)
  }
  
  
  Pop_naclust_list <- list()
  for(i in 1:nrow(unclassified)){
    Pop_naclust_list[[i]]<-unique(PopClusters[which(apply(Pop, 1, equalrows, pt = unclassified[i,])==TRUE)])
  }
  
  whichmiss <- which(is.na(group))
  
  for(i in 1:length(whichmiss)){
    if(length(Pop_naclust_list[[i]])==1){
      group[whichmiss][i]<-Pop_naclust_list[[i]]
    } 
  }
  return(group)
  #' @export
}