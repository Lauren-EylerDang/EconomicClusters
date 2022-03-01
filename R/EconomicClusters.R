EconomicClusters<-function(X, Y=rep(NA, nrow(X)), nvars, kmin, kmax, ncores, threshold=NA, minsize=NA){
  if (class(X[,1])=="numeric"){
    wt<-X[,1]
    X<-X[,-1]
  } else {
    wt<-rep(1, nrow(X))
  }
  col_cat<-seq(1,ncol(X),1)
  combn<-utils::combn(c(1:ncol(X)),nvars)
  col_indx=matrix(c("NULL"), ncol(combn), ncol(X), byrow=FALSE)
  for (i in 1:ncol(combn)){
    col_indx[i,]<-t(is.element(col_cat, combn[,i]))
    col_indx[i,][col_indx[i,]==FALSE]<-NA
    colnames(col_indx)=colnames(X)
  }
  doParallel::registerDoParallel(cores=ncores)
  ASW<-matrix(nrow=nrow(col_indx), ncol=(kmax-kmin+1))
  #' @import foreach
  #' @import doParallel
  if (!is.na(threshold)) {  
    for(n in 1:ncol(ASW)){
      ASW[,n]<-foreach::foreach (i=1:nrow(col_indx), .combine='c')  %dopar% {
        combi<-X[,!is.na(col_indx[i,])]
        if(any(!is.na(Y))){ 
          combi<-cbind(Y, combi)
        }
        combi_mat<-data.matrix(combi)
        parD<-parDist(combi_mat, method =  "hamming") 
        wcKMR<-WeightedCluster::wcKMedRange(parD, kvals=(kmin), weights=wt)
        ASW[i,n]<-wcKMR$stats[,5]
       if (is.na(minsize)==FALSE){
         if (any(prop.table(table((wcKMR$clustering)))<minsize)){
          ASW[i,n]<-NA
        } }
        rm(combi)
        rm(dizzy)
        rm(wcKMR)
        return(ASW[i,n])
      }
      ###
      
      if (any(ASW>threshold, na.rm=TRUE)){
        break}
    }
  } else {
    for(n in 1:ncol(ASW)){
      ASW[,n]<-foreach::foreach (i=1:nrow(col_indx), .combine='c')  %dopar% {
        combi<-X[,!is.na(col_indx[i,])]
        if(any(!is.na(Y))){ 
          combi<-cbind(Y, combi)
        }
        combi_mat<-data.matrix(combi)
        parD<-parDist(combi_mat, method =  "hamming") 
        wcKMR<-WeightedCluster::wcKMedRange(parD, kvals=(kmin+n-1), weights=wt)
        ASW[i,n]<-wcKMR$stats[,5]
        if (is.na(minsize)==FALSE){
        if (any(prop.table(table((wcKMR$clustering)))<minsize)){
          ASW[i,n]<-NA
        } }
        rm(combi)
        rm(dizzy)
        rm(wcKMR)
        return(ASW[i,n])
      }
    }
  }
  
  ASW_max<-max(ASW, na.rm=TRUE)
  ASW_max_model<-which(ASW == ASW_max, arr.ind=TRUE)
  model<-list()
  Assets<-list()
  clust<-list()
  K<-list()
  for (a in 1:nrow(ASW_max_model)){
    model[[a]]<-ASW_max_model[a,1]    
    Assets[[a]]<-which( !is.na(col_indx[model[[a]],]), arr.ind=TRUE)
    Assets[[a]]<-names(Assets[[a]])
    if (any(!is.na(Y))){
      Assets[[a]]<-c(deparse(substitute(Y)), Assets[[a]])
    }
    clust[[a]]<-ASW_max_model[a,2]
    K[[a]]<-kmin-1+clust[[a]]
  }
  results<-list()
  results$ASW_max<-ASW_max
  results$Assets<-Assets
  results$K<-K
  win_ds<-list()
  win_daisy<-list()
  win_wcKMedRange<-list()
  Group<-list()
  Medoids<-list()
  Medoid_df<-list()
  for (a in 1:length(model)){
    win_ds[[a]]<-X[,!is.na(col_indx[model[[a]],])]
    if(any(!is.na(Y))){ 
      win_ds[[a]]<-cbind(Y, win_ds[[a]])
    }
    win_ds[[a]]<-data.matrix(win_ds[[a]])
    win_daisy[[a]]<-parDist(win_ds[[a]], method =  "hamming")
    win_wcKMedRange[[a]]<-WeightedCluster::wcKMedRange(win_daisy[[a]], kvals=K[[a]], weights=wt)
    Group[[a]]<-win_wcKMedRange[[a]]$clustering
    Medoids[[a]]<-c(unique(win_wcKMedRange[[a]]$clustering))
    Medoid_df[[a]]<-X[as.numeric(rownames(X)) %in% unlist(Medoids[[a]],) ==TRUE, !is.na(col_indx[model[[a]],])]
    if (any(!is.na(Y))){
      Medoid_df[[a]]<-cbind(Y[as.numeric(rownames(X)) %in% unlist(Medoids[[a]],) ==TRUE], Medoid_df[[a]])
      colnames(Medoid_df[[a]])<-c(deparse(substitute(Y)), colnames(Medoid_df[[a]][,-1]))
    }
  }
  results$Medoids<-Medoids
  results$Medoid_dataframe<-Medoid_df
  if (length(results$Medoid_dataframe)==1){
    results$Medoid_dataframe<-as.data.frame(results$Medoid_dataframe)
  }
  results$Cluster<-Group
  if (length(results$Cluster)==1){
    results$Cluster<-unlist(results$Cluster)
    results$Cluster<-as.factor(results$Cluster)
  }
  results$Population_assets<-list()
  for(a in 1:length(Assets)){
    results$Population_assets[[a]]<-X[,colnames(X) %in% results$Assets[[a]]]
  }
  if(length(results$Assets)==1){
    results$Population_assets<-as.data.frame(results$Population_assets[[1]])
  }
  class(results)<- "EC"
  return(results)
  #' @export
}

