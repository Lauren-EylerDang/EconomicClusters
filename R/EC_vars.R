EC_vars<-function(X, p){
  bin_test<-vector()
  for(i in 1:ncol(X)){
    if(length(levels(X[,i]))==2){
      bin_test[i]<-TRUE
    } else {
      bin_test[i]<-FALSE
    }
  }
  assets_num<-X[,which(bin_test==TRUE)]
  assets_num<-data.matrix(assets_num)
  for (a in 1:ncol(assets_num)){
    if(any(assets_num[,a]==2)){
      for(i in 1:nrow(assets_num)){
        if (assets_num[i,a]==1){
          assets_num[i,a]<-0
        } else{
          if(assets_num[i,a]==2){
            assets_num[i,a]<-1
        }}
      }
    }
  }
  common_names<-colnames(assets_num[,colMeans(assets_num, na.rm=TRUE)>p])
  pop<-X[,colnames(X) %in% common_names==TRUE]
  pop<-cbind(pop, X[,which(bin_test==FALSE)])
  return(pop)
#' @export
}

