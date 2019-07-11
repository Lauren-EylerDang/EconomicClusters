EC_vars<-function(X, p){
  assets_num<-data.matrix(X)
  common_names<-colnames(assets_num[,colMeans(assets_num, na.rm=TRUE)>p])
  pop<-X[,colnames(X) %in% common_names==TRUE]
  pop<-lapply(pop, as.factor)
  pop<-as.data.frame(pop)
  return(pop)
  #' @export
}
