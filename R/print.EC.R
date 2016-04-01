print.EC<-function(x, ...){
  cat("Results of Economic Clustering\n")
  cat("Max ASW:\n")
  cat(paste(x$ASW_max),"\n")
  cat("Assets:\n")
  cat(paste(x$Assets),"\n")
  cat("Number of Clusters:\n")
  cat(paste(x$K),"\n")
  cat("Cluster Medoids:\n")
  cat(paste(x$Medoids),"\n")
  cat("\n")
  cat("For a data frame containing only cluster medoids with only clustering variables: x$Medoid_dataframe\n")
  cat("\n")
  cat("For a vector of cluster membership for all data points: x$Cluster\n")
  cat("\n")
  cat("For a data frame containing the full population's responses to the clustering variables: x$Population_assets")
#' @export
}