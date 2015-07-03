#' Prepare dataset for \code{droppr} with DBSCAN
#' 
#' Adds a column of DBSCAN clusters to a data frame
#' 
#' @param d a data frame
#' @param eps reachability distance
#' @param min.pts minimum number of points
#' @param ... optional arguments passed to \code{fpc::dbscan()}
#' 
#' @return Returns a data frame with a column of DBSCAN clusters
droppr_dbscan_data<-function(d, eps, min.pts, ...){

  # Reserve original data
  data<-d
  
  # Remove incomplete cases
  data<-data[complete.cases(data),]
  
  # Remove columns that are non-numeric
  data<-data[,sapply(data,is.numeric)]
  
  # Run DBSCAN
  clusters<-fpc::dbscan(data = data, eps = eps, MinPts = min.pts)$cluster+1
  
  # Add clusters to original data
  d[complete.cases(d),"droppr_cluster"]<-clusters
  
  # Return
  return(d)
}