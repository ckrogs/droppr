#' Identify optimal DBSCAN parameters
#' 
#' Iterates DBSCAN over various combinations of parameter values 
#' and returns the associated number of clusters
#' 
#' @param d a data frame
#' @param eps a vector of reachability distances
#' @param min.pts a vector of minimum points values
#' @param ... optional arguments passed to \code{fpc::dbscan()}
#' 
#' @return Returns a named list with the following components: 
#'
#'\item{\code{params.df}}{a data frame containing combinations of parameter values and the resulting number of DBSCAN clusters}
#'\item{\code{params.plot}}{a ggplot object depicting the elbow curve for parameter combinations}
droppr_dbscan_nclust<-function(d, eps, min.pts, ...){
  
  # Reserve original data
  data<-d
  
  # Remove incomplete cases
  data<-data[complete.cases(data),]
  
  # Remove columns that are non-numeric
  data<-data[,sapply(data,is.numeric)]
  
  # Create grid for simulations
  dbscan.params<-expand.grid(eps=eps,min.pts=min.pts)
  
  # Get number of clusters produced over different param combos
  dbscan.params$n.clusters<-sapply(X = 1:nrow(dbscan.params), FUN = function(row){
    tryCatch(expr = {
      out<-fpc::dbscan(data = data, eps = dbscan.params$eps[row], MinPts = dbscan.params$min.pts[row], ...)$cluster
      length(unique(out))
    }, error=function(e) 0)
  })
  dbscan.params<-plyr::arrange(dbscan.params, plyr::desc(n.clusters))
  
  # Create parameter data frame and plot
  dbscan.params<-data.frame(id=1:nrow(dbscan.params), dbscan.params, stringsAsFactors = F)
  dbscan.params.plot<-ggplot2::ggplot(data = dbscan.params, ggplot2::aes(x = id, y = n.clusters))+
    ggplot2::geom_point(size=3, shape=1)+ggplot2::xlab(label = "Parameter Combination ID")+ggplot2::ylab(label = "Number of Clusters")+
    ggplot2::scale_x_continuous(breaks=round(seq.int(from = 1, to = nrow(dbscan.params), length.out = 10), digits = 0))+ggplot2::theme_bw()
  
  # Return
  return(list(params.df=dbscan.params, params.plot=dbscan.params.plot))
}