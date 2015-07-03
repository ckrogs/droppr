#' Assess sensitivity to dropped cases
#' 
#' Implements several dropping methods for assessing result sensitivity,
#' including nth-order jackknife and DBSCAN 
#'  
#' 
#' @param d a data frame
#' @param analysis a function, see details
#' @param method one of "jackknife" or "dbscan"
#' @param n.drops number of units to drop in each iteration (jackknife only)
#' @param unit.vars character containing names of variables used to identify unique units (jackknife only)
#' @param eps a vector of reachability distances (dbscan only)
#' @param min.pts a vector of minimum points values (dbscan only)
#' @param ... optional arguments passed to \code{fpc::dbscan()}
#' @param time.warn logical; whether to give runtime estimate before full execution
#' @param verbose logical; whether to print execution information
#' 
#' @details The \code{analysis} argument must be a function taking only one argument named after
#' the object representing the initial data used in the subsequent analysis.  The body of the
#' function contains the analytic procedure to be applied to each dataset after case dropping.
#' See the examples for more.
#' 
#' @return Returns a named list with the following components: 
#'
#'\item{\code{base.result}}{result of analysis function on full data}
#'\item{\code{drop.order}}{vector of dropped units; index corresponds to drop.results}
#'\item{\code{drop.results}}{a list of dropped results; index corresponds to drop.order }
#'\item{\code{drop.results.classes}}{a character vector containing result classes}
#'\item{\code{drop.method}}{the dropping method used}
#'\item{\code{data}}{a copy of the original dataset; used in \code{droppr_analyze()}}
#'\item{\code{clusters}}{a vector of method-specific cluster/drop groups}
#'
#'@examples
#'
#'# Suppose we have the following analysis:
#'
#'# reg<-lm(logpgp95~avexpr+logem4, data = ajr)
#'# summary(reg)$coefficients
#'
#'# Then the \code{analysis} argument should take a function as follows:
#'
#'# myfunc<-function(ajr){
#'#    reg<-lm(logpgp95~avexpr+logem4, data = ajr)
#'#    summary(reg)$coefficients
#'# }
droppr_main<-function(d, analysis, method, n.drops=NULL, unit.vars=NULL, eps=NULL, min.pts=NULL, ..., time.warn=TRUE, verbose=TRUE){
  
  # Make spare data copy
  d.copy<-d
  
  # Jackknife
  if(method=="jackknife"){
    data<-d
    
    base.result<-analysis(data)
    
    if(length(unit.vars)>1){
      drops<-apply(X = d[,unit.vars], MARGIN = 1, FUN = paste0, collapse=",") 
    }else{
      drops<-d[,unit.vars]
    }
    drops.unique<-unique(drops)
    drops.search<-utils::combn(x = drops.unique, m = n.drops)
    drop.order<-apply(X = drops.search, MARGIN = 2, FUN = paste0, collapse=";")

    if(time.warn){
      sam<-sample(x = 1:ncol(drops.search), size = 100, replace = F)
      est.time<-system.time(expr = {
        drop.results<-parallel::mclapply(X = sam, FUN = function(x){
          tryCatch(expr = {
            data<-data[!drops %in% drops.search[,x],]
            analysis(data) 
          }, error=function(e) "error")
        })
      })
      cont<-readline(prompt = paste0("The estimated runtime is ", 
                                     round(est.time[3]/60*(ncol(drops.search)/100), 2), 
                                     " minutes.  Do you wish to continue (y/n)? "))
      if(cont=="n"){
        return()
      }
    }
    
    cat("Running jackknife...\n")
    drop.results<-parallel::mclapply(X = 1:ncol(drops.search), FUN = function(x){
      tryCatch(expr = {
        data<-data[!drops %in% drops.search[,x],]
        analysis(data) 
      }, error=function(e) "error")
    })

    drop.results.classes<-unique(sapply(X = drop.results, class))
    
    clusters<-drop.order
  }

  # DBSCAN
  if(method=="dbscan"){
    cat("Running DBSCAN...\n")
    
    data<-droppr::droppr_dbscan_data(d = ajr, eps = eps, min.pts = min.pts)
    
    base.result<-analysis(data)
    
    drop.order<-sort(unique(data$droppr_cluster), na.last = TRUE)
    
    drop.results<-parallel::mclapply(X = drop.order, FUN = function(cluster){
      tryCatch(expr = {
        analysis(data[!data$droppr_cluster %in% cluster,])
      }, error=function(e) "error")
    })
    
    drop.results.classes<-unique(sapply(X = drop.results, class))
    
    clusters<-data$droppr_cluster
    
  }
  
  # Return
  output<-list(base.result=base.result, 
               drop.order=drop.order, 
               drop.results=drop.results, 
               drop.results.classes=drop.results.classes,
               drop.method=method,
               data=d,
               clusters=clusters)
  class(output)<-c("droppr", paste0("droppr.", method))
  cat("Done.\n")
  return(output)
}
