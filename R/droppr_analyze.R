#' Analyze results from \code{droppr_main}
#' 
#' Implements several method-specific routines for clarifying
#' results from \code{droppr_main}
#'  
#' 
#' @param droppr.out an object of class "droppr"
#' 
#' @return Returns a named list with method-specific components. 
droppr_analyze<-function(droppr.out){
    
  if(!any(class(droppr.out) %in% "droppr")){
    stop("Must be a droppr object.", call. = FALSE)
  }
  
  # Cluster size
  cluster.size<-data.frame(table(droppr.out$clusters, useNA = "always")/length(droppr.out$clusters), stringsAsFactors = F)
  colnames(cluster.size)<-c("cluster", "prop")
  
  # lm/glm processing
  if(droppr.out$drop.results.classes %in% c("lm", "glm")){
    out<-lapply(X = 1:length(droppr.out$drop.order), FUN = function(x){
      reg.sum<-summary(droppr.out$drop.results[[x]])
      coefs<-data.frame(var=rownames(reg.sum$coefficients), cluster=droppr.out$drop.order[[x]], reg.sum$coefficients, 
                        row.names = NULL, stringsAsFactors = F)
      colnames(coefs)[-c(1,2)]<-c("e", "se", "stat", "p")
      coefs
    })
    out<-do.call("rbind", out)
    out<-out[!out$var %in% "(Intercept)",]
    out<-plyr::join(x = out, y = cluster.size, type = "left")
    
    plot<-ggplot2::ggplot(data = out, ggplot2::aes(x = p, y = e, color=var))+ggplot2::geom_jitter(size=4)+
      ggplot2::geom_vline(x=0.05, linetype=2)+ggplot2::geom_hline(y=0, linetype=2)+
      ggplot2::theme_bw()+ggplot2::xlab("P-Value")+ggplot2::ylab("Coefficient")+ggplot2::scale_color_discrete(name="")
    
    plot.size.scale<-ggplot2::ggplot(data = out, ggplot2::aes(x = p, y = e, color=var, size=1-prop))+ggplot2::geom_jitter()+
      ggplot2::geom_vline(x=0.05, linetype=2)+ggplot2::geom_hline(y=0, linetype=2)+
      ggplot2::theme_bw()+ggplot2::xlab("P-Value")+ggplot2::ylab("Coefficient")+ggplot2::scale_color_discrete(name="")+
      ggplot2::scale_size_continuous(name="Sample\nProportion")
  }  
  else if(droppr.out$drop.results.classes %in% c("summary.lm", "summary.glm")){
    out<-lapply(X = 1:length(droppr.out$drop.order), FUN = function(x){
      reg.sum<-droppr.out$drop.results[[x]]
      coefs<-data.frame(var=rownames(reg.sum$coefficients), cluster=droppr.out$drop.order[[x]], reg.sum$coefficients, 
                        row.names = NULL, stringsAsFactors = F)
      colnames(coefs)[-c(1,2)]<-c("e", "se", "stat", "p")
      coefs
    })
    out<-do.call("rbind", out)
    out<-out[!out$var %in% "(Intercept)",]
    out<-plyr::join(x = out, y = cluster.size, type = "left")    
    
    plot<-ggplot2::ggplot(data = out, ggplot2::aes(x = p, y = e, color=var))+ggplot2::geom_jitter(size=4)+
      ggplot2::geom_vline(x=0.05, linetype=2)+ggplot2::geom_hline(y=0, linetype=2)+
      ggplot2::theme_bw()+ggplot2::xlab("P-Value")+ggplot2::ylab("Coefficient")+ggplot2::scale_color_discrete(name="")
    
    plot.size.scale<-ggplot2::ggplot(data = out, ggplot2::aes(x = p, y = e, color=var, size=1-prop))+ggplot2::geom_jitter()+
      ggplot2::geom_vline(x=0.05, linetype=2)+ggplot2::geom_hline(y=0, linetype=2)+
      ggplot2::theme_bw()+ggplot2::xlab("P-Value")+ggplot2::ylab("Coefficient")+ggplot2::scale_color_discrete(name="")+
      ggplot2::scale_size_continuous(name="Sample\nProportion")
  }
  else{
    return("No analysis method available for this class.")
  }
  
  # Return
  out<-list(cluster.size=cluster.size,
            results=out,
            results.plot=list(plot, plot.size.scale))
  class(out)<-"droppr.analyze"
  return(out)
}