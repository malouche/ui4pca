#' Visualize variables in PCA
#'
#' @export dviz_var
#'
#' @return a factorial map for the variables
#'
#' @example \dontrun {dviz_var}
#'
#' @import shiny,ggplot2, plyr, FactoMineR, factoextra, DT, ggrepel, scales,grid,gridExtra
#'

dviz_var<-function(pc,namesV,scale=T,axes=c(1,2),geom=c("text","arrow"),repel=T,select.var=0,supvar=F){
  circle <- function(center = c(0, 0), npoints = 100) {
    r = 1
    tt = seq(0, 2 * pi, length = npoints)
    xx = center[1] + r * cos(tt)
    yy = center[1] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  corcir = circle(c(0, 0), npoints = 100)

  correlations = as.data.frame(pc$var$coord[,axes])
  colnames(correlations) = c("pc1","pc2")


  v1=paste("Dim ", axes[1], " (", round(pc$eig[axes[1],2], 1), "%)", sep = "")
  v2=paste("Dim ", axes[2],  " (", round(pc$eig[axes[2],2], 1), "%)", sep = "")

  cos2<-as.data.frame(pc$var$cos2[,axes])
  cos2<-rowSums(cos2)
  i=which(cos2>select.var)

  if(length(i)>0){
    correlations=correlations[i,]
    d = nrow(correlations)
    correlations$cols=c(rep("v1",d-1),"v2")

    arrows = data.frame(x1 = rep(0, d), y1 = rep(0, d), x2 = correlations[, 1],
                        y2 = correlations[, 2])


    cercor <- ggplot()
    if(scale==T)   cercor <- cercor + geom_path(data = corcir, aes(x = x, y = y), colour = "gray65")

    if("arrow"%in%geom) cercor <- cercor + geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), colour ="gray65")

    if("point"%in%geom) cercor <- cercor + geom_point(data = correlations,  aes(x = pc1, y = pc2), colour = "gray65")


    if("text"%in%geom)  {
      if (repel==T) cercor <- cercor +  geom_text_repel(data = correlations, aes(x = pc1, y = pc2, label = rownames(correlations)))
      if (repel==F) cercor <- cercor +  geom_text(data = correlations, aes(x = pc1, y = pc2, label = rownames(correlations)))

    }

    cercor <- cercor +  geom_hline(yintercept = 0, colour = "gray65") + geom_vline(xintercept = 0,colour = "gray65") +
      xlim(-1.1, 1.1) + ylim(-1.1, 1.1) + labs(x = v1, y = v2)
  }
  else stop("No variables to be drawn")

  #
  if(supvar==T){
    ii=which(rownames(pc$quanti.sup$coord)%in%namesV)
    if (length(ii)>1){
      dsup=pc$quanti.sup$coord[ii,axes]
      colnames(dsup)=c("x","y")
      dsup=data.frame(dsup)
      ll=nrow(dsup)
      rownames(dsup)=rownames(pc$quanti.sup$coord)
      arrows2 = data.frame(x1 = rep(0,ll), y1 = rep(0,ll), x2 = dsup[,1],
                           y2 = dsup[,2])

      cercor<- cercor+ geom_segment(data = arrows2, aes(x = x1, y = y1, xend = x2, yend = y2),
                                    colour ="blue")
      cercor<- cercor +geom_text(data=dsup,aes(x=x,y=y,label=rownames(dsup)),colour="blue")
    }
    if(length(ii)==1){
      dsup=c(pc$quanti.sup$coord[1,axes[1]],pc$quanti.sup$coord[1,axes[2]])
      arrows2 = data.frame(x1 = 0, y1 = 0, x2 = dsup[1],
                           y2 = dsup[2])
      cercor<- cercor+ geom_segment(data = arrows2, aes(x = x1, y = y1, xend = x2, yend = y2), colour ="blue")
      cercor <- cercor + annotate(geom = "text",x = dsup[1],y = dsup[2],colour = "blue",label=namesV)
      cercor <- cercor + annotate(geom = "segment",x=0,y=0,xend = dsup[1],yend = dsup[2],colour = "blue",label=namesV)
    }
  }



  return(cercor+theme_classic())

}
