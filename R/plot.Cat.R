#' Plotting function for Cat object
#'
#' Description.
#' 
#' @param x Cat.
#' @param item Numeric.
#' @param plotType Character.
#' @param xlim Vector.
#' @param ... Other arguments passed to plot().
#' 
#' 
#' @aliases plot.Cat plot,Cat plot,Cat-method
#' 
#' @rdname plot.Cat
#' @name plot.Cat
#' 
#' @importFrom grDevices rgb
#' @import graphics
#' @export plot
#' 
#' @export
setMethod("plot", c("Cat"),
          definition = function(x, item, plotType, xlim=c(-5, 5), ...) {
                #catObj <- ltm_cat
                #item<-1
                #xlim=c(-5,5)
              
                if(plotType != "ICC" & plotType != "IRF" & plotType != "IIF"){
                  stop("Options for plotType are ICC, IRF, and IIF")
                }
              
              catObj<-x  
              model<-catObj@model
              
              x<-seq(xlim[1]-1,xlim[2]+1,.1)
              
              
              ## DOM: Add an error here for any quesiton that has more than 16 response options  
                linecolors<-c(1, rgb(51/255, 153/255, 102/255), 
                                rgb(0/255, 102/255, 204/255),
                                rgb(204/255, 102/255, 153/255), 
                              rgb(102/255, 0/255, 102/255),
                              rgb(204/255, 51/255, 0/255),
                            ## Above is the customizable part. Codes below return randomized colors.
                            sapply(c(1:11),function(i){return(col=rgb(sample(100:255,1)/255,sample(100:255,1)/255,sample(100:255,1)/255))})) 
                
                ltys<-c(1:16)
              #    par(mar=c(3,3,2,9),mgp=c(1.5,0,0))
              
              
              ## calculate various quantities that we may want to plot
  
              if(plotType=="IIF"){
                par(mar=c(3,3,2,1),mgp=c(1.75,.25,0))
                fInf<-sapply(x,fisherInf, catObj=catObj, item=item)
                plot(x,fInf,"l", main="Item Information Function",col=linecolors[1],
                     ylab=expression(I(theta)),xlab=expression(theta), lwd=2,
                     xlim=xlim, ylim=c(0,max(fInf)), tck=F,cex.axis=.90, las=1)
                abline(h=0, lty=3, col="gray70")
              }
              
              
                if(model=="tpm"){
                  
                  prob<-as.matrix(sapply(x,probability, catObj=catObj, item=item), ncol=1)
                  expectedValue<-prob
                  par(mar=c(3,3,2,1),mgp=c(1.75,.25,0))
                  
                  if(plotType=="ICC"){
                    plot(x,expectedValue,"l", main=paste0("Item Characteristic Curve (", names(catObj@difficulty[item]), ")" ),col=linecolors[1],
                         ylab="Expected Value of Response",xlab=expression(theta), lwd=2,
                         xlim=xlim, ylim=c(0,max(expectedValue)), tck=F,cex.axis=.90, las=1)
                    abline(h=0, lty=3, col="gray70")
                    abline(h=1, lty=3, col="gray70")
                    abline(h=.5, lty=2, col="gray70")
                    #           segments(x0=-catObj@difficulty[item]/catObj@discrimination[item],  y0=0, y1=0.5, lwd=2, col="darkred", lty=2)
                  }
                  if(plotType=="IRF"){
                    plot(x,prob,"l", main=paste0("Item Response Function (", names(catObj@difficulty[item]), ")" ),col=linecolors[1],
                         ylab=expression(paste("Pr(Y=1|", theta, ")")),xlab=expression(theta), lwd=2,
                         xlim=xlim, ylim=c(0,max(expectedValue)), tck=F,cex.axis=.90, las=1)
                    abline(h=0, lty=3, col="gray70")
                    abline(h=1, lty=3, col="gray70")
                    abline(h=.5, lty=2, col="gray70")
                  }
                  
                }
                
           
                if(model=="ltm"){ 
                  prob<-as.matrix(sapply(x,probability, catObj=catObj, item=item), ncol=1)
                  expectedValue<-prob
                  par(mar=c(3,3,2,1),mgp=c(1.75,.25,0))
                  
                  if(plotType=="ICC"){
                    plot(x,expectedValue,"l", main="Item Characteristic Curve",col=linecolors[1],
                         ylab="Expected Value of Response",xlab=expression(theta), lwd=2,
                         xlim=xlim, ylim=c(0,max(expectedValue)), tck=F,cex.axis=.90, las=1)
                    abline(h=0, lty=3, col="gray70")
                    abline(h=1, lty=3, col="gray70")
                    abline(h=.5, lty=2, col="gray70")
                    #           segments(x0=-catObj@difficulty[item]/catObj@discrimination[item],  y0=0, y1=0.5, lwd=2, col="darkred", lty=2)
                  }
                  if(plotType=="IRF"){
                    plot(x,prob,"l", main=paste0("Item Response Function (", names(catObj@difficulty[item]), ")" ),col=linecolors[1],
                         ylab=expression(paste("Pr(Y=1|", theta, ")")),xlab=expression(theta), lwd=2,
                         xlim=xlim, ylim=c(0,max(expectedValue)), tck=F,cex.axis=.90, las=1)
                    abline(h=0, lty=3, col="gray70")
                    abline(h=1, lty=3, col="gray70")
                    abline(h=.5, lty=2, col="gray70")
                    segments(x0=-catObj@difficulty[item]/catObj@discrimination[item],  y0=0, y1=0.5, lwd=2, col="darkred", lty=2)
                  }
                } # End ltm if()
                
                
                
                if(model=="gpcm"){
                  par(mar=c(3,3,2,7.5),mgp=c(1.75,.25,0))
                  prob<-as.matrix(sapply(x,probability, catObj=catObj, item=item))

                  prob<-t(prob)
     
                  expectedValue<-prob%*%1:ncol(prob)
                #    expectedValue<-as.matrix(sapply(1:nrow(prob),function(i) weighted.mean(w=prob[i,],x=1:ncol(prob))))
                  #  expectedValue<-as.matrix(sapply(1:nrow(prob),function(i) weighted.mean(w=prob[i,],x=1:ncol(prob))))

                  if(plotType=="IRF"){
                    par(mar=c(3,3,2,7.5),mgp=c(1.75,.25,0))
                    plot(NULL, main=paste0("Item Response Function (", names(catObj@difficulty[item]), ")" ), xlab=expression(theta), ylab=expression(paste("Pr(Y=x|", theta, ")")),
                         lwd=2, xlim=c(xlim[1], xlim[2]), ylim=c(0, max(prob)),  las=1, tck=F,cex.axis=.90)
                    
                    
                    sapply(1:(ncol(prob)),function(i){lines(x,prob[,i],col=linecolors[i+1],lty=ltys[i], lwd=2)})
                    legend("left",  inset=c(1,1.2), xpd=TRUE, bty="n",legend=sapply(1:ncol(prob),function(i)paste("Response", i,sep=" ")), col=linecolors[2:(ncol(prob)+1)], lty=ltys[1:ncol(prob)] , cex=0.8)
                  }
                  if(plotType=="ICC"){
                    par(mar=c(3,3,2,1),mgp=c(1.75,.25,0))
                    plot(x,expectedValue,"l", main=paste0("Item Characteristic Curve (", names(catObj@difficulty[item]), ")" ),col=linecolors[1],
                         ylab="Expected Value of Response",xlab=expression(theta), lwd=2,
                         xlim=xlim, ylim=c(1,max(expectedValue)), tck=F,cex.axis=.90, las=1)
                    abline(h=1, lty=3, col="gray70")
                    abline(h=ncol(prob), lty=3, col="gray70")
                  }
                }
                
                
                
                if(model=="grm"){
                  
                  prob<-as.matrix(sapply(x,probability, catObj=catObj, item=item))
                  prob<-as.matrix(sapply(1:(nrow(prob)-1),function(i){return(abs(prob[i+1,]-prob[i,]))}))
                  expectedValue<-prob%*%(1:ncol(prob))
                  
                  if(plotType=="IRF"){
                    par(mar=c(3,3,2,7.5),mgp=c(1.75,.25,0))
                    plot(NULL, main=paste0("Item Response Function (", names(catObj@difficulty[item]), ")" ), xlab=expression(theta), ylab=expression(paste("Pr(Y=x|", theta, ")")),
                         lwd=2, xlim=c(xlim[1], xlim[2]), ylim=c(0, max(prob)),  las=1, tck=F,cex.axis=.90)
                    
                    
                    sapply(1:(ncol(prob)),function(i){lines(x,prob[,i],col=linecolors[i+1],lty=ltys[i], lwd=2)})
                    legend("left",  inset=c(1,1.2), xpd=TRUE, bty="n",legend=sapply(1:ncol(prob),function(i)paste("Response", i,sep=" ")), col=linecolors[2:(ncol(prob)+1)], lty=ltys[1:ncol(prob)] , cex=0.8)
                  }
                  if(plotType=="ICC"){
                    par(mar=c(3,3,2,1),mgp=c(1.75,.25,0))
                    plot(x,expectedValue,"l", main=paste0("Item Characteristic Curve (", names(catObj@difficulty[item]), ")" ),col=linecolors[1],
                         ylab="Expected Value of Response",xlab=expression(theta), lwd=2,
                         xlim=xlim, ylim=c(1,max(expectedValue)), tck=F,cex.axis=.90, las=1)
                    abline(h=1, lty=3, col="gray70")
                    abline(h=ncol(prob), lty=3, col="gray70")
                  }
                }
})
