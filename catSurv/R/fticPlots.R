#' Plotting FisherTestInfo Curves (FTIC)
#'
#' This function plots the Fisher test information curves for an object of class \code{Cat}.
#'
#' @param object A \code{Cat} object
#' @param theta_range A numerical vector of thetas at which the probabilities are to be calculated 
#'
#'  @return A list with components,
#' \itemize{
#' \item The \code{Cat} object from which the fisherInf values were calculated
#' \item \code{fticInfo} A matrix of theta values and their corresponding fisherTestInfo values
#'}
#' @rdname ftic
#' @export
setGeneric("ftic", function(object, theta_range = seq(-3, 3, .1), ...){standardGeneric("ftic")})

#' @export
setMethod(f="ftic", signature="Cat",
          definition=function(object, theta_range, ...){
            
            if(is.null(object)) stop("object is null!")
            if(class(object)!="Cat") stop("object is not class Cat")
            
            ## exract indices for question items that thave been answered
            answered<-which(!is.na(object@answers),arr.ind=T)
            
            ## function for summing fisherInf values over all answered questions
            fisherSum<-function(obj="Cat",items="numeric", theta="numeric"){
              sum = 0
              for (i in items){
                sum = sum+fisherInf(obj,theta,i)
              }
              return(sum)
            }
            
            fishers <- sapply(theta_range, function(t){
              return(fisherSum(object, answered,t))
            })
            
            plot.new()
            return_plot <- recordPlot()
            if(max(fishers)>100) {
              maxFish=100 
              }else maxFish=max(fishers)
            
            plot.window(c(min(theta_range), max(theta_range)), c(0, 1.1*maxFish))
            title(main = paste0("Fisher Test Information Curve"),
                  xlab = "Theta",
                  ylab = "Fisher Test Information")
            axis(1, at = seq(min(theta_range), max(theta_range),1), labels = seq(min(theta_range), max(theta_range),1))
            ifelse(maxFish<0.5,
                   ifelse(maxFish<.05,
                          axis(2, at = c(0,signif(maxFish,1)), labels = c(0,signif(maxFish,1)),las=1), 
                          axis(2, at = seq(0,maxFish, .05), labels = seq(0,maxFish, .05), las = 1)),
                   ifelse(maxFish<2,
                          axis(2, at = seq(0, 1.1*max(fishers), .2), labels = seq(0,1.1*max(fishers), .2), las = 1),
                          axis(2, at = seq(0, 1.1*max(fishers), .5), labels = seq(0,1.1*max(fishers), .5), las = 1)))
            
          
            lines(x = theta_range, y = fishers, lty = 1)
            #legend("topright", paste("Fisher Test\nInformation\ngiven theta"),bty='n',cex=.6)
         
            fticInfo<-cbind(theta_range, fishers)
            return(list(object, fticInfo))
          })