#' Plotting Item Information Curves (IIC)
#'
#' This function plots the item information curves for an object of class \code{Cat}.
#'
#' @param object A \code{Cat} object
#' @param theta_range A numerical vector of thetas at which the probabilities are to be calculated 
#' @param question A single numeric indicating the question/item for which the curves are to be drawn.
#'
#'  @return A list with components,
#' \itemize{
#' \item The \code{Cat} object from which the fisherInf values were calculated
#' \item \code{question} A single numeric indicating the question/item from which the curves were drawn
#' \item \code{iicInfo} A matrix of theta values and their corresponding fisherInf values
#'}
#' @rdname iic
#' @export
setGeneric("iic", function(object, theta_range = seq(-3, 3, .1), question, ...){standardGeneric("iic")})

#' @export
setMethod(f="iic", signature="Cat",
          definition=function(object, theta_range, question, ...){
            
            if(is.null(object)) stop("object is null!")
            if(class(object)!="Cat") stop("object is not class Cat")
            
            fishers <- sapply(theta_range, function(i){
              return(fisherInf(object, i, question))
            })
            
            plot.new()
            return_plot <- recordPlot()
            if(max(fishers)>100) maxFish=100
            else maxFish=max(fishers)
            
            plot.window(c(min(theta_range), max(theta_range)), c(0, 1.1*maxFish))
            title(main = paste0("Item Information Curves for Question ", question),
                  xlab = "Theta",
                  ylab = "Fisher Information")
            axis(1, at = seq(min(theta_range), max(theta_range),1), labels = seq(min(theta_range), max(theta_range),1))
            ifelse(maxFish<0.5,
                   ifelse(maxFish<.05,
                      axis(2, at = c(0,signif(maxFish,1)), labels = c(0,signif(maxFish,1)),las=1), 
                      axis(2, at = seq(0,maxFish, .05), labels = seq(0,maxFish, .05), las = 1)),
                   ifelse(maxFish<2,
                      axis(2, at = seq(0, 1.1*max(fishers), .2), labels = seq(0,1.1*max(fishers), .2), las = 1),
                      axis(2, at = seq(0, 1.1*max(fishers), .5), labels = seq(0,1.1*max(fishers), .5), las = 1)))
                      
#             if (object@poly){
#               legend(x=1.6,y=.9, legend=c(1:(nrow(fishers)+1)), lty = 1:(nrow(fishers)+1), bty = "n", cex=.7)
#               legend(x=1.4, y=1, legend="Response\nCategory",bty = "n",cex=.7)
#               #lines(x = theta_range, y = (probs[1, ] - 0), lty = 1)
#               #lines(x = theta_range, y = (1 - probs[nrow(probs), ]), lty = nrow(probs)+1)
#               for(i in 1:(nrow(fishers))){
#                 lines(x = theta_range, y = (probs[i, ] - probs[(i-1), ]), lty = i)
#                 iccInfo<-cbind(theta_range,t(probs))
#               }
#             
#             }
            
            #else {
              lines(x = theta_range, y = fishers, lty = 1)
              legend("topright", paste("Fisher Information\ngained by asking\nquestion",question,"\ngiven theta"),bty='n',cex=.6)
              
           # }
            #lines(x = theta_range, y = (1 - x[nrow(x), ]), lty = nrow(x)+1)
            #for(i in 2:(nrow(x))){
            #  lines(x = theta_range, y = (x[i, ] - x[(i-1), ]), lty = i)
            #}
            iicInfo<-cbind(theta_range, fishers)
            return(list(object, question, iicInfo))
          })