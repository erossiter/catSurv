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
setGeneric("iic", function(object, theta_range = seq(-5, 5, .1), question, ...){standardGeneric("iic")})

#' @export
setMethod(f="iic", signature="Cat",
          definition=function(object, theta_range, question, ...){
            fishers <- sapply(theta_range, function(i){
              return(fisherInf(object, i, question))
            })
            
            plot.new()
            return_plot <- recordPlot()
            plot.window(c(min(theta_range), max(theta_range)), c(0,1.1*max(fishers)))
            title(main = paste0("Item Information Curves for Question ", question),
                  xlab = "Theta",
                  ylab = "Fisher Information")
            axis(1, at = seq(min(theta_range), max(theta_range),1), labels = seq(min(theta_range), max(theta_range),1))
            if(max(fishers)>=0.5){
              axis(2, at = seq(0, 1.1*max(fishers), .2), labels = seq(0,1.1*max(fishers), .2), las = 1)
            }
             else axis(2, at = seq(0,max(fishers), .05), labels = seq(0,max(fishers), .05), las = 1)
#             
#              if(max(fishers)>=0.5){
#               axis(2, at = seq(0,max(fishers), signif((max(fishers)/5),2)), labels = seq(0,max(fishers), signif((max(fishers)/5),1)), las = 1)
#             }
#             else axis(2, at = seq(0,max(fishers), signif((max(fishers)/5),2)), labels = seq(0,max(fishers), signif((max(fishers)/5),2)), las = 1)
#             #legend("topright", c(paste0("Item ", 1:(nrow(x)+1))), lty = 1:(nrow(x)+1), bty = "n")
            
            lines(x = theta_range, y = fishers, lty = 1)
            #lines(x = theta_range, y = (1 - x[nrow(x), ]), lty = nrow(x)+1)
            #for(i in 2:(nrow(x))){
            #  lines(x = theta_range, y = (x[i, ] - x[(i-1), ]), lty = i)
            #}
            iicInfo<-cbind(theta_range, fishers)
            return(list(object, question, iicInfo))
          })