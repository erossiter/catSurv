#' Plotting Item Characteristic Curves (ICC)
#'
#' This function plots the item characteristic curves for an object of class \code{Cat}.
#'
#' @param object A \code{Cat} object
#' @param theta_range A numerical vector of thetas at which the probabilities are to be calculated 
#' @param question A single numeric indicating the question/item for which the curves are to be drawn.
#'
#'  @return A list with components,
#' \itemize{
#' \item \code{iccPlot} a named vector of difficulty parameters for use with dichotomous questions/items.  Each element's name tells the question/item to which it applies.
#' \item \code{x} A matrix of probabilities calculated at each theta from which the curves were drawn.
#'
#' @rdname icc
#' @export
setGeneric("icc", function(object, theta_range, question, ...){standardGeneric("icc")})

#' @export
setMethod(f="icc", signature="Cat",
          definition=function(object, theta_range = seq(-3, 3, .1), question, ...){
            if(!is.null(object)) if(class(object)!="Cat") stop("object is not class Cat")
            
            prob_given_theta <- function(i){
              probability(object, theta_range[i], question)$all.probabilities$probabilities
            }
            x <- sapply(1:length(theta_range), prob_given_theta)
            
            plot.new()
            return_plot <- recordPlot()
            plot.window(c(min(theta_range), max(theta_range)), c(0,1))
            title(main = paste0("Item Characteristic Curves for Question ", question),
                  xlab = "Theta",
                  ylab = "Probability")
            axis(1, at = seq(-3,3,1), labels = seq(-3,3,1))
            axis(2, at = seq(0,1,.25), labels = seq(0,1,.25), las = 1)
            legend("topright", c(paste0("Item ", 1:(nrow(x)+1))), lty = 1:(nrow(x)+1), bty = "n")
            
            lines(x = theta_range, y = (x[1, ] - 0), lty = 1)
            lines(x = theta_range, y = (1 - x[nrow(x), ]), lty = nrow(x)+1)
            for(i in 2:(nrow(x))){
              lines(x = theta_range, y = (x[i, ] - x[(i-1), ]), lty = i)
            }
            return(list(plot = return_plot, x = x))
          })
          
