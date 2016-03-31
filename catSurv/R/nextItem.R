#' Computerized Adaptive Testing Survey Next Item Function
#'
#' This function takes a respondent's previous answers to determine the next item from the list of available questions determined according to a user specified methods for latent trait estimation and item selection.
#'
#' @param cat An object of class \code{CATsurv}
#' @param ability.estimator The estimation procedure used to estimate the respondent's position on the latent scale.  The three options are "EAP" for expected a posterior (the default),  "ML" for maximum likelihood, and "MAP" for maximum a posterior.
#' @param item.selection The item selection procedure.  The five options are "EPV" for minimum expected posterior variance (the default),  "KL" for Kullback-Leibler, "MFI" for maximum Fisher's information, "MPWI" for maximum posterior wieghted information, "MWFI" for maximum weighted Fisher's information, "MEI" for maximum expected information.
#'
#' @return A data frame of available questions based on the use selected item selection criterion for the respondent and a row name for the next item to be asked
#'
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @seealso \code{\link{three.pl}},\code{\link{likelihood}}, \code{\link{prior.value}}, \code{\link{estimateTheta}}, \code{\link{estimateSE}}, \code{\link{expectedPV}},  \code{\link{storeAnswer}}, \code{\link{debugNextItem}}
#' @rdname nextItemR
#' @export
setGeneric("nextItemR", function(cat,ability.estimator="EAP", item.selection="EPV"){standardGeneric("nextItemR")})

#' @export
setMethod(f="nextItemR", signature=class.name, definition=function(cat,ability.estimator="EAP", item.selection="EPV") {

  available_questions = data.frame(questions=which(is.na(cat@answers)),a=NA)

  ## A workaround to allow for refusals.
  these<-which(cat@answers==-1)
  cat@answers[these]<-NA

  cat@Theta.est <- switch(ability.estimator,EAP=estimateTheta(cat),ML=estimateThetaML(cat),MAP=estimateThetaMAP(cat))

  cat@answers[these]<--1

  to.return <- switch(item.selection, EPV=nextItemEPV(cat), KL=nextItemKL(cat), MFI=nextItemMFI(cat),MPWI=nextItemMPWI(cat), MWFI=nextItemMWFI(cat), MEI=nextItemMEI(cat))

  return(to.return)
})
