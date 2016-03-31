#' CATPack
#'
#' @name CATPack
#' @docType package
#' @useDynLib CATPack
#' @importFrom Rcpp sourceCpp

.onUnload <- function (libpath) {
  library.dynam.unload("CATPack", libpath)
}