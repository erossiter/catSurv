.onAttach <- function(libname = find.package("catSurv"), pkgname = "catSurv"){
  msg <- "## \n## Support provided by the U.S. National Science Foundation \n## (Grant SES-1558907) \n##"
  packageStartupMessage(msg)
}