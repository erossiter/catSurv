\name{nfc}
\alias{nfc}
\docType{data}
\title{
%%   ~~ Need For Cognition dataset from TAPS and AMT ~~
}
\description{
%%  ~~ Data of survey respondents' respond to 18 NFC questions, which is a reduced version of NFC. For each question, respondents could choose one of five response options: Agree strongly, agree somewhat, neither agree nor disagree, disagree somewhat, disagree strongly. Sample variable indicates where the dataset come from: 1 = The American Panel Survey, 2 = Amazon's Mechanical Turk (fall 2014), 3 = Amazon's Mechanical Turk (spring 2015) ~~
}
\usage{data("nfc")}
\format{
  A data frame with 4043 observations on the following 20 variables.
  \describe{
    \item{\code{X}}{a numeric vector}
    \item{\code{NFC1}}{a numeric vector}
    \item{\code{NFC4}}{a numeric vector}
    \item{\code{NFC10}}{a numeric vector}
    \item{\code{NFC12}}{a numeric vector}
    \item{\code{NFC15}}{a numeric vector}
    \item{\code{NFC16}}{a numeric vector}
    \item{\code{NFC19}}{a numeric vector}
    \item{\code{NFC21}}{a numeric vector}
    \item{\code{NFC22}}{a numeric vector}
    \item{\code{NFC23}}{a numeric vector}
    \item{\code{NFC24}}{a numeric vector}
    \item{\code{NFC29}}{a numeric vector}
    \item{\code{NFC31}}{a numeric vector}
    \item{\code{NFC32}}{a numeric vector}
    \item{\code{NFC33}}{a numeric vector}
    \item{\code{NFC39}}{a numeric vector}
    \item{\code{NFC40}}{a numeric vector}
    \item{\code{NFC43}}{a numeric vector}
    \item{\code{sample}}{a numeric vector}
  }
}
\details{
%%  ~~ TAPS data was from the December 2014 wave (n=1,506). First AMT data was collected in the fall of 2014 (n=1,204) and second sample was collected in the spring of 2015 (n=1,333).  ~~
}
\source{
%%  ~~ TAPS is conducted by Washington University in St.Louis and AMT sample is recruited via Amazon's Mechanical Turk, which both are online survey ~~
}
\references{
%%  ~~ Jacob Montgomery ~~
}
\examples{
data(nfc)
## maybe str(nfc) ; plot(nfc) ...
}
\keyword{datasets}
