\name{npi}
\alias{npi}
\docType{data}
\title{
%%   ~~ NPI ~~
}
\description{
%%  ~~ Data from the test with the 40 paired items on one page. Respondents had to choose the one that fit them the best. Respondents' demographics were entered on the next page. In addition, they were also asked to affirm that their answers were acurate and suitable for research, those that did not (9%) are not included in this dataset.~~
}
\usage{data("npi")}
\format{
  A data frame with 11243 observations on the following 44 variables.
  \describe{
    \item{\code{score}}{a numeric vector}
    \item{\code{Q1}}{a numeric vector}
    \item{\code{Q2}}{a numeric vector}
    \item{\code{Q3}}{a numeric vector}
    \item{\code{Q4}}{a numeric vector}
    \item{\code{Q5}}{a numeric vector}
    \item{\code{Q6}}{a numeric vector}
    \item{\code{Q7}}{a numeric vector}
    \item{\code{Q8}}{a numeric vector}
    \item{\code{Q9}}{a numeric vector}
    \item{\code{Q10}}{a numeric vector}
    \item{\code{Q11}}{a numeric vector}
    \item{\code{Q12}}{a numeric vector}
    \item{\code{Q13}}{a numeric vector}
    \item{\code{Q14}}{a numeric vector}
    \item{\code{Q15}}{a numeric vector}
    \item{\code{Q16}}{a numeric vector}
    \item{\code{Q17}}{a numeric vector}
    \item{\code{Q18}}{a numeric vector}
    \item{\code{Q19}}{a numeric vector}
    \item{\code{Q20}}{a numeric vector}
    \item{\code{Q21}}{a numeric vector}
    \item{\code{Q22}}{a numeric vector}
    \item{\code{Q23}}{a numeric vector}
    \item{\code{Q24}}{a numeric vector}
    \item{\code{Q25}}{a numeric vector}
    \item{\code{Q26}}{a numeric vector}
    \item{\code{Q27}}{a numeric vector}
    \item{\code{Q28}}{a numeric vector}
    \item{\code{Q29}}{a numeric vector}
    \item{\code{Q30}}{a numeric vector}
    \item{\code{Q31}}{a numeric vector}
    \item{\code{Q32}}{a numeric vector}
    \item{\code{Q33}}{a numeric vector}
    \item{\code{Q34}}{a numeric vector}
    \item{\code{Q35}}{a numeric vector}
    \item{\code{Q36}}{a numeric vector}
    \item{\code{Q37}}{a numeric vector}
    \item{\code{Q38}}{a numeric vector}
    \item{\code{Q39}}{a numeric vector}
    \item{\code{Q40}}{a numeric vector}
    \item{\code{elapse}}{a numeric vector}
    \item{\code{gender}}{a numeric vector}
    \item{\code{age}}{a numeric vector}
  }
}
\details{
%%  ~~ elapse was calculated with following equation: (time submitted) - (time loaded) of the questions page in seconds. Response optinos for gender question is following: 1=male, 3=female, 3=other, 0=none). Age was entered as a free response but ages below 14 were omiited from the dataset. ~~
}
\source{
%%  ~~ ~~
}
\references{
%%  ~~ Jacob Montgomery ~~
}
\examples{
data(npi)
## maybe str(npi) ; plot(npi) ...
}
\keyword{datasets}
