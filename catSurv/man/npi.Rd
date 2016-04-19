\name{npi}
\alias{npi}
\docType{data}
\title{NPI}
\description{
Data from the test with the 40 paired items on one page. Respondents had to choose the one that fit them the best. Missingness is optional. Respondents' demographics were entered on the next page. In addition, they were also asked to affirm that their answers were acurate and suitable for research, those that did not (9\%) are not included in this dataset.
}
\usage{data("npi")}
\format{
  A data frame with 11243 observations on the following 44 variables.
  \describe{
    \item{\code{score}}{a numeric vector}
    \item{\code{Q1}}{1=I have a natural talent for influencing people. 2=I am not good at influencing people}
    \item{\code{Q2}}{1=Modesty doesnt become me. 2=I am essentially a modest person}
    \item{\code{Q3}}{1=I would do almost anything on a dare. 2=I tend to be a fairly cautious person}
    \item{\code{Q4}}{1=When people compliment me I sometimes get embarrassed. 2=I know that I am good because everybody keeps telling me so.}
    \item{\code{Q5}}{1=The thought of ruling the world frightens the hell out of me. 2=If I ruled the world it would be a better place}
    \item{\code{Q6}}{1=I can usually talk my way out of anything. 2=I try to accept the consequences of my behavior}
    \item{\code{Q7}}{1=I prefer to blend in with the crowd. 2=I like to be the center of attention}
    \item{\code{Q8}}{1=I will be a success. 2=I am not too concerned about success}
    \item{\code{Q9}}{1=I am no better or worse than most people. 2=I think I am a special person}
    \item{\code{Q10}}{1=I am not sure if I would make a good leader. 2=I see myself as a good leader}
    \item{\code{Q11}}{1=I am assertive. 2=I wish I were more assertive}
    \item{\code{Q12}}{1=I like to have authority over other people. 2=I don't mind following orders}
    \item{\code{Q13}}{1=I find it easy to manipulate people. 2=I don't like it when I find myself manipulating people}
    \item{\code{Q14}}{1=I insist upon getting the respect that is due me. 2=I usually get the respect that I deserve}
    \item{\code{Q15}}{1=I don't particularly like to show off my body. 2=I like to show off my body}
    \item{\code{Q16}}{1=I can read people like a book. 2=People are sometimes hard to understand}
    \item{\code{Q17}}{1=If I feel competent I am willing to take responsibility for making decisions. 2=I like to take responsibility for making decisions}
    \item{\code{Q18}}{1=I just want to be reasonably happy. 2=I want to amount to something in the eyes of the world}
    \item{\code{Q19}}{1=My body is nothing special. 2=I like to look at my body}
    \item{\code{Q20}}{1=I try not to be a show off. 2=I will usually show off if I get the chance}
    \item{\code{Q21}}{1=I always know what I am doing. 2=Sometimes I am not sure of what I am doing}
    \item{\code{Q22}}{1=I sometimes depend on people to get things done. 2=I rarely depend on anyone else to get things done}
    \item{\code{Q23}}{1=Sometimes I tell good stories. 2=Everybody likes to hear my stories}
    \item{\code{Q24}}{1=I expect a great deal from other people. 2=I like to do things for other people}
    \item{\code{Q25}}{1=I will never be satisfied until I get all that I deserve. 2=I take my satisfactions as they come}
    \item{\code{Q26}}{1=Compliments embarrass me. 2=I like to be complimented}
    \item{\code{Q27}}{1=I have a strong will to power. 2=Power for its own sake doesn't interest me}
    \item{\code{Q28}}{1=I don't care about new fads and fashions. 2=I like to start new fads and fashions}
    \item{\code{Q29}}{1=I like to look at myself in the mirror. 2=I am not particularly interested in looking at myslef in the mirror}
    \item{\code{Q30}}{1=I really like to be the center of attention. 2=It makes me uncomfortable to be the center of attention}
    \item{\code{Q31}}{1=I can live my life in any way I want to. 2=People can't always live their lives in terms of what they want}
    \item{\code{Q32}}{1=Being an authority doesn't mean that much to me. 2=People alwyas seem to recognize my authority}
    \item{\code{Q33}}{1=I would prefer to be a leader. 2=It makes little difference to me whether I am a leader or not.}
    \item{\code{Q34}}{1=I am going to be a great person. 2=I hope I am going to be successful}
    \item{\code{Q35}}{1=People sometimes believe what I tell them. 2=I can make anybody believe anything I want them to}
    \item{\code{Q36}}{1=I am a born leader. 2=Leadership is a quality that takes a long time to develop}
    \item{\code{Q37}}{1=I wish somebody would someday write my biography. 2=I don't like people to pry into my life for any reason}
    \item{\code{Q38}}{1=I get upset when people don't notice how I look when I go out in public. 2=I don't mind blending into the crowd when I go out in public}
    \item{\code{Q39}}{1=I am more capable than other people. 2=There is a lot that I can learn from other people}
    \item{\code{Q40}}{1=I am much like everybody else. 2=I am an extraordinary person}
    \item{\code{elapse}}{a numeric vector indicates between time submitted and time loaded of the questions}
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
