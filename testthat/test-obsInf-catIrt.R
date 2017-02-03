library(catSurv)
library(ltm)
library(catIrt)
context("obsInf")

test_that("obsInf calculates correctly", {
  
  obsInf_test_catIrt <- function(data, poly){
    
    if(poly == FALSE){
      cat <- ltmCat(binary_data)
      ltm_cat <- ltm(binary_data ~ z1, control = list(GHk = 100))
      cat_coefs <- coef(ltm_cat)

      # obsInf for one person, for all items
      cat@answers <- unlist(data[100,])
      our_obsInf <- c()
      for(i in 1:ncol(data)){
        our_obsInf <- append(our_obsInf, obsInf(cat,1,i))
      }

      params <- matrix(c(cat@discrimination,
                      cat_coefs[,1],
                      cat@guessing) , ncol = 3)
      
      catIrt_obsInf <- FI(params = params, theta = 1, type = "observed", resp = cat@answers)$item
      
      return(abs(our_obsInf - catIrt_obsInf))
    }
    
    if(poly == TRUE){
      cat <- grmCat(poly_data)
      cat_coefs <- coef(grm(poly_data))
      
      #for(j in 1:nrow(poly_data)){
      
      our_obsInf <- c()
      cat@answers <- unlist(poly_data[1,])

      for(i in 1:ncol(poly_data)){
        our_obsInf <- append(our_obsInf, obsInf(cat,1,i))
      }
      
      params <- matrix(c(cat@discrimination,
                 cat_coefs[,1],
                 cat_coefs[,2],
                 cat_coefs[,3],
                 cat_coefs[,4]),
                 ncol = 5)
      
      catIrt_obsInf <- FI.grm(params = params, theta = 1, type = "observed", resp = cat@answers)$item
      
      which(abs(our_obsInf - catIrt_obsInf) > .1)
      
      #Irt[j] <- mean(abs(our_obsInf - catIrt_obsInf))
      
      #which(our_obsInf - catIrt_obsInf == max(our_obsInf - catIrt_obsInf))
      
      #print(mean(abs(our_obsInf - catIrt_obsInf)))   
      
      #}
      
      return(abs(our_obsInf - their_obsInf))
    }
    
  }

  data("npi")
  data("nfc")
  binary_data <- npi[1:100, ]
  poly_data <- nfc[1:100, ]
  
  expect_equal(obsInf_test_catIrt(binary_data, F),
               rep(0,ncol(binary_data)),
               tolerance = .9)
  
  # expect_equal(obsInf_test_catIrt(poly_data, T),
  #              rep(0,ncol(poly_data)),
  #              tolerance = .9)
})


#######################################################################


obsInf_R <- function(cat, theta, question){
  answer_k <- cat@answers[question]
  # let's say they answer 1:
  answer_k <- answer_k + 1
  # 0, .07, .45, .69, .88, .1
	# 1    2    3    4    5   6
  probs <- probability(cat, theta, question)$all.probabilities$probabilities
  probs <- c(0, probs, 1)

  P_star1 = probs[answer_k]
	P_star2 = probs[answer_k - 1]
	P = P_star1 - P_star2

	Q_star1 = 1 - P_star1
	Q_star2 = 1 - P_star2

	w2 = P_star2 * Q_star2
	w1 = P_star1 * Q_star1
	w = w1 - w2

	first_term = (-w2 * (Q_star2 - P_star2) + w1 * (Q_star1 - P_star1)) / P
	second_term = w^2 / P^2

	return (-1 * cat@discrimination[question]^2 * (first_term - second_term))
}

## making cat object
cat_2<-grm(poly_data, IRT.param=TRUE, control = list(GHk = 100))
cat <- grmCat(poly_data)
cat@discrimination
mytest<-rep(NA, 18)
for(i in 1:18){
  mytest[i]<-cat_2$coefficients[[i]]["beta"]
}
mytest
cat_2$coefficients[[18]]["beta"]
cbind(cat@discrimination, mytest    )
    

cat_2$coef

cat@discrimination

coef(cat_2)
cat@discrimination

cat_coefs <- coef(cat_2)
cat@answers <- unlist(poly_data[1,])
cat_coefs
cat@discrimination
question <- 2

## R test function
obsInf_R(cat, 1, question)
## c++
obsInf(cat, 1, question)
## catR
it <- matrix(c(as.numeric(cat@discrimination),
        cat_coefs[,1],
        cat_coefs[,2],
        cat_coefs[,3],
        cat_coefs[,4]),
        ncol = 5)
their_obsInf <- OIi(th = 1, it = it, x = cat@answers-1, model = "GRM")
their_obsInf[question]


using_derivs <- function(cat, theta, question){
  b <- cat@discrimination[question]
  answer_k <- cat@answers[question]
  answer_k <- answer_k + 1
  probs <- probability(cat, theta, question)$all.probabilities$probabilities
  probs <- c(0, probs, 1)
  P_star1 = probs[answer_k]
	P_star2 = probs[answer_k - 1]
	P = P_star1 - P_star2

	Q_star1 = 1 - P_star1
	Q_star2 = 1 - P_star2

	w2 = P_star2 * Q_star2
	w1 = P_star1 * Q_star1

	first_term <- (b * (w1-w2)) / P^2
	second_term <- (b^2 * ( (w1 - 2*P_star1*Q_star1) - (w2 - 2 * P_star2^2 * Q_star2^2 ) ) ) / P

	return (first_term^2 - second_term)
}

using_derivs(cat, 1, question)

our_probs <- c(0, probability(cat, 1, 1)$all.probabilities$probabilities, 1)
our_probs
diff(our_probs)

cat


?diff

Pi(th = 1, it = it, model = "GRM")$Pi[1,]
diff(our_probs)
#debug(Pi)

D=1
aj=it[1,1]
th=1
bj=it[1,2:5]

aj

ej <- exp(D * aj * (th - bj))
 Pjs <- ej/(1 + ej)
Pjs <- c(1, Pjs, 0)
Pjs
rev(diff(rev(Pjs)))
Pi(th = 1, it = it, model = "GRM")$Pi[1,]



probability(cat, 1, 1)
myej<-exp(cat@difficulty[[1]]-cat@discrimination[1]*theta)
myPjs<-myej/(1+myej)
myPjs<-c(0, myPjs, 1)

diff(myPjs)

cbind(cat@discrimination, it)
cat@difficulty
it
