library(stats)
library(devtools)
library(roxygen2)
library(Rcpp)
library(testthat)
library(ltm)
library(catR)
setwd("~/Dropbox/Spring2016/Rclass/CATsurv")
current.code <- as.package("catSurv")
load_all(current.code)
document(current.code)


### gpcmCat --------------------------------------------------------------------
data("nfc")
nfc <- nfc[1:100, -c(3,15)]

## getting item parameters from the ltm package
ltm_cat <- gpcm(nfc, constraint = "gpcm", control = list(GHk = 100))

## testing the gpcmCat function
## using that output to make my Cat object
test_cat <- gpcmCat(ltm_cat, quadraturePoints = 100)


#### probability and derivaties ------------------------------------------------
prob <- function(cat, theta, question){
  discrimination = cat@discrimination[question]
  categoryparams = c(0, cat@difficulty[[question]])

  f <- f_prime <- f_primeprime <- rep(NA,length(categoryparams))
  for(ans in 1:length(categoryparams)){  
    f[ans] <- exp(sum(discrimination * (theta - categoryparams[1:ans])))
    f_prime[ans] <- f[ans] * (discrimination * ans)
    f_primeprime[ans] <- f[ans] * (discrimination * ans)^2
  }

  g <- sum(f)
  g_prime <- sum(f * (discrimination * 1:length(categoryparams)))
  g_primeprime <- sum(f * (discrimination * 1:length(categoryparams))^2)
  
  prob <- f/g
  
  ## quotient rule 
  ## (g * f' - g' * f ) / g^2
  first_deriv <- (g * f_prime - f * g_prime) / g^2
  
  ## quotient rule again
  ## (b * a' - b' * a ) / b^2
  a <- (g * f_prime - f * g_prime)
  a_prime <- f_primeprime * g - g_primeprime * f
  b <- g^2
  b_prime <- (2 * g) * g_prime
  
  second_deriv <- (b * a_prime - a * b_prime) / b^2
  return(list(p = prob, d1p = first_deriv, d2p = second_deriv))
}


## probability -- testing my results against catR
my_prob <- matrix(NA, ncol = 5, nrow=10)
for(q in 1:10){
  my_prob[q, ] <- round(prob(test_cat, 1, q)$p, 4)
}

it_poly <- matrix(c(coef(ltm_cat)[,5], coef(ltm_cat)[,1:4]),
                    ncol= 5, byrow = F)
catR_prob <- round(Pi(th=1, it=it_poly, model="GPCM")$Pi, 4)[1:10, ]


## first derivative -- testing my results against catR
my_d1 <- matrix(NA, ncol = 5, nrow=10)
for(q in 1:10){
  my_d1[q, ] <- round(prob(test_cat, 1, q)$d1p, 4)
}

catR_d1 <- round(Pi(th=1, it=it_poly, model="GPCM")$dPi, 4)[1:10, ]


## second derivative -- testing my results against catR
my_d2 <- matrix(NA, ncol = 5, nrow=10)
for(q in 1:10){
  my_d2[q, ] <- round(prob(test_cat, 1, q)$d2p, 4)
}

catR_d2 <- round(Pi(th=1, it=it_poly, model="GPCM")$d2Pi, 4)[1:10, ]



#### likelihood and derivatives ------------------------------------------------
## no direct test of likelihood, but I will use it in
## the EAP and MAP estimation, so that will test it
like <- function(cat, theta){
  answered_qs <- which(!is.na(cat@answers))
  L <- d1l <- d2l <- length(answered_qs)
  for(i in 1:length(answered_qs)){
    q <- answered_qs[i]
    answer <- cat@answers[q]
    probs <- prob(cat, theta, question = q)
    p <- probs$p[answer]
    p_prime <- probs$d1p[answer]
    p_primeprime <- probs$d2p[answer]

    L[i] <- log(p)
    d1l[i] <- p_prime / p
    d2l[i] <- (p_prime^2 / p^2) - (p_primeprime / p)
  }
  L <- exp(sum(L))
  d1l <- sum(d1l)
  d2l <- (-sum(d2l))
  ## returns likelihood, 1st deriv of loglikelihood,
  ## and 2nd derive of loglikelihood
  return(list(L = L, d1l = d1l, d2l = d2l))
}
test_cat@answers <- rep(NA, 18)
test_cat@answers[1:4] <- as.numeric(nfc[1,1:4])
like(test_cat, 1)

sum(OIi(th = 1, it = it_poly, x = c(1,1,1,1), model = "GPCM", D = 1))



#### estimateTheta (EAP) -------------------------------------------------------
eap <- function(cat){
  numerator <- function(theta){
    prior_values <- prior(theta, cat@priorName, cat@priorParams)
    return(theta * like(cat, theta)$L * prior_values)
  }
  denominator <- function(theta){
    prior_values <- prior(theta, cat@priorName, cat@priorParams)
    return(like(cat, theta)$L * prior_values)
  }
  results <- ((integrate(Vectorize(numerator), -5, 5)$value)/
    (integrate(Vectorize(denominator), -5, 5)$value))

  return(results)
}

eap_ests <- rep(NA, 10)
for(i in 1:10){
  test_cat@answers[c(1, 5:10, 12)] <- as.numeric(nfc[i,c(1, 5:10, 12)])
  eap_ests[i] <- eap(test_cat)
}

## testing my results against ltm
ltm_eap <- factor.scores.gpcm(ltm_cat, method = "EAP",
                              resp.patterns = nfc[1:10,])$score.dat[,"z1"]

## testing my results against catR
catR_eap <- rep(NA, 10)
for(i in 1:10){
  catR_eap[i] <- eapEst(it_poly, x=as.numeric(nfc[i,])-1, model="GPCM")
}

round(cbind(eap_ests, catR_eap, ltm_eap), 4)


#### estimateTheta (MAP) -------------------------------------------------------
map <- function(cat){
  theta_old <- 1
  iter <- 0
  diff <- 1
  while(diff > .00001 | iter < 50){
    iter <- iter + 1
    prior_shift <- (theta_old - cat@priorParams[1]) / cat@priorParams[2]^2
    theta_new <- theta_old - ((like(cat, theta_old)$d1l - prior_shift)
                              / (like(cat, theta_old)$d2l - prior_shift))
    diff <- abs(theta_new - theta_old)
    theta_old <- theta_new
  }
  return(theta_new)
}

map_ests <- rep(NA, 10)
for(i in 1:10){
  test_cat@answers[1:10] <- as.numeric(nfc[i,1:10])
  map_ests[i] <- map(test_cat)
}

ltm_map <- factor.scores.gpcm(ltm_cat, method = "EB",
                              resp.patterns = nfc[1:10,])$score.dat[,"z1"]

cbind(map_ests, ltm_map)


#### estimateSE (EAP) ----------------------------------------------------------
eap_se <- function(cat){
  theta_hat <- eap(cat)
  numerator <- function(theta){
    prior_values <- prior(theta, cat@priorName, cat@priorParams)
    return((theta - theta_hat)^2 * like(cat, theta)$L * prior_values)
  }
  denominator <- function(theta){
    prior_values <- prior(theta, cat@priorName, cat@priorParams)
    return(like(cat, theta)$L * prior_values)
  }
  results <- ((integrate(Vectorize(numerator), -5, 5)$value)/
    (integrate(Vectorize(denominator), -5, 5)$value))

  return(sqrt(results))
}

eapSE_ests <- rep(NA, 10)
for(i in 1:10){
  test_cat@answers[c(1:15)] <- as.numeric(nfc[i,c(1:15)])
  eapSE_ests[i] <- eap_se(test_cat)
}

## testing my results against ltm
ltm_eapSE <- factor.scores.gpcm(ltm_cat, method = "EAP",
                              resp.patterns = nfc[1:10,])$score.dat[,"se.z1"]

cbind(eapSE_ests, ltm_eapSE)

#### ObsInf --------------------------------------------------------------------
oi <- function(cat, theta){
  -1 * like(cat, theta)$d2l
}

# #### FisherInf --------------------------------------------------------------------
# fi <- function(cat, theta){
#   sum
# }

#### estimateSE (MAP) ----------------------------------------------------------
map_se <- function(cat){
  theta_hat <- map(cat)
  var <- 1 / (oi(cat, theta_hat) + 1 / cat@priorParams[2])
  return(sqrt(var))
}

mapSE_ests <- rep(NA, 10)
for(i in 1:10){
  test_cat@answers[1:18] <- as.numeric(nfc[i,1:18])
  mapSE_ests[i] <- map_se(test_cat)
}

ltm_mapSE <- factor.scores.gpcm(ltm_cat, method = "EB",
                              resp.patterns = nfc[1:10,])$score.dat[,"se.z1"]

cbind(map_ests, ltm_map[1:10])

#### selectItem (MEI) ----------------------------------------------------------
expected_oi <- function(cat, item){
  theta_hat <- map(cat)
  item_probs <- prob(cat, theta_hat, item)$p
  item_ois <- rep(NA, length(item_probs))
  for(i in 1:length(item_probs)){
    cat@answers[item] <- i
    theta_hat <- map(cat)
    item_ois[i] <- oi(cat, theta_hat)
    cat@answers[item] <- NA
    }
  item_EI <- sum(item_probs * item_ois)
  return(item_EI)
}
test_cat@answers[1:16] <- rep(NA, 16)
test_cat@answers[5:16] <- as.numeric(nfc[100,5:16])
expected_oi(test_cat, 1)

# epv <- function(cat){
#   unanswered_qs <- which(is.na(cat@answers))
#   epv_ests <- rep(NA, length(unanswered_qs))
#   for(i in 1:length(unanswered_qs)){
#     epv_ests[i] <- epv(cat, unanswered_qs[i])
#   }
#   names(epv_ests) <- as.character(unanswered_qs)
#   return(max(epv_ests))
# }


#### selectItem (EPV) ----------------------------------------------------------
epv <- function(cat, item){
  item_probs <- prob(cat, eap(cat), item)$p
  item_vars <- rep(NA, length(item_probs))
  for(i in 1:length(item_probs)){
    cat@answers[item] <- i
    item_vars[i] <- eap_se(cat)^2
    cat@answers[item] <- NA
    }
  item_EPV <- sum(item_probs * item_vars)
  return(item_EPV)
}

pickItem <- function(cat){
  unanswered_qs <- which(is.na(cat@answers))
  epv_ests <- rep(NA, length(unanswered_qs))
  for(i in 1:length(unanswered_qs)){
    epv_ests[i] <- epv(cat, unanswered_qs[i])
  }
  names(epv_ests) <- as.character(unanswered_qs)
  return(epv_ests)
}

test_cat@answers[1:16] <- rep(NA, 16)
test_cat@answers[1:10] <- as.numeric(nfc[3,1:10])
pickItem(test_cat)

#### Court example ----------------------------------------------------------
# library(foreign)
# setwd("~/Dropbox/Fall2016/Measurement/Paper/court_example")
# court <- read.spss("spss_data.por", to.data.frame = TRUE)
# cols <- c("PROTECT", "REPRESNT", "HONEST", "ATTENTN", "GOINGON", "UNDRSTD", "ORDERS", "ACCURAT", "IGNORE", "SUES", "POLITICL", "INFLUNCE", "ATTORNEY", "MYSELF", "AFFORD", "TIMELY", "MONITOR", "HELPFUL" )
# court2 <- court[,cols]
# 
# 
# df <- data.frame(matrix(ncol = 18, nrow = nrow(court2)))
# for(i in 1:18){
#   new_levels <- factor(court2[,i],  levels = c("Strongly disagree",
#                                                "Somewhat disagree",
#                                                "Somewhat agree",
#                                                "Strongly agree",
#                                                "Dont Know", ## 5
#                                                "No Answer/Refuse")) ## 6
#   df[,i] <- as.numeric(new_levels)
#   df[which(df[,i] == 5), i] <- NA
#   df[which(df[,i] == 6), i] <- NA
# }
# colnames(df) <- colnames(court2)
# 
# 
# #### Equality and Fairness of Courts (pg 30)
# ## REPRESNT “Most juries are not representative of the community.”
# ## HONEST "Judges are generally honest and fair in deciding cases.”
# ## ATTENTN “Judges do not give adequate attention and time to each individual case.”
# ## PROTECT “Courts protect defendants’ constitutional rights.”
# ## IGNORE “I would prefer that a judge ignore the law to ensure that a defendant is convicted.”
# ## UNDRSTD “Court rulings are understood by the people involved in the cases.”
# ## ORDERS “Courts do not make sure their orders are enforced.”
# ## SUES “When a person sues a corporation, the courts generally favor the corporation over the person.”
# 
# #### Perceptions of Equal Treatment (pg 38)
# 
# #these_cols <- which(colnames(df) %in% c("REPRESNT", "HONEST", "ATTENTN", "PROECT",
#             #"IGNORE", "UNDRSTD", "ORDERS", 'SUES'))
# #eq <- df[,these_cols]
# #eq <- eq[complete.cases(eq),]
# 
# 
# ## getting item parameters from the ltm package
# set.seed(1)
# samp <- sample(1:nrow(df), nrow(df)-100,  replace = FALSE)
# court_ltm <- gpcm(df[samp, ], constraint = "gpcm", control = list(GHk = 100))
# 
# ## testing the gpcmCat function
# ## using that output to make my Cat object
# court_cat <- gpcmCat(court_ltm)#, quadraturePoints = 100)
# 
# 
# ## 100 people not used in calibration
# ppl <- df[-samp, ]
# 
# court_cat@answers[1] <- 3
# max(pickItem(court_cat))
# court_cat@answers
# 
# ## true pos on trait is when they answer all of them and factor scores that
# ## grm dynamic 5
# ## gpcm dynamic 5
# ## random 5



