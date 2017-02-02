## first derivative of probability ---------------------------------------------
prob_derivs <- function(cat, theta, question){
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

pout <- prob_derivs(test_cat, theta = 1, question = 1)

likeli_derivs <- function(cat, theta, question, ans){
  p <- prob(cat, theta, question)[ans]
  pderivs <- prob_derivs(cat, theta, question, ans)
  
  first_deriv <- 
  
  first_term <- pderivs$pd^2 / p^2
  second_term <- pderivs$d2d / p
  second_deriv <- first_term - second_term
  return()
}

likeli_derivs(test_cat, 1, 1, 3)

Ii(th = 1, it = it, model = "GPCM", D = 1)$Ii



their_prob<-function (th, it, model = NULL, D = 1) 
{
    it <- rbind(it)
    nc <- ncol(it)
    prov <- prov1 <- prov2 <- prov3 <- matrix(NA, nrow(it), nc)
    
    for (i in 1:nrow(it)) {
      dj <- v <- 0
      if (model == "GPCM") {
        for (t in 1:(ncol(it) - 1)){
          ## dj[t] is previous one
          ## it[i,1] is difficulty * d * theta - discrimination above it
          dj <- c(dj, dj[t] + it[i, 1] * D * (th - it[i, t + 1]))
          ## 0, disc
          v <- c(v,it[i, 1]*t)
          }
      }
      dj <- dj[!is.na(dj)]
      v <- v[!is.na(dj)]
      Gammaj <- exp(dj)
      dGammaj <- Gammaj * v
      d2Gammaj <- Gammaj * v^2
      d3Gammaj <- Gammaj * v^3
      Sg <- sum(Gammaj)
      Sdg <- sum(dGammaj)
      Sd2g <- sum(d2Gammaj)
      Sd3g <- sum(d3Gammaj)
      n <- length(Gammaj)
      prov[i, 1:n] <- Gammaj/Sg ## probability
      prov1[i, 1:n] <- dGammaj/Sg - Gammaj * Sdg/Sg^2
      prov2[i, 1:n] <- (d2Gammaj/Sg - 2 * dGammaj * Sdg/Sg^2 - Gammaj
                        * Sd2g/Sg^2 + 2 * Gammaj * Sdg^2/Sg^3)
      prov3[i, 1:n] <- (d3Gammaj/Sg - (Gammaj * Sd3g + 3 * dGammaj * Sd2g
                                       + 3 * d2Gammaj * Sdg)/Sg^2 + 
                          (6 * Gammaj * Sdg * Sd2g + 6 * dGammaj * Sdg^2)/Sg^3
                        - 6 * Gammaj * Sdg^3/Sg^4)
      }
    return(list(Pi = prov, dPi = prov1, d2Pi = prov2, d3Pi = prov3))
}


it <- matrix(c(coef(ltm_cat)[,5], coef(ltm_cat)[,1:4]),
                    ncol= 5, byrow = F)
theta <- 1
model <- "GPCM"
D <- 1
prob_out <- their_prob(theta, it, model, D)


oi <- (prob_out$dPi[1,]^2 / prob_out$Pi[1,]^2) - (prob_out$d2Pi[1,] / prob_out$Pi[1,])
second_deriv <- (-oi)

OIi(2, it = it, x = c(1,1,1), model = "GPCM", D = 1)

