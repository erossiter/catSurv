## eventually delete this file

## loading dataset from package
data("nfc")
data("npi")

ltm_fit <- ltm(npi[1:500, ] ~ z1, control = list(GHk = 100))
ltm_cat <- ltmCat(ltm_fit)

grm_fit <- grm(nfc[1:500, ], control = list(GHk = 100))
grm_cat <- grmCat(grm_fit)

## Need to suppress warning.  This object is just used for testing
## purposes, thus we do not worry about convergence.
gpcm_fit <- gpcm(nfc[1:500, ], control = list(GHk = 100))
gpcm_cat <- gpcmCat(gpcm_fit)


## Setting up for tests against catR package
## (different parameterization, so need params from ltm fits)
it_ltm <- matrix(c(ltm_cat@discrimination, coef(ltm_fit)[,1],
                   rep(0, length(ltm_cat@discrimination)),
                   rep(1, length(ltm_cat@discrimination))),
                   ncol = 4, byrow = F)
it_grm <- matrix(c(grm_cat@discrimination, coef(grm_fit)[,1:4]),
                    ncol= 5, byrow = F)
it_gpcm <- cbind(gpcm_cat@discrimination,
                 matrix(unlist(gpcm_cat@difficulty), ncol = 4, byrow = T))


save(ltm_fit, ltm_cat, grm_fit, grm_cat, gpcm_fit, gpcm_cat,
     it_ltm, it_grm, it_gpcm,
     file = "CATsurv/catSurv/tests/testthat/cat_objects.Rdata")
