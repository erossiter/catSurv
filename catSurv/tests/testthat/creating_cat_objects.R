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

# save(ltm_fit, ltm_cat, grm_fit, grm_cat, gpcm_fit, gpcm_cat,
#      file = "CATsurv/catSurv/tests/testthat/cat_objects.Rdata")
