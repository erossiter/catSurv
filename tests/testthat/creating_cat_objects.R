## creating Cat objects for testing purposes

## loading datasets from package
data("nfc")
data("npi")
data("polknowMT")
data("polknowTAPS")

ltm_fit <- ltm(npi ~ z1, control = list(GHk = 100))
ltm_cat <- ltmCat(ltm_fit)

tpm_fit <- tpm(polknowMT[,1:20], start.val = "random")
tpm_cat <- tpmCat(tpm_fit)

grm_fit <- grm(nfc, control = list(GHk = 100))
grm_cat <- grmCat(grm_fit)

gpcm_fit <- gpcm(polknowTAPS, constraint = "gpcm", control = list(iter.qN = 200, GHk = 100))
gpcm_cat <- gpcmCat(gpcm_fit)


## Setting up for tests against catR package
## (different parameterization, so need params from ltm fits)
it_ltm <- matrix(c(ltm_cat@discrimination, coef(ltm_fit)[,1],
                   rep(0, length(ltm_cat@discrimination)),
                   rep(1, length(ltm_cat@discrimination))),
                   ncol = 4, byrow = F)
it_tpm <- matrix(c(tpm_cat@discrimination, coef(tpm_fit)[,1],
                   tpm_cat@guessing,
                   rep(1, length(tpm_cat@discrimination))),
                   ncol = 4, byrow = F)
it_grm <- matrix(c(grm_cat@discrimination, coef(grm_fit)[,1:4]),
                    ncol= 5, byrow = F)
it_gpcm <- cbind(gpcm_cat@discrimination,
                 matrix(c(unlist(gpcm_cat@difficulty[1:5]), NA,
                          unlist(gpcm_cat@difficulty[6:10])),
                        ncol = 4, byrow = T))


save(ltm_fit, ltm_cat, tpm_fit, tpm_cat,
     grm_fit, grm_cat, gpcm_fit, gpcm_cat,
     it_ltm, it_grm, it_gpcm,
     file = "catSurv/tests/testthat/cat_objects.Rdata")
