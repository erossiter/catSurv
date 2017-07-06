context("estimateThetas")
load("cat_objects.Rdata")
data("nfc")
data("npi")
data("polknowMT")
data("polknowTAPS")

simulate_i <- function(i, cat){
  while(!checkStopRules(cat)){
    ask <- selectItem(cat)$next_item
    cat@answers[ask] <- i[ask]
    if(sum(!is.na(cat@answers)) == length(cat@answers)){
      break ##answered all questions
    }
  }
  return(estimateTheta(cat))
}

simulate_all <- function(cat, response_mat){
  out <- apply(response_mat, 1, FUN = simulate_i, cat = cat)
  names(out) <- NULL
  return(out)
}

test_that("Length threshold operates correctly", {
  ltm_cat@lengthThreshold <- tpm_cat@lengthThreshold <- grm_cat@lengthThreshold <- gpcm_cat@lengthThreshold <- 3
  
  expect_equal(simulateThetas(ltm_cat, npi[1:5, ]), simulate_all(ltm_cat, npi[1:5, ]))
  expect_equal(simulateThetas(tpm_cat, polknowMT[1:10,1:20]), simulate_all(tpm_cat, polknowMT[1:10,1:20]))
  expect_equal(simulateThetas(grm_cat, nfc[1:10, ]), simulate_all(grm_cat, nfc[1:10, ]))
  expect_equal(simulateThetas(gpcm_cat, polknowTAPS[1:10, ]), simulate_all(gpcm_cat, polknowTAPS[1:10, ]))
})


test_that("Multiple stopping rules operates correctly", {
  ## Can stop if answered 3 questions, but cannot stop unless all gains are less than .005
  ltm_cat@lengthThreshold <- tpm_cat@lengthThreshold <- grm_cat@lengthThreshold <- gpcm_cat@lengthThreshold <- 3
  ltm_cat@gainOverride <- tpm_cat@gainOverride <- grm_cat@gainOverride <- gpcm_cat@gainOverride <- .005

  expect_equal(simulateThetas(ltm_cat, npi[1:5, ]), simulate_all(ltm_cat, npi[1:5, ]))
  expect_equal(simulateThetas(tpm_cat, polknowMT[1:5,1:20]), simulate_all(tpm_cat, polknowMT[1:5,1:20]))
  expect_equal(simulateThetas(grm_cat, nfc[1:10, ]), simulate_all(grm_cat, nfc[1:10, ]))
  expect_equal(simulateThetas(gpcm_cat, polknowTAPS[1:10, ]), simulate_all(gpcm_cat, polknowTAPS[1:10, ]))
})

test_that("Errors are thrown due to bad input", {
  expect_error(simulateThetas(ltm_cat, npi))
  
  ltm_cat@answers[1] <- 1
  expect_error(simulateThetas(ltm_cat, npi))
})
