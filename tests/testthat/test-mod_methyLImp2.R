test_that("same results", {
  set.seed(1234)
  beta_sim <- sim_mat(500, 10, perc_NA = 0.2)

  set.seed(1234)
  m <- methyLImp2(
    input = beta_sim$input,
    type = "user",
    annotation = beta_sim$user,
    BPPARAM = BiocParallel::SerialParam(RNGseed = 1234)
  )

  set.seed(1234)
  m_mod <- mod_methyLImp2(
    input = beta_sim$input,
    type = "user",
    annotation = beta_sim$user
  )

  expect_equal(m, m_mod)
})

test_that("mod_methyLImp2 input validations", {
  set.seed(1234)
  sim <- function() {
    sim_mat(500, 10, perc_NA = 0.2)
  }
  beta_sim <- sim()
  beta_sim$input[, 1] <- NA
  # All NA column
  expect_error(mod_methyLImp2(
    input = beta_sim$input,
    type = "user",
    annotation = beta_sim$user
  ))
  # All NA row
  beta_sim <- sim()
  beta_sim$input[1, ] <- NA
  expect_error(mod_methyLImp2(
    input = beta_sim$input,
    type = "user",
    annotation = beta_sim$user
  ))
})

test_that("skip imputation id works", {
  set.seed(1234)
  beta_sim <- sim_mat(500, 10, perc_NA = 0.5)
  
  # Make sure the cgs we are skipping has missing data
  na_cols <- colSums(is.na(beta_sim$input))
  cg <- which(na_cols > 0)[1:3]
  expect_true(anyNA(beta_sim$input[, cg]))

  argv <- methyLImp2_internal_args()
  argv$skip_imputation_ids <- NULL
  
  # same results given skip imputation ids
  m <- suppressWarnings({
    do.call(
      "methyLImp2_internal",
      c(
        list(dat = beta_sim$input),
        argv,
        list(skip_imputation_ids = cg)
      )
    )
  })
  m1 <- do.call(
    "mod_methyLImp2_internal",
    c(
      list(dat = beta_sim$input),
      argv,
      list(skip_imputation_ids = names(cg))
    )
  )
  expect_equal(m, m1)

  expect_error(
    do.call(
      "mod_methyLImp2",
      c(
        list(dat = beta_sim$input),
        argv,
        list(skip_imputation_ids = "INVALID CPG")
      )
    )
  )

  expect_error(
    do.call(
      "mod_methyLImp2",
      c(
        list(dat = beta_sim$input),
        argv,
        list(skip_imputation_ids = 1)
      )
    )
  )
})
