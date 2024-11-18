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

test_that("mod_methyLImp2", {
  set.seed(1234)
  beta_sim <- sim_mat(500, 10, perc_NA = 0.2)
  beta_sim$input[, 1] <- NA
  # All NA column
  expect_error(mod_methyLImp2(
    input = beta_sim$input,
    type = "user",
    annotation = beta_sim$user
  ))
  # All NA row
  beta_sim <- sim_mat(500, 10, perc_NA = 0.2)
  beta_sim$input[1, ] <- NA
  expect_error(mod_methyLImp2(
    input = beta_sim$input,
    type = "user",
    annotation = beta_sim$user
  ))
})

test_that("skip imputation id", {
  
})