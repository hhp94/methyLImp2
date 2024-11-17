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

test_that("mod_methyLImp2_internal handles NA patterns correctly", {
  # Generate test data
  set.seed(123)
  sim_data <- sim_mat(n = 100, m = 100, nchr = 2, perc_NA = 0.1)

  # Subset to chr1 data
  test_mat <- t(sim_data$input[sim_data$user$chr == "chr1", ])
  # Fill NAs in some columns because otherwise no CpGs has no missing
  test_mat[is.na(test_mat[, 1]), 1] <- 0.5
  test_mat[is.na(test_mat[, 2]), 2] <- 0.5

  # Test case 1: Matrix with NAs (should return matrix)
  args_methyLImp2_internal <- list(
    dat = test_mat,
    skip_imputation_ids = NULL,
    min = 0,
    max = 1,
    minibatch_frac = 1,
    minibatch_reps = 1
  )
  for (i in list("mod_methyLImp2_internal", "methyLImp2_internal")) {
    result <- do.call(i, args_methyLImp2_internal)
    expect_true(is.matrix(result))
  }
  # Test case 2: Matrix with no NAs (should return string)
  test_mat_no_na <- test_mat
  test_mat_no_na[is.na(test_mat_no_na)] <- 0.5
  args_methyLImp2_internal$dat <- test_mat_no_na

  for (i in list("mod_methyLImp2_internal", "methyLImp2_internal")) {
    result <- do.call(i, args_methyLImp2_internal)
    expect_equal(result, "No columns with missing values detected.")
  }

  # Test case 3: Matrix with all NAs in a column
  test_mat_all_na <- test_mat
  test_mat_all_na[, "cg1"] <- NA
  args_methyLImp2_internal$dat <- test_mat_all_na

  for (i in list("mod_methyLImp2_internal", "methyLImp2_internal")) {
    result <- do.call(i, args_methyLImp2_internal)
    expect_true(ncol(result) < ncol(test_mat))
  }
})
