
test_that("output of MFO Kinetics function", {

  # Get old working directory
  oldwd <- getwd()

  # Set temporary directory
  setwd(tempdir())

  # Simulate MFO databases
  # Basal Metabolism; rows = 48, HR, VO2, VCO2 and RER
  HR <- rnorm(n = 48, mean = 66.21, sd = 4.15)
  VO2 <- rnorm(n = 48, mean = 281.33, sd = 53.50)
  VCO2 <- rnorm(n = 48, mean = 219.96, sd = 36.57)
  RER <- rnorm(n = 48, mean = 66.21, sd = 4.15)

  M_basal <- data.frame(HR, VO2, VCO2, RER)

  # MFO test; rows = 45, HR, VO2, VCO2 and RER
  # Random increment
  random_inc <- rnorm(n = 45, mean = 0, sd = 1)

  HR <- seq(82,162, by = 1.78) + random_inc
  VO2 <- seq(837, 2179, by = 29.823) + random_inc
  VCO2 <- seq(681, 2222, by = 34.25) + random_inc
  RER <- seq(0.81, 1.02, by = 0.00467) + random_inc

  MFO_test <- data.frame(HR, VO2, VCO2, RER)

  # VO2max
  VO2max <- rnorm(n = 1, mean = 2615, sd = 25)

  # Random cv_var
  cv_var <- sample(c("VO2", "VCO2", "RER"), 1)

  # Random author
  author <- sample(c("Frayn", "Jeukendrup"), 1)

  test_result_MFO <- MFO(step_time = 20,
                         db_MFO = MFO_test,
                         db_basal = M_basal,
                         db_graded = NULL,
                         cv_var = "VO2",
                         author = author,
                         VO2max = VO2max)

  test_result_MFO_kinetics <- MFO_kinetics(test_result_MFO$MFO_db)

  expect_equal(ncol(test_result_MFO_kinetics$MFO_kinetics_data), 5)
  expect_equal(class(test_result_MFO_kinetics$d), "numeric")
  expect_equal(class(test_result_MFO_kinetics$s), "numeric")
  expect_equal(class(test_result_MFO_kinetics$t), "numeric")

  # set user working directory
  setwd(oldwd)

})

