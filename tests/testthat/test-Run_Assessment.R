test_that("Run_Assessment without file variable", {
  expect_error(run_Assessment())
})

test_that("Run_Assessment with no 'assessment' variable", {
  x <- 1
  expect_error(run_Assessment(x))
})
