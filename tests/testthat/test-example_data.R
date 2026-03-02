test_that("example_data$interview has expected dimensions", {
  expect_equal(nrow(example_data$interview), 1860)
  expect_equal(ncol(example_data$interview), 39)
})
