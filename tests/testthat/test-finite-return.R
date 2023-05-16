## Testing if the function works as expected
test_that(desc = "log_summed_exponents_correct", code = {
  testthat::expect_equal(object = log_summed_exps(x = c(1:200)),
                         expected = log(sum(exp(c(1:200)))),
                         tolerance = 1e-10)
})

## Testing if the value returned is finite for large sums
test_that(desc = "log_summed_exponents_are_finite", code = {
  testthat::expect_lt(object = log_summed_exps(x = c(1:2000)), expected = 1e1000)
})

## Testing if the function takes non-numeric values
test_that(desc = "does not take non-numeric values", code = {
  testthat::expect_error(object = log_summed_exps(x = letters))
})
