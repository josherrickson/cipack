test_that("contains", {
  myci <- makeCI(.95, lb = 2, ub = 5)
  expect_true(is.logical(contains(myci, 5)))
  expect_true(contains(myci, 4))
  expect_false(contains(myci, 10))
  #expect_false(contains(myci, 3))
})

test_that("contains error handling", {
  myci <- makeCI(.95, lb = 2, ub = 5)
  expect_error(contains(myci, "Abc"))
  #expect_error(contains(myci, 3))
})
