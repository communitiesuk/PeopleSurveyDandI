test_that("addition works", {
  expect_equal(Add_Numbers(2,2), 4)
  expect_equal(Add_Numbers(-2,2), 0)
  expect_equal(Add_Numbers(-2,-2), -4)
})

