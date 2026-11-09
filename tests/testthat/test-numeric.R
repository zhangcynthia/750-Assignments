test_that("euler_improve", {
  s <- cumsum(map(function(n) (-1)^n / (2 * n + 1), integers_from(0)))
  expect_lt(abs(at(euler_improve(s), 5) - pi / 4), abs(at(s, 6) - pi / 4))
})

test_that("integrate", {
  s <- integrate(from_seq(c(1, 2, 3)), 0.5, 10)
  expect_equal(take(s, Inf), c(10, 10.5, 11.5, 13))
})

test_that("diffeq1", {
  y <- solve_diffeq1(function(y) y, 1, 0.001)
  expect_equal(at(y, 1000), exp(1), tolerance = 1e-3)
})

test_that("diffeq2", {
  y <- solve_diffeq2(0, -1, 1, 0, 0.001)
  expect_equal(at(y, 1000), cos(1), tolerance = 1e-3)
})

test_that("merge", {
  s <- merge(from_seq(c(1, 4, 6)), from_seq(c(2, 3, 7)), function(a, b) a <= b)
  expect_equal(take(s, Inf), c(1, 2, 3, 4, 6, 7))
})

test_that("convolve and shuffle product", {
  s <- from_seq(c(1, 1))
  expect_equal(take(convolve(s, s), Inf), c(1, 2, 1))
  expect_equal(take(shuffle_product(s, s), Inf), c(1, 2, 2))
})

test_that("reciprocal", {
  s <- from_seq(c(1, -1))
  expect_equal(take(reciprocal(s), 5), c(1, 1, 1, 1, 1))
  expect_equal(take(convolve(s, reciprocal(s)), 4), c(1, 0, 0, 0))
})
