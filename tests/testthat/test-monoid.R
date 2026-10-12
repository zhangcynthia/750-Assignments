test_that("Sum satisfies the monoid laws", {
  m <- Sum()
  x <- c(1, 2)
  y <- c(-3, 5)
  z <- c(0.5, 4)
  expect_equal(mop(m, munit(m), x), x)
  expect_equal(mop(m, x, munit(m)), x)
  expect_equal(mop(m, mop(m, x, y), z), mop(m, x, mop(m, y, z)))
})

test_that("Sum works on plain numbers", {
  m <- Sum()
  expect_equal(mop(m, 3, 4), 7)
  expect_equal(mop(m, munit(m), 5), 5)
})

test_that("mean_agg computes the average", {
  agg <- mean_agg()
  expect_equal(aggregate_window(agg, list(1, 2, 3, 4)), 2.5)
  expect_equal(aggregate_window(agg, list(7)), 7)
  xs <- c(-2.5, 10, 3.3, 0, 8)
  expect_equal(aggregate_window(agg, as.list(xs)), mean(xs))
})

test_that("sd_agg computes the sample standard deviation", {
  agg <- sd_agg()
  xs <- c(2, 4, 4, 4, 5, 5, 7, 9)
  expect_equal(aggregate_window(agg, as.list(xs)), sd(xs))
  expect_equal(aggregate_window(agg, list(3, 3, 3)), 0)
})

test_that("regression_agg matches lm", {
  agg <- regression_agg()
  x <- c(1, 2, 3, 4, 5)
  y <- c(2.1, 3.9, 6.2, 7.8, 10.1)
  points <- Map(c, x, y)
  expected <- unname(coef(lm(y ~ x)))
  expect_equal(unname(aggregate_window(agg, points)), expected)
})
