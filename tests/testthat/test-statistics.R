test_that("uniforms lie in the interval", {
  x <- take(uniforms(2, 5), 100)
  expect_true(all(x >= 2 & x <= 5))
})

test_that("normals have roughly the right mean", {
  set.seed(1)
  expect_equal(mean(take(normals(3, 1), 2000)), 3, tolerance = 0.1)
})

test_that("rejection_sample stays in the support with the right mean", {
  set.seed(1)
  x <- take(rejection_sample(function(x) 2 * x, 0, 1, 2), 2000)
  expect_true(all(x >= 0 & x <= 1))
  expect_equal(mean(x), 2 / 3, tolerance = 0.05)
})

test_that("mcmc_normal centers on the posterior mean", {
  set.seed(1)
  y <- c(1.2, 0.8, 1.5, 1.1, 0.9)
  draws <- take(mcmc_normal(uniforms(), y, 1, 0, 10, 0, 0.5), 3000)
  posterior_mean <- (sum(y) / 1 + 0 / 100) / (length(y) / 1 + 1 / 100)
  expect_equal(mean(draws[-(1:500)]), posterior_mean, tolerance = 0.1)
})

test_that("local_smooth reproduces a straight line", {
  pts <- from_seq(lapply(1:10, function(x) c(x, 2 * x + 1)))
  fitted <- take(local_smooth(pts, 3), Inf)
  expect_equal(vapply(fitted, function(p) p[2], numeric(1)), 2 * (1:10) + 1)
})
