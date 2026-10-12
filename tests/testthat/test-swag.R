test_that("swag reproduces the examples from the assignment", {
  expect_equal(
    swag(Concat(), 4, c("a", "b", "c", "d", "e", "f", "g", "h")),
    c("abcd", "bcde", "cdef", "defg", "efgh")
  )
  expect_equal(swag(Sum(), 3, c(0, 10, 20, 30, 40, 50)), c(30, 60, 90, 120))

  d1 <- list(a = 10, b = 1)
  d2 <- list(a = 3, b = 2)
  d3 <- list(b = 4, c = 0)
  d4 <- list()
  expect_equal(
    swag(DictMerge(), 2, list(d1, d2, d3, d4)),
    list(list(a = 3, b = 2), list(a = 3, b = 4, c = 0), list(b = 4, c = 0))
  )
})

test_that("swag handles edge cases", {
  expect_equal(swag(Sum(), 1, c(4, 5, 6)), c(4, 5, 6))
  expect_equal(swag(Concat(), 3, c("x", "y", "z")), "xyz")
  expect_length(swag(Sum(), 5, c(1, 2, 3)), 0)
})

test_that("swag rejects a non-positive window size", {
  expect_error(swag(Sum(), 0, c(1, 2, 3)))
})

# swag stream

stream_values <- function(stream) {
  values <- list()
  step <- next_elt(stream)
  while (step$active) {
    values <- c(values, list(step$value))
    step <- next_elt(step$rest)
  }
  values
}

test_that("swag_stream reproduces the examples from the assignment", {
  out <- swag_stream(Sum(), 3, make_stream(c(0, 10, 20, 30, 40, 50)))
  expect_equal(stream_values(out), list(30, 60, 90, 120))

  out <- swag_stream(Concat(), 4, make_stream(c("a", "b", "c", "d", "e", "f", "g", "h")))
  expect_equal(stream_values(out), list("abcd", "bcde", "cdef", "defg", "efgh"))
})

test_that("swag_stream returns an empty stream when the input is shorter than the window", {
  out <- swag_stream(Sum(), 5, make_stream(c(1, 2, 3)))
  expect_false(is_active(out))
})

# swag prime
test_that("swag_prime computes running regression coefficients", {
  x <- c(1, 2, 3, 4, 5, 6, 7)
  y <- c(2.0, 4.1, 5.9, 8.2, 9.8, 12.1, 14.0)
  w <- 4

  out <- swag_prime(regression_agg(), w, make_stream(Map(c, x, y)))

  expected <- lapply(1:(length(x) - w + 1), function(i) {
    window <- i:(i + w - 1)
    unname(coef(lm(y[window] ~ x[window])))
  })
  expect_equal(lapply(stream_values(out), unname), expected)
})

# swag group
test_that("swag_group reproduces the addition example", {
  out <- swag_group(Sum(), 3, make_stream(c(0, 10, 20, 30, 40, 50)))
  expect_equal(stream_values(out), list(30, 60, 90, 120))
})

test_that("swag_group agrees with swag_stream", {
  input <- c(5, -2, 7, 1, -8, 3, 4, 9, -6, 2)
  for (w in c(2, 3, 5, 10)) {
    expect_equal(
      stream_values(swag_group(Sum(), w, make_stream(input))),
      stream_values(swag_stream(Sum(), w, make_stream(input)))
    )
  }
})

