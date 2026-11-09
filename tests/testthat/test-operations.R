test_that("take", {
  expect_equal(take(integers_from(0), 5), c(0, 1, 2, 3, 4))
  expect_equal(take(from_seq(c(1, 2, 3)), Inf), c(1, 2, 3))
})

test_that("take matches the assignment examples", {
  fibs_from <- function(a, b) cons(a, fibs_from(b, a + b))
  fibs <- fibs_from(0, 1)
  expect_equal(take(fibs, 10), c(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
})

test_that("drop_n", {
  expect_equal(take(drop_n(integers_from(0), 3), 2), c(3, 4))
  expect_true(is_empty(drop_n(from_seq(c(1, 2)), 5)))
})

test_that("at counts from 0", {
  expect_equal(at(integers_from(10), 0), 10)
  expect_equal(at(integers_from(10), 3), 13)
})

test_that("split_at", {
  parts <- split_at(integers_from(0), 3)
  expect_equal(parts[[1]], c(0, 1, 2))
  expect_equal(head(parts[[2]]), 3)
})

test_that("map", {
  expect_equal(take(map(function(x) x * x, integers_from(0)), 5), c(0, 1, 4, 9, 16))
})

test_that("keep and the sieve example", {
  divisible <- function(x, y) (x %% y) == 0
  sieve <- function(stream) {
    p <- head(stream)
    not_div_p <- function(x) !divisible(x, p)
    cons(p, sieve(keep(tail(stream), not_div_p)))
  }
  primes <- sieve(integers_from(2))
  expect_equal(take(primes, 10), c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29))
})

test_that("remove_if", {
  expect_equal(take(remove_if(integers_from(1), function(x) x %% 2 == 0), 3), c(1, 3, 5))
})

test_that("map_maybe skips NULL", {
  f <- function(x) if (x %% 2 == 0) x * 10 else NULL
  expect_equal(take(map_maybe(f, integers_from(1)), 3), c(20, 40, 60))
})

test_that("mask", {
  s <- mask(integers_from(0), cycle(c(TRUE, FALSE)))
  expect_equal(take(s, 3), c(0, 2, 4))
})

test_that("zip_with stops at the shortest stream", {
  s <- zip_with(`+`, integers_from(0), from_seq(c(10, 20, 30)))
  expect_equal(take(s, Inf), c(10, 21, 32))
})

test_that("take_while and take_until are complementary", {
  s <- from_seq(c(1, 2, 3, 10, 4, 5))
  expect_equal(take_while(s, function(x) x < 5), c(1, 2, 3))
  expect_equal(take_until(s, function(x) x > 5), c(1, 2, 3))
})

test_that("drop_while and drop_until are complementary", {
  s <- from_seq(c(1, 2, 3, 10, 4, 5))
  expect_equal(take(drop_while(s, function(x) x < 5), Inf), c(10, 4, 5))
  expect_equal(take(drop_until(s, function(x) x > 5), Inf), c(10, 4, 5))
})

test_that("chain", {
  s <- chain(from_seq(c(1, 2)), empty, from_seq(c(3)), integers_from(10))
  expect_equal(take(s, 5), c(1, 2, 3, 10, 11))
})

test_that("interleave", {
  expect_equal(take(interleave(repeatedly(0), repeatedly(1)), 4), c(0, 1, 0, 1))
  expect_equal(take(interleave(from_seq(c(1, 2)), from_seq(c(10, 20, 30))), Inf),
               c(1, 10, 2, 20, 30))
})

test_that("cumsum and diff on streams", {
  s <- from_seq(c(1, 4, 9, 16))
  expect_equal(take(cumsum(s), Inf), c(1, 5, 14, 30))
  expect_equal(take(diff(s), Inf), c(3, 5, 7))
})

test_that("cumsum and diff on vectors are unchanged", {
  expect_equal(cumsum(c(1, 2, 3)), c(1, 3, 6))
  expect_equal(diff(c(1, 4, 9)), c(3, 5))
})

test_that("slice", {
  expect_equal(take(slice(integers_from(0), 2, 10, 3), Inf), c(2, 5, 8))
  expect_equal(take(slice(integers_from(0), dn = 2), 3), c(0, 2, 4))
})

test_that("chunked", {
  s <- chunked(integers_from(0), 2)
  expect_equal(take(s, 2), list(c(0, 1), c(2, 3)))
  s2 <- chunked(integers_from(0), 3, 1)
  expect_equal(take(s2, 2), list(c(0, 1, 2), c(1, 2, 3)))
})

test_that("group_by groups consecutive items", {
  s <- group_by(from_seq(c(1, 3, 2, 4, 5)), function(x) x %% 2)
  groups <- take(s, Inf)
  expect_equal(length(groups), 3)
  expect_equal(take(groups[[1]], Inf), c(1, 3))
  expect_equal(take(groups[[3]], Inf), 5)
})

test_that("long streams do not overflow the stack", {
  expect_equal(at(map(function(x) x + 1, integers_from(0)), 5000), 5001)
})
