test_that("fold with count, to_list, and drain", {
  s <- from_seq(c(5, 6, 7))
  expect_equal(fold(count(), s), 3)
  expect_equal(fold(to_list(), s), list(5, 6, 7))
  expect_null(fold(drain(), s))
})

test_that("Puzzle 1", {
  expect_equal(fold(collect(5), integers_from(0)), take(integers_from(0), 5))
})

test_that("transduce includes the initial value", {
  sum_fold <- Fold(wrap = identity, step = `+`, init = 0, done = identity)
  expect_equal(take(transduce(sum_fold, from_seq(c(1, 2, 3))), Inf), c(0, 1, 3, 6))
})

test_that("transduce stops early", {
  expect_equal(take(transduce(collect(2), integers_from(0)), Inf), list(NULL, 0, c(0, 1)))
})

test_that("fold1 and transduce1", {
  max_fold <- Fold1(wrap = identity, step = max, done = identity)
  s <- from_seq(c(3, 1, 4, 1, 5))
  expect_equal(fold1(max_fold, s), 5)
  expect_equal(take(transduce1(max_fold, s), Inf), c(3, 3, 4, 4, 5))
  expect_error(fold1(max_fold, empty))
})

test_that("dimap, lmap, rmap", {
  sum_fold <- Fold(wrap = identity, step = `+`, init = 0, done = identity)
  s <- from_seq(c(1, 2, 3))
  expect_equal(fold(lmap(function(x) x * 10, sum_fold), s), 60)
  expect_equal(fold(rmap(function(r) r / 2, sum_fold), s), 3)
  expect_equal(fold(dimap(function(x) x * 10, function(r) r / 2, sum_fold), s), 30)
})

test_that("defer and resolve process several streams", {
  paused <- fold(defer(count()), from_seq(c(1, 2)))
  paused <- fold(defer(paused), from_seq(c(3, 4, 5)))
  expect_equal(resolve(paused), 5)
})

test_that("tee runs folds in parallel", {
  sum_fold <- Fold(wrap = identity, step = `+`, init = 0, done = identity)
  expect_equal(fold(tee(count(), sum_fold), from_seq(c(1, 2, 3))), list(3, 6))
})
