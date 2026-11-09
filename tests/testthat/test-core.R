test_that("empty is the empty stream", {
  expect_true(is_stream(empty))
  expect_true(is_empty(empty))
})

test_that("rejects non-streams", {
  expect_false(is_stream(c(1, 2, 3)))
})

test_that("cons, head, and tail", {
  s <- cons(1, empty)
  expect_true(is_stream(s))
  expect_false(is_empty(s))
  expect_equal(head(s), 1)
  expect_true(is_empty(tail(s)))
})

test_that("cons does not evaluate the tail", {
  expect_no_error(cons(1, stop("tail evaluated")))
})

test_that("forcing the tail is memoized", {
  counter <- new.env()
  counter$n <- 0
  s <- cons(1, {
    counter$n <- counter$n + 1
    empty
  })
  tail(s)
  tail(s)
  expect_equal(counter$n, 1)
})

test_that("integers_from example from the assignment", {
  integers_from <- function(n) {
    cons(n, integers_from(n + 1))
  }
  s <- integers_from(0)
  expect_equal(head(s), 0)
  expect_equal(head(tail(s)), 1)
  expect_equal(head(tail(tail(s))), 2)
})

test_that("fibs example", {
  fibs_from <- function(a, b) cons(a, fibs_from(b, a + b))
  fibs <- fibs_from(0, 1)
  expect_equal(head(tail(tail(tail(fibs)))), 2)
})

test_that("delay and force", {
  d <- delay(1 + 2)
  expect_equal(force(d), 3)
})

test_that("print and str", {
  expect_output(print(empty), "empty")
  expect_output(print(cons(1, empty)), "stream")
  expect_output(str(cons(1, empty)), "stream")
})

test_that("integers_from", {
  s <- integers_from(3)
  expect_equal(head(s), 3)
  expect_equal(head(tail(s)), 4)
})

test_that("repeatedly", {
  s <- repeatedly(7)
  expect_equal(head(tail(s)), 7)
})

test_that("iterate", {
  s <- iterate(function(x) 2 * x, 1)
  expect_equal(head(tail(tail(s))), 4)
})

test_that("cycle", {
  s <- cycle(c(1, 2, 3))
  expect_equal(head(tail(tail(tail(s)))), 1)
})

test_that("from_seq", {
  s <- from_seq(c(1, 2))
  expect_equal(head(s), 1)
  expect_equal(head(tail(s)), 2)
  expect_true(is_empty(tail(tail(s))))
  expect_true(is_empty(from_seq(c())))
})
