test_that("a stream yields its values in order", {
  s <- make_stream(c(1, 2))
  expect_true(is_active(s))

  r1 <- next_elt(s)
  expect_true(r1$active)
  expect_equal(r1$value, 1)

  r2 <- next_elt(r1$rest)
  expect_equal(r2$value, 2)

  expect_false(is_active(r2$rest))
  expect_false(next_elt(r2$rest)$active)
})
