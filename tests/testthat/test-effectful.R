make_file <- function(lines) {
  path <- tempfile(fileext = ".txt")
  writeLines(lines, path)
  path
}

test_that("line_from reads all lines and then closes the file", {
  path <- make_file(c("a b", "c"))
  before <- nrow(showConnections())
  expect_equal(take(line_from(path), Inf), c("a b", "c"))
  expect_equal(nrow(showConnections()), before)
})

test_that("grep_files gives (file, line number, line)", {
  path <- make_file(c("apple", "banana", "apricot"))
  matches <- take(grep_files("ap", path), Inf)
  expect_equal(matches[[2]], list(file = path, line = 3, text = "apricot"))
})

test_that("grep_output respects the options", {
  path <- make_file(c("apple", "banana"))
  expect_output(grep_output("an", path, show_file = FALSE), "^2:banana")
})

test_that("wc counts lines, words, and characters", {
  path <- make_file(c("a b", "c", "d e f"))
  expect_equal(wc(path), c(lines = 3, words = 6, chars = 12))
})

test_that("socket streams replies from the echo server", {
  server <- test_path("..", "..", "exercises", "echo.R")
  bg <- callr::r_bg(function(path) source(path), args = list(normalizePath(server)))
  on.exit(bg$kill())
  Sys.sleep(2)
  expect_equal(take(socket(from_seq(c("hello"))), Inf), "<< hello >>")
})
