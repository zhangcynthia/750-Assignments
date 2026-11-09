# Euler improvement of a sequence
#' @export
euler_improve <- function(s) {
  improve <- function(s0, s1, s2) {
    s2 - (s2 - s1)^2 / (s2 - 2 * s1 + s0)
  }
  zip_with(improve, s, tail(s), tail(tail(s)))
}

# Integrate a stream
#' @export
integrate <- function(x, dt, initial_value = 0) {
  cons(initial_value, {
    xs <- if (is_stream(x)) x else force(x)
    map(function(v) initial_value + v, cumsum(map(function(v) v * dt, xs)))
  })
}

# First-order differential equation
#' @export
solve_diffeq1 <- function(f, y_0, dt) {
  e <- new.env()
  y <- integrate(delay(e$dy), dt, y_0)
  e$dy <- map(f, y)
  y
}

# Second-order differential equation
#' @export
solve_diffeq2 <- function(a, b, y_0, dy_0, dt) {
  e <- new.env()
  y <- integrate(delay(e$dy), dt, y_0)
  e$dy <- integrate(delay(e$ddy), dt, dy_0)
  e$ddy <- zip_with(function(dy, y) a * dy + b * y, e$dy, y)
  y
}

method(merge, Stream) <- function(x, y, ordering, ...) {
  if (is_empty(x)) return(y)
  if (is_empty(y)) return(x)
  if (ordering(head(x), head(y))) {
    cons(head(x), merge(tail(x), y, ordering))
  } else {
    cons(head(y), merge(x, tail(y), ordering))
  }
}

# Product of two streams
#' @export
convolve <- function(s, t) {
  coef <- function(n) {
    a <- take(s, n + 1)
    b <- take(t, n + 1)
    if (length(a) + length(b) - 1 <= n) return(empty)
    a <- c(a, rep(0, n + 1 - length(a)))
    b <- c(b, rep(0, n + 1 - length(b)))
    cons(sum(a * rev(b)), coef(n + 1))
  }
  coef(0)
}

# Shuffle product of two streams
#' @export
shuffle_product <- function(s, t) {
  coef <- function(n) {
    a <- take(s, n + 1)
    b <- take(t, n + 1)
    if (length(a) + length(b) - 1 <= n) return(empty)
    a <- c(a, rep(0, n + 1 - length(a)))
    b <- c(b, rep(0, n + 1 - length(b)))
    cons(sum(choose(n, 0:n) * a * rev(b)), coef(n + 1))
  }
  coef(0)
}

# Reciprocal
#' @export
reciprocal <- function(s) {
  coef <- function(n, prev) {
    a <- take(s, n + 1)
    a <- c(a, rep(0, n + 1 - length(a)))
    t_n <- -sum(a[-1] * rev(prev))
    cons(t_n, coef(n + 1, c(prev, t_n)))
  }
  cons(1, coef(1, 1))
}
