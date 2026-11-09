# Stream of iid Uniform(a, b) draws
#' @export
uniforms <- function(a = 0, b = 1) {
  cons(stats::runif(1, a, b), uniforms(a, b))
}

# Stream of iid Normal(mean, sd) draws
#' @export
normals <- function(mean = 0, sd = 1) {
  cons(stats::rnorm(1, mean, sd), normals(mean, sd))
}

# Stream of iid draws from a density on [a, b] by rejection sampling
#' @export
rejection_sample <- function(density, a, b, M) {
  proposals <- zip_with(c, uniforms(a, b), uniforms(0, M))
  accepted <- keep(proposals, function(p) p[2] <= density(p[1]))
  map(function(p) p[1], accepted)
}

# Metropolis-Hastings
#' @export
mcmc_normal <- function(u, y, sigma, mu0, tau0, theta0, step) {
  log_post <- function(theta) {
    sum(stats::dnorm(y, theta, sigma, log = TRUE)) +
      stats::dnorm(theta, mu0, tau0, log = TRUE)
  }
  mh_from <- function(theta, u) {
    proposal <- theta + step * stats::qnorm(head(u))
    accept <- log(head(tail(u))) < log_post(proposal) - log_post(theta)
    theta_new <- if (accept) proposal else theta
    cons(theta_new, mh_from(theta_new, tail(tail(u))))
  }
  mh_from(theta0, u)
}

# Local polynomial regression smoothing
#' @export
local_smooth <- function(s, h, degree = 1) {
  fit_at <- function(x0, window) {
    xs <- vapply(window, function(p) p[1], numeric(1))
    ys <- vapply(window, function(p) p[2], numeric(1))
    w <- (1 - (abs(xs - x0) / h)^3)^3
    X <- outer(xs - x0, 0:degree, `^`)
    unname(stats::lm.wfit(X, ys, w)$coefficients[1])
  }
  smooth_from <- function(s, past) {
    if (is_empty(s)) return(empty)
    current <- head(s)
    x0 <- current[1]
    past <- Filter(function(p) x0 - p[1] < h, past)
    future <- take_while(tail(s), function(p) p[1] - x0 < h)
    window <- c(past, list(current), future)
    cons(c(x0, fit_at(x0, window)), smooth_from(tail(s), c(past, list(current))))
  }
  smooth_from(s, list())
}
