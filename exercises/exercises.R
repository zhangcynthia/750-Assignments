devtools::load_all()

# Estimating pi/4
pi_4_series <- cumsum(map(function(n) (-1)^n / (2 * n + 1), integers_from(0)))
pi / 4

take(pi_4_series, 8)
take(euler_improve(pi_4_series), 8)

take(map(function(s) head(s), iterate(euler_improve, pi_4_series)), 8)
take(map(function(s) at(s, 1), iterate(euler_improve, pi_4_series)), 8)

# Estimating ln(2)
ln2_series <- cumsum(map(function(n) (-1)^n / (n + 1), integers_from(0)))
log(2)

take(ln2_series, 8)
take(euler_improve(ln2_series), 8)

take(map(function(s) head(s), iterate(euler_improve, ln2_series)), 8)
take(map(function(s) at(s, 1), iterate(euler_improve, ln2_series)), 8)

# Solving Differential Equations
# First order
at(solve_diffeq1(function(y) y, 1, 0.001), 1000)
# Second order
at(solve_diffeq2(0, -1, 1, 0, 0.001), 1000)

# Streams of Pairs
# Pairs <s_i, t_j> ordered by s_i + t_j, then by s_i
pairs_by_sum <- function(s, t) {
  ordering <- function(a, b) {
    sum(a) < sum(b) || (sum(a) == sum(b) && a[1] <= b[1])
  }
  first <- head(s)
  cons(c(first, head(t)),
       merge(map(function(tj) c(first, tj), tail(t)),
             pairs_by_sum(tail(s), t),
             ordering))
}

take(pairs_by_sum(integers_from(1), integers_from(1)), 10)

# Power Series
# Reciprocal of s = exp(z) = 1 + z + z^2/2! + ...
exp_series <- map(function(n) 1 / factorial(n), integers_from(0))
take(reciprocal(exp_series), 6)

# Probability generating function of a sum of three independent Bernoulli(p)
p <- 0.3
bernoulli_pgf <- from_seq(c(1 - p, p))
take(convolve(convolve(bernoulli_pgf, bernoulli_pgf), bernoulli_pgf), Inf)

# Puzzle 102
a <- from_seq(c(1, 2, 4, 8, 16, 31, 57, 99, 163))
delta_at_0 <- take(map(function(s) head(s), iterate(function(s) diff(s), a)), 9)
sum(choose(9, 0:8) * delta_at_0)

# Statistics: Draws from Distributions
take(uniforms(2, 5), 5)
take(normals(0, 1), 5)

# Statistics: Rejection Sampling
# Density f(x) = 6 x (1 - x) on [0, 1], bounded by 1.5
draws <- take(rejection_sample(function(x) 6 * x * (1 - x), 0, 1, 1.5), 1000)
mean(draws)

# Statistics: MCMC
y <- c(1.2, 0.8, 1.5, 1.1, 0.9)
samples <- take(mcmc_normal(uniforms(), y, sigma = 1, mu0 = 0, tau0 = 10,
                            theta0 = 0, step = 0.5), 3000)
mean(samples[-(1:500)])

# Statistics: Smoothing
x <- seq(0, 2 * pi, length.out = 50)
pts <- from_seq(lapply(x, function(xi) c(xi, sin(xi) + rnorm(1, 0, 0.2))))
smoothed <- take(local_smooth(pts, h = 1), Inf)
head(smoothed, 3)

# Effectful Computations: line_from
take(line_from("DESCRIPTION"), 3)

# Effectful Computations: grep
r_files <- list.files("R", full.names = TRUE)
take(grep_files("^method\\(", r_files), 2)
grep_output("^method\\(", r_files)
grep_output("^method\\(", r_files, show_file = FALSE)
grep_output("^method\\(", r_files, output = tempfile())

# Effectful Computations: wc
wc("R/core.R")

# Effectful Computations: socket
take(socket(from_seq(c("hello", "streams"))), Inf)

