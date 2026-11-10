## Answers

### Puzzle 1

`collect(n)` uses `step = function(acc, x) { acc <- c(acc, x); if (length(acc) >= n) reduced(acc) else acc }`. `reduced()` stops the fold early, so `fold(collect(n), s)` equals `take(s, n)` even for infinite `s`.

### Puzzle 2

It works for length 2 or more but is not right. For length 1, `(n - 1):1` is `0:1`, so `from_seq(c(5))` gives `5, numeric(0), 5`. Using `vec[i]` instead of `vec[[i]]` wraps list elements in a list. It builds the whole stream but losing laziness. It also only works because `delay` copies `s` when `cons` is called, with a plain delayed expression every tail would point to the final `s`.

### Estimating pi/4 and ln(2)

Each sequence was compared with the true value after 8 terms. For pi/4, the error is about 3e-2 for `pi_4_series`, 8e-5 after one Euler improvement, and about 1e-15 for both the first and second elements of the inner streams of `iterate(euler_improve, pi_4_series)`. 

For ln(2) the errors are about 6e-2, 1e-4, and 1e-15. Since the error of the original series is about 1/n, it would need about 10^13 terms to reach this accuracy.


### Differential Equations

`y' = y`, `dt = 0.001`: y(1) = 2.7169 (e = 2.7183).

`y'' = -y`, y(0) = 1, y'(0) = 0: y(1) = 0.5406 (cos 1 = 0.5403).


### Power Series

The reciprocal of exp(z) gives the coefficients of exp(-z). 

Convolving three Bernoulli(p) generating functions gives `dbinom(0:3, 3, p)`.

Puzzle 102: the 5th differences are all 0 and the first element of each lower-order difference is 1, so the next term is choose(9, 0) + ... + choose(9, 4) = 256.
