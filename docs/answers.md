## Answers

### Puzzle 1

`collect(n)` uses `step = function(acc, x) { acc <- c(acc, x); if (length(acc) >= n) reduced(acc) else acc }`.
`reduced()` stops the fold early, so `fold(collect(n), s)` equals `take(s, n)` even for infinite `s`.

### Puzzle 2

### Estimating pi/4 and ln(2)

### Differential Equations

### Power Series
