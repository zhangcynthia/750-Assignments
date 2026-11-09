## Test Summary

Tests are in `tests/testthat/`, one file per source file.

### test-core.R

These tests check the basic stream structure. They confirm that `empty` is a stream and is recognized by `is_empty`, that `is_stream` rejects an ordinary vector, and that `cons`, `head`, and `tail` build and take apart a stream correctly. Two tests check the two defining properties of delayed pairs. 1.`cons(1, stop(...))` does not raise an error, which shows that the tail is not evaluated when the stream is built. 
2. A tail that increments a counter is forced twice but the counter ends at 1, which shows that the tail is computed only once and then cached. The `integers_from` and `fibs_from` examples from the assignment are run directly. `delay` and `force` are checked on a simple expression, and `print` and `str` are checked to produce output. Finally, the constructors from Table 2 (`integers_from`, `repeatedly`, `iterate`, `cycle`, `from_seq`) are each checked on their first few elements, including that `cycle` wraps around to its first element and that `from_seq` ends with `empty`.

### test-operations.R

These tests cover the Table 3 operations on infinite and finite streams. `take`, `drop_n`, `at` (counting from 0), and `split_at` are checked on simple inputs, and `take` also against the Fibonacci example. `map` and `keep` are tested with the squares and prime sieve examples from the assignment. The `while`/`until` pairs are tested with opposite predicates to show they are complementary. `chain`, `interleave`, and `zip_with` are checked with streams of different lengths. `cumsum` and `diff` are tested on streams and on vectors.

### test-fold.R

These tests check `fold` with the three required folds (`count`, `to_list`, `drain`), and `fold1`/`transduce1` with a maximum fold, including an error on an empty stream. `transduce` is checked to start with the initial value and to stop early. Puzzle 1 is tested by comparing `fold(collect(5), ...)` with `take(..., 5)` on an infinite stream. `dimap`, `lmap`, and `rmap` are checked on a sum fold, `defer` and `resolve` by counting two streams in turn, and `tee` by running a count and a sum together.

### test-numeric.R

These tests compare the numerical functions with known answers. `euler_improve` is checked to be closer to pi/4 than the original series, and `integrate` on a short finite stream. The first-order solver is tested on y' = y against e, and the second-order solver on y'' = -y against cos(1). `merge` is checked on two sorted streams, and both products on (1 + z) times itself. `reciprocal` is tested on 1 - z and the product of the two is checked to be 1.

### test-statistics.R

Uniform draws are checked to stay in the interval and normal draws to have about the right mean. Rejection sampling from the density 2x on [0, 1] is checked to stay in the support with mean near 2/3. The MCMC sampler is checked against the exact posterior mean of the Normal-Normal model after burn-in, and the smoother on points from a straight line, which a local linear fit should reproduce exactly.

### test-effectful.R

`line_from` is checked to read every line and to leave no open connection afterwards. `grep_files` is checked to return the (file, line number, line) triple, and `grep_output` to print in the format set by its options. `wc` is checked on a file with known counts. The `socket` test starts the echo server in the background, sends one item, checks that the reply has the prefix and suffix, and stops the server.

### Results

All test has passed after running `devtools::test()`.
