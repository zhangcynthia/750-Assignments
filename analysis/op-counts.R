devtools::load_all()

# counting how many times mop is called
counter <- new.env()
counter$calls <- 0

CountedSum <- new_class(
  name = "CountedSum",
  parent = Group
)

method(munit, CountedSum) <- function(monoid) 0
method(mop, CountedSum) <- function(monoid, x, y) {
  counter$calls <- counter$calls + 1
  x + y
}
method(minv, CountedSum) <- function(group, x) -x

count_calls <- function(run) {
  counter$calls <- 0
  run()
  counter$calls
}

n <- 10000
input <- as.numeric(seq_len(n))

results <- do.call(rbind, lapply(c(2, 10, 100, 1000), function(w) {
  data.frame(
    w = w,
    swag = count_calls(function() swag(CountedSum(), w, input)),
    swag_stream = count_calls(function() swag_stream(CountedSum(), w, make_stream(input))),
    swag_prime = count_calls(function() swag_prime(basic(CountedSum()), w, make_stream(input))),
    swag_group = count_calls(function() swag_group(CountedSum(), w, make_stream(input)))
  )
}))

print(results)
