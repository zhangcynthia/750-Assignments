# The vector with the first n items of s
#' @export
take <- function(s, n) {
  out <- list()
  i <- 0
  while (i < n && !is_empty(s)) {
    out[[i + 1]] <- head(s)
    s <- tail(s)
    i <- i + 1
  }
  if (all(vapply(out, function(x) is.atomic(x) && length(x) == 1, logical(1)))) {
    unlist(out)
  } else {
    out
  }
}

# The stream s with the first n items dropped
#' @export
drop_n <- function(s, n) {
  i <- 0
  while (i < n && !is_empty(s)) {
    s <- tail(s)
    i <- i + 1
  }
  s
}

# Returns the nth element of s, counting from 0
#' @export
at <- function(s, n) {
  head(drop_n(s, n))
}

# Returns a pair take(s, n), drop_n(s, n)
#' @export
split_at <- function(s, n) {
  list(take(s, n), drop_n(s, n))
}

# A stream that maps elements of s with f
#' @export
map <- function(f, s) {
  if (is_empty(s)) return(empty)
  cons(f(head(s)), map(f, tail(s)))
}

# A stream keeping elements of s that satisfy pred
#' @export
keep <- function(s, pred) {
  while (!is_empty(s) && !pred(head(s))) {
    s <- tail(s)
  }
  if (is_empty(s)) return(empty)
  cons(head(s), keep(tail(s), pred))
}

# A stream removing elements of s that satisfy pred
#' @export
remove_if <- function(s, pred) {
  keep(s, Negate(pred))
}

# A stream that maps elements of s where f returns a value
#' @export
map_maybe <- function(f, s) {
  while (!is_empty(s) && is.null(f(head(s)))) {
    s <- tail(s)
  }
  if (is_empty(s)) return(empty)
  cons(f(head(s)), map_maybe(f, tail(s)))
}

# A stream with items from s1 where the corresponding item of s2 is truthy
#' @export
mask <- function(s1, s2) {
  while (!is_empty(s1) && !is_empty(s2) && !isTRUE(as.logical(head(s2)))) {
    s1 <- tail(s1)
    s2 <- tail(s2)
  }
  if (is_empty(s1) || is_empty(s2)) return(empty)
  cons(head(s1), mask(tail(s1), tail(s2)))
}

# map for more than one stream, length of shortest input stream
#' @export
zip_with <- function(f, ...) {
  streams <- list(...)
  if (any(vapply(streams, is_empty, logical(1)))) return(empty)
  heads <- lapply(streams, head)
  cons(do.call(f, heads), do.call(zip_with, c(list(f), lapply(streams, tail))))
}

# Initial items from s as long as pred is true
#' @export
take_while <- function(s, pred) {
  n <- 0
  rest <- s
  while (!is_empty(rest) && pred(head(rest))) {
    rest <- tail(rest)
    n <- n + 1
  }
  take(s, n)
}

# Items from s until the first item with pred true
#' @export
take_until <- function(s, pred) {
  take_while(s, Negate(pred))
}

# The stream s with initial items dropped as long as pred is true
#' @export
drop_while <- function(s, pred) {
  while (!is_empty(s) && pred(head(s))) {
    s <- tail(s)
  }
  s
}

# The stream s with items dropped until the first item with pred true
#' @export
drop_until <- function(s, pred) {
  drop_while(s, Negate(pred))
}

# First s1 then s2 then ...
#' @export
chain <- function(...) {
  chain_list <- function(streams) {
    while (length(streams) > 0 && is_empty(streams[[1]])) {
      streams <- streams[-1]
    }
    if (length(streams) == 0) return(empty)
    first <- streams[[1]]
    cons(head(first), chain_list(c(list(tail(first)), streams[-1])))
  }
  chain_list(list(...))
}

# Alternate values from given streams in order, s1 first
#' @export
interleave <- function(...) {
  interleave_list <- function(streams) {
    streams <- Filter(Negate(is_empty), streams)
    if (length(streams) == 0) return(empty)
    first <- streams[[1]]
    cons(head(first), interleave_list(c(streams[-1], list(tail(first)))))
  }
  interleave_list(list(...))
}

method(cumsum, Stream) <- function(x) {
  cumsum_from <- function(s, total) {
    if (is_empty(s)) return(empty)
    total <- total + head(s)
    cons(total, cumsum_from(tail(s), total))
  }
  cumsum_from(x, 0)
}

method(diff, Stream) <- function(x, ...) {
  if (is_empty(x) || is_empty(tail(x))) return(empty)
  cons(head(tail(x)) - head(x), diff(tail(x)))
}

# Stream s at indices n0 to n1 by dn
#' @export
slice <- function(s, n0 = 0, n1 = Inf, dn = 1) {
  slice_from <- function(s, i) {
    if (i >= n1 || is_empty(s)) return(empty)
    cons(head(s), slice_from(drop_n(s, dn), i + dn))
  }
  slice_from(drop_n(s, n0), n0)
}

# Stream of chunks of length n, starting every m items
#' @export
chunked <- function(s, n, m = n) {
  if (is_empty(s)) return(empty)
  cons(take(s, n), chunked(drop_n(s, m), n, m))
}

# Stream of streams from s grouped by values of f
#' @export
group_by <- function(s, f) {
  if (is_empty(s)) return(empty)
  key <- f(head(s))
  items <- list()
  while (!is_empty(s) && identical(f(head(s)), key)) {
    items[[length(items) + 1]] <- head(s)
    s <- tail(s)
  }
  cons(from_seq(items), group_by(s, f))
}
