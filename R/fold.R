# Fold
#' @export
Fold <- new_class("Fold",
                  properties = list(
                    wrap = class_function,
                    step = class_function,
                    init = class_any,
                    done = class_function
                  )
)

# Fold1
#' @export
Fold1 <- new_class("Fold1",
                   properties = list(
                     wrap = class_function,
                     step = class_function,
                     done = class_function
                   )
)

Reduced <- new_class("Reduced",
                     properties = list(value = class_any)
)

# Signal early termination from a step function
#' @export
reduced <- function(x) {
  Reduced(value = x)
}

# Test whether a step returned reduced()
#' @export
is_reduced <- function(x) {
  S7_inherits(x, Reduced)
}

# Aggregate a stream with a Fold
#' @export
fold <- function(fld, s) {
  acc <- fld@init
  if (is_reduced(acc)) return(fld@done(acc@value))
  while (!is_empty(s)) {
    acc <- fld@step(acc, fld@wrap(head(s)))
    if (is_reduced(acc)) {
      acc <- acc@value
      break
    }
    s <- tail(s)
  }
  fld@done(acc)
}

# Transduce a stream with a Fold
#' @export
transduce <- function(fld, s) {
  scan_from <- function(acc, s) {
    cons(fld@done(acc), scan_next(acc, s))
  }
  scan_next <- function(acc, s) {
    if (is_empty(s)) return(empty)
    acc <- fld@step(acc, fld@wrap(head(s)))
    if (is_reduced(acc)) return(cons(fld@done(acc@value), empty))
    scan_from(acc, tail(s))
  }
  if (is_reduced(fld@init)) return(cons(fld@done(fld@init@value), empty))
  scan_from(fld@init, s)
}

# Aggregate a non-empty stream with a Fold1
#' @export
fold1 <- function(fld, s) {
  if (is_empty(s)) stop("fold1 needs a non-empty stream", call. = FALSE)
  acc <- fld@wrap(head(s))
  s <- tail(s)
  while (!is_empty(s)) {
    acc <- fld@step(acc, fld@wrap(head(s)))
    if (is_reduced(acc)) {
      acc <- acc@value
      break
    }
    s <- tail(s)
  }
  fld@done(acc)
}

# Transduce a stream with a Fold1
#' @export
transduce1 <- function(fld, s) {
  scan_from <- function(acc, s) {
    cons(fld@done(acc), scan_next(acc, s))
  }
  scan_next <- function(acc, s) {
    if (is_empty(s)) return(empty)
    acc <- fld@step(acc, fld@wrap(head(s)))
    if (is_reduced(acc)) return(cons(fld@done(acc@value), empty))
    scan_from(acc, tail(s))
  }
  if (is_empty(s)) return(empty)
  scan_from(fld@wrap(head(s)), tail(s))
}

# Pre-compose wrap with f and post-compose done with g
#' @export
dimap <- function(f, g, fld) {
  wrap <- function(x) fld@wrap(f(x))
  done <- function(acc) g(fld@done(acc))
  if (S7_inherits(fld, Fold1)) {
    Fold1(wrap = wrap, step = fld@step, done = done)
  } else {
    Fold(wrap = wrap, step = fld@step, init = fld@init, done = done)
  }
}

# dimap with g = identity
#' @export
lmap <- function(f, fld) {
  dimap(f, identity, fld)
}

# dimap with f = identity
#' @export
rmap <- function(g, fld) {
  dimap(identity, g, fld)
}

# Replace done so that running the fold returns a paused Fold
#' @export
defer <- function(fld) {
  wrap <- fld@wrap
  step <- fld@step
  done <- fld@done
  resume <- function(acc) Fold(wrap = wrap, step = step, init = acc, done = done)
  if (S7_inherits(fld, Fold1)) {
    Fold1(wrap = wrap, step = step, done = resume)
  } else {
    Fold(wrap = wrap, step = step, init = fld@init, done = resume)
  }
}

# Extract the final result from a paused Fold
#' @export
resolve <- function(fld) {
  fld@done(fld@init)
}

# Run several folds in parallel
#' @export
tee <- function(...) {
  flds <- list(...)
  k <- seq_along(flds)
  wrap <- function(x) {
    lapply(flds, function(f) f@wrap(x))
  }
  step <- function(acc, w) {
    lapply(k, function(i) flds[[i]]@step(acc[[i]], w[[i]]))
  }
  done <- function(acc) {
    lapply(k, function(i) flds[[i]]@done(acc[[i]]))
  }
  if (all(vapply(flds, S7_inherits, logical(1), class = Fold1))) {
    Fold1(wrap = wrap, step = step, done = done)
  } else {
    Fold(wrap = wrap, step = step, init = lapply(flds, function(f) f@init), done = done)
  }
}

# Fold that collects the first n items into a vector
#' @export
collect <- function(n) {
  step <- function(acc, x) {
    acc <- c(acc, x)
    if (length(acc) >= n) reduced(acc) else acc
  }
  Fold(wrap = identity, step = step, init = c(), done = identity)
}

# Fold that counts items
#' @export
count <- function() {
  Fold(
    wrap = identity,
    step = function(acc, x) acc + 1,
    init = 0,
    done = identity
  )
}

# Fold that collects all items into a list
#' @export
to_list <- function() {
  Fold(
    wrap = identity,
    step = function(acc, x) c(acc, list(x)),
    init = list(),
    done = identity
  )
}

# Fold that absorbs all items and produces a trivial value
#' @export
drain <- function() {
  Fold(
    wrap = identity,
    step = function(acc, x) acc,
    init = NULL,
    done = function(acc) NULL
  )
}
