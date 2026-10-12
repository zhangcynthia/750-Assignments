#' @import S7
NULL

Monoid <- new_class(
  name = "Monoid",
  abstract = TRUE
)

munit <- new_generic("munit", dispatch_args = "monoid") # monoid unit
mop <- new_generic("mop", dispatch_args = "monoid") # monoid operation

Group <- new_class(
  name = "Group",
  parent = Monoid,
  abstract = TRUE
)

minv <- new_generic("minv", dispatch_args = "group") # monoid inverse

Agg <- new_class(
  name = "Agg",
  properties = list(
    op = class_function,
    init = class_any,
    annotate = class_function,
    finalize = class_function
  )
)

basic <- function(monoid) {
  Agg(
    op = function(a1, a2) mop(monoid, a1, a2),
    init = munit(monoid),
    annotate = identity,
    finalize = identity
  )
}

annotated <- function(monoid, ann, fin) {
  Agg(
    op = function(a1, a2) mop(monoid, a1, a2),
    init = munit(monoid),
    annotate = ann,
    finalize = fin
  )
}

aggregate_window <- function(agg, values) {
  agg@finalize(Reduce(agg@op, lapply(values, agg@annotate), agg@init))
}

Sum <- new_class(
  name = "Sum",
  parent = Group
)

method(munit, Sum) <- function(monoid) 0
method(mop, Sum) <- function(monoid, x, y) x + y
method(minv, Sum)  <- function(group, x) -x


# Exercise 1 ----
# Average, annotate(x) = (x, 1), finalize((s, n)) = s/n
mean_agg <- function() {
  annotated(
    Sum(),
    ann = function(x) c(x, 1),
    fin = function(a) a[1] / a[2]
  )
}

# Standard Deviation, annotate(x) = (1, x, x^2), finalize uses n, sum x, sum x^2
sd_agg <- function() {
  annotated(
    Sum(),
    ann = function(x) c(1, x, x^2),
    fin = function(a) {
      n <- a[1]
      s1 <- a[2]
      s2 <- a[3]
      sqrt(max(0, (s2 - s1^2 / n) / (n - 1)))
    }
  )
}

# Linear regression, annotate(p) = (1, x, y, x^2, x y), p = (x,y), finalize solves the normal equations
regression_agg <- function() {
  annotated(
    Sum(),
    ann = function(p) c(1, p[1], p[2], p[1]^2, p[1] * p[2]),
    fin = function(a) {
      n <- a[1]
      sx <- a[2]
      sy <- a[3]
      sxx <- a[4]
      sxy <- a[5]
      slope <- (n * sxy - sx * sy) / (n * sxx - sx^2)
      intercept <- (sy - slope * sx) / n
      c(intercept = intercept, slope = slope)
    }
  )
}

# String concatenation: unit "", operation paste0
Concat <- new_class(
  name = "Concat",
  parent = Monoid
)

method(munit, Concat) <- function(monoid) ""
method(mop, Concat) <- function(monoid, x, y) paste0(x, y)

# Dictionary merge: unit list(), on a shared key the value from the second argument wins
DictMerge <- new_class(
  name = "DictMerge",
  parent = Monoid
)

method(munit, DictMerge) <- function(monoid) list()
method(mop, DictMerge) <- function(monoid, x, y) {
  x[names(y)] <- y
  x
}

