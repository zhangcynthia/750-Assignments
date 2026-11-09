#' @importFrom rlang enquo env_parent
#' @importFrom utils head tail str
#' @importFrom S7 new_class class_any method method<- methods_register S7_inherits
NULL

#' @export
Stream <- new_class("Stream",
                    properties = list(
                      head = class_any,
                      tail = class_any
                    )
)

# The empty stream
#' @export
NULL
empty <- Stream()

# test if it is an empty stream
#' @export
is_empty <- function(s) {
  identical(s, empty)
}

# test if it is a stream
#' @export
is_stream <- function(s) {
  S7_inherits(s, Stream)
}

# construct a stream with head a and tail s
#' @export
cons <- function(a, s) {
  tail_quo <- enquo(s)
  Stream(head = a, tail = delay(!!tail_quo))
}

# force the evaluation of a delayed computation
#' @export
force <- function(delayed) {
  delayed()
}

method(head, Stream) <- function(x, ...) {
  x@head
}

method(tail, Stream) <- function(x, ...) {
  force(x@tail)
}

method(print, Stream) <- function(x, ...) {
  if (is_empty(x)) {
    cat("<empty stream>\n")
  } else {
    cat("<stream:", format(x@head), "...>\n")
  }
  invisible(x)
}

method(str, Stream) <- function(object, ...) {
  if (is_empty(object)) {
    cat(" <empty stream>\n")
  } else {
    cat(" <stream:", format(object@head), "...>\n")
  }
}

.onLoad <- function(libname, pkgname) {
  methods_register()
  registerS3method("compare_proxy", "streams::Stream", compare_proxy.stream,
                   envir = asNamespace("waldo"))
}

# integers n, n+1, ...
#' @export
integers_from <- function(n) {
  cons(n, integers_from(n + 1))
}

# stream that repeats a
#' @export
repeatedly <- function(a) {
  cons(a, repeatedly(a))
}

# stream a, f(a), f(f(a)), ...
#' @export
iterate <- function(f, a) {
  cons(a, iterate(f, f(a)))
}

# Construct a stream that repeatedly cycles through seq
#' @export
cycle <- function(seq) {
  n <- length(seq)
  cycle_from <- function(i) {
    cons(seq[[i]], cycle_from(i %% n + 1))
  }
  cycle_from(1)
}

# Build a stream from a sequence
#' @export
from_seq <- function(seq) {
  n <- length(seq)
  from_index <- function(i) {
    if (i > n) return(empty)
    cons(seq[[i]], from_index(i + 1))
  }
  from_index(1)
}

