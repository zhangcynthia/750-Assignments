make_stream <- function(xs) {
  list(items = xs, pos = 1)
}

is_active <- function(stream) {
  stream$pos <= length(stream$items)
}

next_elt <- function(stream) {
  if (!is_active(stream)) return(list(active = FALSE, result = NULL))
  list(
    active = TRUE,
    value = stream$items[[stream$pos]],
    rest = list(items = stream$items, pos = stream$pos + 1)
  )
}
