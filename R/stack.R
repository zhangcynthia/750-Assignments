empty_stack <- function() NULL

is_empty <- function(stack) is.null(stack)

push <- function(elt, stack) list(top = elt, rest = stack)

pop <- function(stack) {
  if (is_empty(stack)) stop("attempt to pop an empty stack")
  list(top = stack$top, rest = stack$rest)
}
