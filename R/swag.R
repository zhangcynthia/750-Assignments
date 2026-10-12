flip <- function(agg, back) {
  front <- empty_stack()
  while (!is_empty(back)) {
    popped <- pop(back)
    x <- popped$top
    back <- popped$rest
    if (is_empty(front)) {
      front <- push(x, front)
    } else {
      front <- push(agg@op(x, front$top), front)
    }
  }
  front
}

swag <- function(monoid, wsize, input) {
  if (wsize < 1) stop("swag requires a positive window size")
  if (wsize == 1) return(input)
  agg <- basic(monoid)

  front <- empty_stack()
  back <- empty_stack()
  back_agg <- agg@init
  size <- 0
  output <- list()

  for (x in input) {
    back <- push(x, back)
    back_agg <- agg@op(back_agg, x)
    size <- size + 1

    if (size == wsize) {
      if (is_empty(front)) {
        front <- flip(agg, back)
        back <- empty_stack()
        back_agg <- agg@init
      }
      output <- c(output, list(agg@op(front$top, back_agg)))
      front <- pop(front)$rest
      size <- size - 1
    }
  }

  if (!is.atomic(input)) return(output)
  if (length(output) == 0) return(input[0])
  unlist(output)
}

swag_stream <- function(monoid, wsize, input) {
  if (wsize < 1) stop("swag requires a positive window size")
  if (wsize == 1) return(input)
  agg <- basic(monoid)

  front <- empty_stack()
  back <- empty_stack()
  back_agg <- agg@init
  size <- 0
  output <- list()

  step <- next_elt(input)
  while (step$active) {
    x <- step$value

    back <- push(x, back)
    back_agg <- agg@op(back_agg, x)
    size <- size + 1

    if (size == wsize) {
      if (is_empty(front)) {
        front <- flip(agg, back)
        back <- empty_stack()
        back_agg <- agg@init
      }
      output <- c(output, list(agg@op(front$top, back_agg)))
      front <- pop(front)$rest
      size <- size - 1
    }

    step <- next_elt(step$rest)
  }

  make_stream(output)
}

swag_prime <- function(agg, wsize, input) {
  if (wsize < 1) stop("swag requires a positive window size")

  front <- empty_stack()
  back <- empty_stack()
  back_agg <- agg@init
  size <- 0
  output <- list()

  step <- next_elt(input)
  while (step$active) {
    a <- agg@annotate(step$value)

    back <- push(a, back)
    back_agg <- agg@op(back_agg, a)
    size <- size + 1

    if (size == wsize) {
      if (is_empty(front)) {
        front <- flip(agg, back)
        back <- empty_stack()
        back_agg <- agg@init
      }
      output <- c(output, list(agg@finalize(agg@op(front$top, back_agg))))
      front <- pop(front)$rest
      size <- size - 1
    }

    step <- next_elt(step$rest)
  }

  make_stream(output)
}

swag_group <- function(group, wsize, input) {
  if (wsize < 1) stop("swag requires a positive window size")
  if (wsize == 1) return(input)
  agg <- basic(group)

  front <- empty_stack()
  back <- empty_stack()
  total <- agg@init
  size <- 0
  output <- list()

  step <- next_elt(input)
  while (step$active) {
    x <- step$value

    back <- push(x, back)
    total <- agg@op(total, x)
    size <- size + 1

    if (size == wsize) {
      output <- c(output, list(total))

      if (is_empty(front)) {
        while (!is_empty(back)) {
          popped <- pop(back)
          front <- push(popped$top, front)
          back <- popped$rest
        }
      }
      popped <- pop(front)
      total <- agg@op(minv(group, popped$top), total)
      front <- popped$rest
      size <- size - 1
    }

    step <- next_elt(step$rest)
  }

  make_stream(output)
}

