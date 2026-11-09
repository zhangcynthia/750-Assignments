# Stream of lines from a file
#' @export
line_from <- function(path) {
  state <- new.env()
  state$con <- file(path, open = "r")
  close_con <- function(e) {
    if (!is.null(e$con)) {
      close(e$con)
      e$con <- NULL
    }
  }
  reg.finalizer(state, close_con, onexit = TRUE)
  next_line <- function() {
    line <- tryCatch(
      readLines(state$con, n = 1, warn = FALSE),
      error = function(e) {
        close_con(state)
        stop(e)
      }
    )
    if (length(line) == 0) {
      close_con(state)
      return(empty)
    }
    cons(line, next_line())
  }
  next_line()
}

# Stream of matches (file, line number, line) for a pattern in files
#' @export
grep_files <- function(pattern, paths) {
  file_matches <- function(path) {
    numbered <- zip_with(
      function(n, text) list(file = path, line = n, text = text),
      integers_from(1),
      line_from(path)
    )
    keep(numbered, function(m) grepl(pattern, m$text))
  }
  from_files <- function(paths) {
    if (length(paths) == 0) return(empty)
    append_rest(file_matches(paths[1]), paths[-1])
  }
  append_rest <- function(s, rest) {
    if (is_empty(s)) return(from_files(rest))
    cons(head(s), append_rest(tail(s), rest))
  }
  from_files(paths)
}

# Print each item to the console
#' @export
display <- function(s) {
  if (is_empty(s)) return(empty)
  cat(head(s), "\n", sep = "")
  cons(head(s), display(tail(s)))
}

# Write each item to a file
#' @export
sink_file <- function(s, path) {
  file.create(path)
  write_from <- function(s) {
    if (is_empty(s)) return(empty)
    cat(head(s), "\n", sep = "", file = path, append = TRUE)
    cons(head(s), write_from(tail(s)))
  }
  write_from(s)
}

# grep with output options
#' @export
grep_output <- function(pattern, paths, show_file = TRUE, show_line = TRUE,
                        output = NULL) {
  format_match <- function(m) {
    parts <- c(if (show_file) m$file, if (show_line) m$line, m$text)
    paste(parts, collapse = ":")
  }
  lines <- map(format_match, grep_files(pattern, paths))
  shown <- if (is.null(output)) display(lines) else sink_file(lines, output)
  fold(drain(), shown)
}

# Fold counting lines
#' @export
count_lines <- function() {
  Fold(wrap = function(line) 1, step = `+`, init = 0, done = identity)
}

# Fold counting words
#' @export
count_words <- function() {
  Fold(
    wrap = function(line) length(regmatches(line, gregexpr("\\S+", line))[[1]]),
    step = `+`,
    init = 0,
    done = identity
  )
}

# Fold counting characters
#' @export
count_chars <- function() {
  Fold(wrap = function(line) nchar(line) + 1, step = `+`, init = 0, done = identity)
}

# Count lines, words, and characters in a file
#' @export
wc <- function(path) {
  counts <- tee(count_lines(), count_words(), count_chars())
  named <- rmap(function(r) c(lines = r[[1]], words = r[[2]], chars = r[[3]]), counts)
  fold(named, line_from(path))
}

# Send each item to an echo server and stream back its replies
#' @export
socket <- function(s, host = "127.0.0.1", port = 65432) {
  state <- new.env()
  state$con <- NULL
  close_con <- function(e) {
    if (!is.null(e$con)) {
      close(e$con)
      e$con <- NULL
    }
  }
  reg.finalizer(state, close_con, onexit = TRUE)
  send_from <- function(s) {
    if (is_empty(s)) {
      close_con(state)
      return(empty)
    }
    reply <- tryCatch({
      if (is.null(state$con)) {
        state$con <- socketConnection(host = host, port = port,
                                      blocking = TRUE, open = "r+")
      }
      writeLines(as.character(head(s)), state$con)
      readLines(state$con, n = 1)
    }, error = function(e) {
      close_con(state)
      stop(e)
    })
    cons(reply, send_from(tail(s)))
  }
  send_from(s)
}
