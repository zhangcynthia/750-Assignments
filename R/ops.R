# library(S7)

df_reorder <- function(x, col, descending = FALSE) {
  step <- list(op = "reorder", col = col, descending = descending)
  add_step(x, step)
}

df_rename <- function(x, old_name, new_name) {
  step <- list(op = "rename", old_name = old_name, new_name = new_name)
  add_step(x, step)
}

df_rearrange <- function(x, ...) {
  cols <- c(...)
  step <- list(op = "rearrange", cols = cols)
  add_step(x, step)
}

df_select <- function(x, ...) {
  cols <- c(...)
  step <- list(op = "select", cols = cols)
  add_step(x, step)
}

# df <- DataFrame(
#   columns = list(c(1, 2, 3), c("a", "b", "c")),
#   colnames = c("id", "name")
# )
#
# p1 <- df_select(df, "id")
# p1@steps[[1]]$op
# p1@steps[[1]]$cols

df_exclude <- function(x, ...) {
  cols <- c(...)
  step <- list(op = "exclude", cols = cols)
  add_step(x, step)
}

df_keep <- function(x, condition) {
  condition_quo <- rlang::enquo(condition)
  step <- list(op = "keep", condition = condition_quo)
  add_step(x, step)
}

df_remove <- function(x, condition) {
  condition_quo <- rlang::enquo(condition)
  step <- list(op = "remove", condition = condition_quo)
  add_step(x, step)
}


df_map <- function(x, col, fn) {
  step <- list(op = "map", col = col, fn = fn)
  add_step(x, step)
}

df_group_by_aggregate <- function(x, group_cols, ...) {
  aggs <- list(...)
  step <- list(op = "group_by_aggregate", group_cols = group_cols, aggs = aggs)
  add_step(x, step)
}

df_derive <- function(x, name, expr) {
  expr_quo <- rlang::enquo(expr)
  step <- list(op = "derive", name = name, expr = expr_quo)
  add_step(x, step)
}
