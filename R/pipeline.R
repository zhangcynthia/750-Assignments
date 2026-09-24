# library(S7)
Pipeline <- new_class("Pipeline",
                      properties = list(
                        df = DataFrame,
                        steps = class_list
                      )
                      )

# df <- DataFrame(
#   columns = list(c(1, 2, 3), c("a", "b", "c")),
#   colnames = c("id", "name")
# )
# p <- Pipeline(df = df, steps = list())
# p@df
# p@steps

add_step <- function(x, step) {
  if (S7_inherits(x, DataFrame)) {  # if x is a dataframe
    Pipeline(df = x, steps = list(step))
  } else if (S7_inherits(x, Pipeline)) {  # if x is a pipeline
    Pipeline(df = x@df, steps = c(x@steps, list(step)))
  } else {
    stop("the first parameter of add_step must be dataframe or pipeline")
  }
}

# test
# df <- DataFrame(
#   columns = list(c(1, 2, 3), c("a", "b", "c")),
#   colnames = c("id", "name")
# )
# p1 <- add_step(df, list(op = "select", cols = c("id")))
# p1@df
# p1@steps
# p2 <- add_step(p1, list(op = "keep", condition = "id > 1"))
# p2@df
# p2@steps

# Execution
df_execute <- function(pipeline) {
  result <- pipeline@df
  for (step in pipeline@steps) {
    result <- exec_step(result, step)
  }
  result
}

exec_step <- function(df, step) {
  switch(step$op,
         "select" = select_exec(df, step$cols),
         "exclude" = exclude_exec(df, step$cols),
         "keep" = keep_exec(df, step$condition),
         "remove" = remove_exec(df, step$condition),
         "rename" = rename_exec(df, step$old_name, step$new_name),
         "derive" = derive_exec(df, step$name, step$expr),
         "map" = map_exec(df, step$col, step$fn),
         "rearrange" = rearrange_exec(df, step$cols),
         "reorder" = reorder_exec(df, step$col, step$descending),
         "group_by_aggregate" = group_by_aggregate_exec(df, step$group_cols, step$aggs),
         stop("Unknown operator")
  )
}

select_exec <- function(df, cols) {
  new_columns <- lapply(cols, function(name){get_column(df, name)})
  DataFrame(columns = new_columns, colnames = cols)
}

exclude_exec <- function(df, cols) {
  keep_cols <- setdiff(df@colnames, cols)
  select_exec(df, keep_cols)
}

keep_exec <- function(df, condition_quo) {
  data_env <- setNames(df@columns, df@colnames)
  mask <- rlang::eval_tidy(condition_quo, data = data_env)
  matching_rows <- which(mask)
  new_columns <- lapply(df@columns, function(col) col[matching_rows])
  DataFrame(columns = new_columns, colnames = df@colnames)
}

remove_exec <- function(df, condition_quo) {
  data_env <- setNames(df@columns, df@colnames)
  mask <- rlang::eval_tidy(condition_quo, data = data_env)
  matching_rows <- which(!mask)
  new_columns <- lapply(df@columns, function(col) col[matching_rows])
  DataFrame(columns = new_columns, colnames = df@colnames)
}

rename_exec <- function(df, old_name, new_name) {
  rename_column(df, old_name, new_name)
}

derive_exec <- function(df, name, expr_quo) {
  data_env <- setNames(df@columns, df@colnames)
  new_col_data <- rlang::eval_tidy(expr_quo, data = data_env)
  new_columns <- c(df@columns, list(new_col_data))
  new_colnames <- c(df@colnames, name)
  DataFrame(columns = new_columns, colnames = new_colnames)
}

map_exec <- function(df, col, fn) {
  idx <- col_idxs(df, col)
  new_columns <- df@columns
  new_columns[[idx]] <- sapply(new_columns[[idx]], fn)
  DataFrame(columns = new_columns, colnames = df@colnames)
}

rearrange_exec <- function(df, cols) {
  new_columns <- lapply(cols, function(name) get_column(df, name))
  DataFrame(columns = new_columns, colnames = cols)
}

reorder_exec <- function(df, col, descending = FALSE) {
  target_col <- get_column(df, col)
  order_idx <- order(target_col, decreasing = descending)
  new_columns <- lapply(df@columns, function(c) c[order_idx])
  DataFrame(columns = new_columns, colnames = df@colnames)
}

group_by_aggregate_exec <- function(df, group_cols, aggs) {
  idxs <- col_idxs(df, group_cols)
  group_col_data <- df@columns[idxs]

  keys <- do.call(paste, c(group_col_data, sep = "_"))
  groups <- split(seq_along(keys), keys)

  result_columns <- list()

  for (gc in group_cols) {
    col_data <- get_column(df, gc)
    result_columns[[gc]] <- unname(sapply(groups, function(rows) col_data[rows[1]]))
  }

  for (agg_name in names(aggs)) {
    fn <- aggs[[agg_name]]
    result_columns[[agg_name]] <- unname(sapply(groups, function(rows) {
      sub_columns <- lapply(df@columns, function(col) col[rows])
      sub_df <- DataFrame(columns = sub_columns, colnames = df@colnames)
      fn(sub_df)
    }))
  }

  DataFrame(columns = unname(result_columns), colnames = names(result_columns))
}
