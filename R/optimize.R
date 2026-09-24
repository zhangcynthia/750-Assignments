# Optimization I choose: Predicate Pushdown

# extract column names in keep/remove conditions
get_condition_vars <- function(condition) {
  expr <- rlang::quo_get_expr(condition)
  all.vars(expr)
}

# q <- rlang::quo(delay > 15)
# get_condition_vars(q)

can_move <- function(filter_step, other_step) {
  required_cols <- get_condition_vars(filter_step$condition)
  switch(other_step$op,
         "select" = all(required_cols %in% other_step$cols), # can move when keep columns include in select column
         "exclude" = !any(required_cols %in% other_step$cols), # can move when keep columns not include in remove column
         "derive" = !(other_step$name %in% required_cols), # can move when keep columns not the same as new columns
         "map" = !(other_step$col %in% required_cols), # can move when keep columns not the same as map columns
         "keep" = TRUE, # always can move
         "remove" = TRUE, # always can move
         FALSE
         )
}

optimize_pipeline <- function(pipeline) {
  steps <- pipeline@steps
  changed <- TRUE

  while (changed) {
    changed <- FALSE
    for (i in seq_along(steps)) {
      if (i == 1) next    # already the first step
      current <- steps[[i]]
      if (current$op %in% c("keep", "remove")) {
        prev <- steps[[i - 1]]
        if (can_move(current, prev)) {
          steps[[i - 1]] <- current
          steps[[i]] <- prev
          changed <- TRUE
        }
      }
    }
  }
  Pipeline(df = pipeline@df, steps = steps)
}

# print out the order of pipelines
describe_pipeline <- function(pipeline) {
  sapply(pipeline@steps, function(step) step$op)
}

# test
# df <- DataFrame(
#   columns = list(c(1, 2, 3), c(10, 20, 30)),
#   colnames = c("id", "delay")
# )
#
# p <- df |> df_select("id", "delay") |> df_derive("id_sq", id^2) |> df_keep(delay > 15)
# describe_pipeline(p)
# p_opt <- optimize_pipeline(p)
# describe_pipeline(p_opt)

