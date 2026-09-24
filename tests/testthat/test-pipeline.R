make_test_df <- function() {
  DataFrame(
    columns = list(
      c("AAL", "AAL", "DAL", "DAL", "DAL"),
      c(10, 20, 30, 5, 60),
      c(100, 150, 200, 120, 300)
    ),
    colnames = c("carrier", "delay", "distance")
  )
}

# ================== select ==================
test_that("select keeps only the specified columns", {
  df <- make_test_df()
  result <- df |> df_select("carrier", "delay") |> df_execute()
  expect_equal(result@colnames, c("carrier", "delay"))
  expect_equal(length(result@columns), 2)
  expect_equal(result@columns[[2]], c(10, 20, 30, 5, 60))
})

test_that("select with a nonexistent column name errors", {
  df <- make_test_df()
  expect_error(df |> df_select("nonexistent") |> df_execute())
})

# ================== exclude ==================
test_that("exclude removes only the specified columns", {
  df <- make_test_df()
  result <- df |> df_exclude("distance") |> df_execute()
  expect_equal(result@colnames, c("carrier", "delay"))
})

# ================== keep ==================
test_that("keep filters rows matching the condition", {
  df <- make_test_df()
  result <- df |> df_keep(delay > 15) |> df_execute()
  expect_equal(result@columns[[2]], c(20, 30, 60))
})

# ================== remove ==================
test_that("remove is the complement of keep", {
  df <- make_test_df()
  kept <- df |> df_keep(delay > 15) |> df_execute()
  removed <- df |> df_remove(delay > 15) |> df_execute()
  expect_equal(length(kept@columns[[1]]) + length(removed@columns[[1]]), 5)
})

# ================== rename ==================
test_that("rename changes the column name but not the data", {
  df <- make_test_df()
  result <- df |> df_rename("delay", "delay_minutes") |> df_execute()
  expect_equal(result@colnames, c("carrier", "delay_minutes", "distance"))
  expect_equal(result@columns[[2]], df@columns[[2]])
})

# ================== rearrange ==================
test_that("rearrange reorders columns without changing their data", {
  df <- make_test_df()
  result <- df |> df_rearrange("distance", "carrier", "delay") |> df_execute()
  expect_equal(result@colnames, c("distance", "carrier", "delay"))
})

# ================== reorder ==================
test_that("reorder keeps rows aligned across columns", {
  df <- make_test_df()
  result <- df |> df_reorder("delay") |> df_execute()
  idx <- which(result@columns[[2]] == 5)
  expect_equal(result@columns[[1]][idx], "DAL")
})

# ================== derive ==================
test_that("derive adds a new column computed from existing ones", {
  df <- make_test_df()
  result <- df |> df_derive("speed", distance / delay) |> df_execute()
  expect_equal(result@colnames, c("carrier", "delay", "distance", "speed"))
  expect_equal(result@columns[[4]], df@columns[[3]] / df@columns[[2]])
})

# ================== map ==================
test_that("map applies a function elementwise to a column", {
  df <- make_test_df()
  result <- df |> df_map("delay", function(x) x * 2) |> df_execute()
  expect_equal(result@columns[[2]], df@columns[[2]] * 2)
})

# ================== group_by_aggregate ==================
test_that("group_by_aggregate computes correct group averages", {
  df <- make_test_df()
  result <- df |>
    df_group_by_aggregate("carrier",
                          avg_delay = function(sub_df) mean(get_column(sub_df, "delay"))) |>
    df_execute()

  expect_equal(result@colnames, c("carrier", "avg_delay"))
  aal_idx <- which(result@columns[[1]] == "AAL")
  dal_idx <- which(result@columns[[1]] == "DAL")
  expect_equal(result@columns[[2]][aal_idx], 15)
  expect_equal(result@columns[[2]][dal_idx], mean(c(30, 5, 60)))
})

test_that("pipeline execution never mutates the original dataframe", {
  df <- make_test_df()
  original <- df@columns

  df |> df_select("carrier") |> df_execute()
  df |> df_keep(delay > 10) |> df_execute()
  df |> df_derive("x", delay + 1) |> df_execute()
  df |> df_group_by_aggregate("carrier", n = function(sub_df) dims(sub_df)[2]) |> df_execute()

  expect_equal(df@columns, original)
})
