test_that("optimize_pipeline pushes keep earlier", {
  df <- DataFrame(columns = list(c(1,2,3), c(10,20,30)), colnames = c("id", "delay"))
  p <- df |> df_select("id", "delay") |> df_derive("id_sq", id^2) |> df_keep(delay > 15)

  p_opt <- optimize_pipeline(p)

  ops_before <- sapply(p@steps, function(s) s$op)
  ops_after  <- sapply(p_opt@steps, function(s) s$op)

  expect_equal(ops_before, c("select", "derive", "keep"))
  expect_equal(ops_after,  c("keep", "select", "derive"))
})

test_that("optimize_pipeline does not move keep when shouldn't", {
  df <- DataFrame(columns = list(c(1,2,3), c(10,20,30)), colnames = c("id", "delay"))
  p <- df |> df_select("id") |> df_keep(delay > 15)

  expect_error(df |> df_select("id") |> df_keep(delay > 15) |> df_execute())

  p_opt <- optimize_pipeline(p)
  ops_after <- sapply(p_opt@steps, function(s) s$op)
  expect_equal(ops_after, c("select", "keep"))
})

test_that("optimization does not change the final result", {
  df <- DataFrame(
    columns = list(c(1,2,3,4), c(10,20,30,40)),
    colnames = c("id", "delay")
  )
  p <- df |> df_select("id", "delay") |> df_derive("id_sq", id^2) |> df_keep(delay > 15)

  result_before <- p |> df_execute()
  result_after  <- optimize_pipeline(p) |> df_execute()

  expect_equal(result_before@columns, result_after@columns)
  expect_equal(result_before@colnames, result_after@colnames)
})
