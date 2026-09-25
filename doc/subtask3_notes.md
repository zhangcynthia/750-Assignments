---
title: Subtask 3 Description
output: html_document
---

## Operations Implemented
The pipeline operations I implemented are :

- `df_select` and `df_exclude`: select or exclude columns by name
- `df_keep` and `df_remove`: filter rows by a condition
- `df_rename`: rename a column
- `df_rearrange`: reorder columns
- `df_reorder`: sort rows by a column's values
- `df_derive`: add a new column computed from existing ones
- `df_map`: apply a function element wise to a column
- `df_group_by_aggregate`: group rows and compute aggregate values per group

Each operation has a user-facing function in `R/ops.R` that records the operation into a pipeline. The corresponding internal `_exec` functions are in `R/pipeline.R`. The actual computation will only be executed when call the `df_execute` function also in `R/pipeline.R`.


## Testing
Tests are in `tests/testthat/test-pipeline.R`, with a small DataFrame (`carrier`, `delay`, and `distance` columns) used across tests.

Each operation has at least one test checking that it produces the correct output as expected. For instance, `select` returns only the requested columns, `keep` filters to the expected rows, `derive` computes the expected new column, `group_by_aggregate` computes the correct per-group average.

One test checks an error case, `select` on a nonexistent column name. One test verifies the immutability property, running several pipeline operations on the same DataFrame does not modify the original object.

All tests pass using `devtools::test()`.

