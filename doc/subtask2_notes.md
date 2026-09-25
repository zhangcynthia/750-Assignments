---
title: Subtask 2 Description
output: html_document
---

## Dataframe Design
`DataFrame` is implemented as a class (`R/dataframe.R`) with two properties:

- `columns`: a list of vectors holding the actual data
- `colnames`: a character vector giving the name of each column

Columns are R vectors with type-guessing and parsing logic (`guess_type` and `parse_column` function in `R/column.R`) handled independently of the DataFrame structure

In `R/dataframe.R`, I write a set of helper functions: `dims`, `df_colnames`, `col_idxs`, `col_name`, `fill_col`, and `validate`. The access functions are: `get_column` and `set_column` by name, `get_value` and `set_value` by column and row index, `rename_column`, `change_column_type`, `delete_column`, and `slice_rows`.

All modification functions I write, `set_column`, `set_value`, `rename_column`, `change_column_type`, `delete_column`, `slice_rows`, are immutable.

CSV files are read via `read_dataframe_csv` function in `R/dataframe.R`. The function loads all columns as character strings and then applies `parse_column` to each column individually to convert to the appropriate type.


## Testing
Tests are in `tests/testthat/test-dataframe.R`, with a small DataFrame (`id`, `name` columns) used across tests.

I applied at least one test for each core function: dataframe construction, `dims`, `df_colnames`, `col_idxs`, `col_name`, `fill_col`, `validate`, `get_column`, `set_column`, `get_value`, `set_value`, `rename_column`, `change_column_type`, `delete_column`, `slice_rows`, `guess_type`, `parse_column`, and `read_dataframe_csv`. 

Error cases are tested for `col_idxs` (invalid column name) and `validate` (mismatched column lengths).

Immutability is checked for `set_column` and `set_value`, confirming that the original dataframe is not changed after modification.

All tests pass using `devtools::test()`.
