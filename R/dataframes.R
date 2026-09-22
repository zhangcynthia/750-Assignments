library(S7)

DataFrame <- new_class("DataFrame",
                       properties = list(
                         columns = class_list,
                         colnames = class_character
                         ),
                       validator = function(self){
                         if (length(self@columns) != length(self@colnames)) {
                           "columns and colnames need to have the same length"
                         }
                       }
)

# df <- DataFrame(
#   columns = list(
#     c(1, 2, 3),
#     c("a", "b", "c")
#   ),
#   colnames = c("id", "name")
# )


# method: dim
dims <- new_generic("dims", "df")
method(dims, DataFrame) <- function(df){
  ncol <- length(df@columns)
  nrow <- if (ncol == 0) 0 else length(df@columns[[1]])
  c(ncol, nrow)
}
# dims(df)

# method: colnames
df_colnames <- new_generic("df_colnames", "df")
method(df_colnames, DataFrame) <- function(df) {
  df@colnames
}
# df_colnames(df)

# method: col_idxs
col_idxs <- new_generic("col_idxs", "df")
method(col_idxs, DataFrame) <- function(df, name) {
  idx <- match(name, df@colnames)
  if (any(is.na(idx))) {
    stop("Couldn't find these column names: ", paste(name[is.na(idx)], collapse = ", "))
  }
  idx
}
# col_idxs(df, "id")

# method: col_name
col_name <- new_generic("col_name", "df")
method(col_name, DataFrame) <- function(df, idx) {
  df@colnames[idx]
}
# col_name(df, c(2,1))

# method: fill_col
fill_col <- function(value, len, type = NULL) {
  data <- rep(value, len)
  if (!is.null(type)) {
    data <- switch(type,
                   "numeric" = as.numeric(data),
                   "character" = as.character(data),
                   "boolean" = as.logical(data),
                   stop("Not supported type: ", type)
                   )
  }
  data
}
# fill_col(NA, 5, type = "numeric")
# fill_col("x", 3)

# method: validate
validate <- new_generic("validate", "df")
method(validate, DataFrame) <- function(df) {
  lens <- sapply(df@columns, length)
  if (length(unique(lens)) > 1) {
    stop("Not all columns have the same length")
  }
  TRUE
}
# validate(df)


# method: get_column
get_column <- new_generic("get_column", "df")
method(get_column, DataFrame) <- function(df, name) {
  idx <- col_idxs(df, name)
  df@columns[[idx]]
}

# method: set_column
set_column <- new_generic("set_column", "df")
method(set_column, DataFrame) <- function(df, name, data) {
  nrow_df <- dims(df)[2]
  if (length(data) == 1) {
    data <- rep(data, nrow_df)
  } else if (length(data) != nrow_df) {
    stop("The length of the column doesn't match to the row of dataframe")
  }
  
  new_columns <- df@columns
  new_colnames <- df@colnames
  
  if (name %in% df@colnames) {
    idx <- col_idxs(df, name)
    new_columns[[idx]] <- data
  } else {
    new_columns <- c(new_columns, list(data))
    new_colnames <- c(new_colnames, name)
  }
  
  DataFrame(columns = new_columns, colnames = new_colnames)
}

# method: get_value
get_value <- new_generic("get_value", "df")
method(get_value, DataFrame) <- function(df, col, row) {
  idx <- if (is.character(col)) {col_idxs(df, col)} else {col}
  df@columns[[idx]][row]
}

# method: set_value
set_value <- new_generic("set_value", "df")
method(set_value, DataFrame) <- function(df, col, row, value) {
  idx <- if (is.character(col)) {col_idxs(df, col)} else {col}
  
  new_columns <- df@columns
  new_columns[[idx]][row] <- value
  
  DataFrame(columns = new_columns, colnames = df@colnames)
}

# method: rename_column
rename_column <- new_generic("rename_column", "df")
method(rename_column, DataFrame) <- function(df, old_name, new_name) {
  idx <- col_idxs(df, old_name)
  new_colnames <- df@colnames
  new_colnames[idx] <- new_name
  DataFrame(columns = df@columns, colnames = new_colnames)
}

# method: change_column_type
change_column_type <- new_generic("change_column_type", "df")
method(change_column_type, DataFrame) <- function(df, name, type) {
  idx <- col_idxs(df, name)
  new_columns <- df@columns
  new_columns[[idx]] <- switch(type,
                               "numeric" = as.numeric(new_columns[[idx]]),
                               "character" = as.character(new_columns[[idx]]),
                               "boolean" = as.logical(new_columns[[idx]]),
                               stop("Do not support this type")
                               )
  DataFrame(columns = new_columns, colnames = df@colnames)
}

# method: delete_column
delete_column <- new_generic("delete_column", "df")
method(delete_column, DataFrame) <- function(df, name) {
  idx <- col_idxs(df, name)
  new_columns <- df@columns[-idx]
  new_colnames <- df@colnames[-idx]
  DataFrame(columns = new_columns, colnames = new_colnames)
}

# method: slice_rows
slice_rows <- new_generic("slice_rows", "df")
method(slice_rows, DataFrame) <- function(df, start, end) {
  new_columns <- lapply(df@columns, function(col) col[start:end])
  DataFrame(columns = new_columns, colnames = df@colnames)
}

# test
# df <- DataFrame(
#   columns = list(c(1, 2, 3), c("a", "b", "c")),
#   colnames = c("id", "name")
# )
# 
# get_column(df, "name")             # c("a", "b", "c")
# get_value(df, "id", 2)             # 2
# set_value(df, "id", 2, 99)@columns[[1]]   # c(1, 99, 3)
# rename_column(df, "id", "ID")@colnames    # c("ID", "name")
# delete_column(df, "name")@colnames        # "id"
# slice_rows(df, 2, 3)@columns[[1]]         # c(2, 3)
# set_column(df, "score", c(10,20,30))@colnames  # c("id","name","score")
# 
# df@columns[[1]]

library(readr)
read_dataframe_csv <- function(filepath, skip = 0, header = TRUE){
  raw <- readr::read_csv(
    filepath,
    skip = skip,
    col_names = header,
    col_types = readr::cols(.default = readr::col_character())
  )
  
  col_names <- colnames(raw)
  new_cols <- lapply(raw, function(col){parse_column(col)})
  DataFrame(columns = new_cols, colnames = col_names)
}

# test
# writeLines(
#   c("id,name,active",
#     "1,Alice,TRUE",
#     "2,Bob,FALSE",
#     "3,Carol,TRUE"),
#   "test.csv"
# )
# df <- read_dataframe_csv("test.csv")



