guess_type <- function(str_vector){
  non_na <- str_vector[!is.na(str_vector) & str_vector != ""]

  if (all(!is.na(suppressWarnings(as.numeric(non_na))))) {
    return("numeric")
  }

  if (all(non_na %in% c("TRUE", "FALSE", "T", "F", "true", "false"))) {
    return("boolean")
  }

  return("character")
}

parse_column <- function(str_vector, type = NULL) {
  if (is.null(type)) {
    type <- guess_type(str_vector)
  }

  switch(type,
         "numeric" = as.numeric(str_vector),
         "boolean" = as.logical(str_vector),
         "character" = as.character(str_vector),
         stop("Do not support the type")
         )
}

