#' @importFrom tidyr spread
spread.data.table <- function(data, key, value, fill = NA, convert = FALSE,
                              drop = TRUE, sep = NULL) {
  key_var <- tidyselect::vars_pull(names(data), !! enquo(key))
  value_var <- tidyselect::vars_pull(names(data), !! enquo(value))

  # setdiff(names(data), 

  formula <- new_formula(quote(...), sym(key_var))
  out <- dcast(data, formula, value.var = value_var, fill = fill, drop = drop, sep = sep)

  if (convert) {
    abort("not yet done")
    # ordered[] <- map(ordered, type.convert, as.is = TRUE)
  }
  
  out
  # out <- append_df(row_labels, ordered)
  # reconstruct_tibble(data, out, c(key_var, value_var))
}

