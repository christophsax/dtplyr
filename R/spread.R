#' @export
spread.data.table <- function(data, key, value, fill = NA, convert = FALSE,
                              drop = TRUE, sep = NULL) {
  key_var <- tidyselect::vars_pull(names(data), !! enquo(key))
  value_var <- tidyselect::vars_pull(names(data), !! enquo(value))

  formula <- as.formula(paste("... ~", key_var))
  out <- dcast(data, formula, value.var = value_var)

  # col <- data[key_var]
  # col_id <- id(col, drop = drop)
  # col_labels <- split_labels(col, col_id, drop = drop)

  # rows <- data[setdiff(names(data), c(key_var, value_var))]
  # if (length(rows) == 0) {
  #   # Special case when there's only one row
  #   row_id <- structure(1L, n = 1L)
  #   row_labels <- as.data.frame(matrix(nrow = 1, ncol = 0))
  # } else {
  #   row_id <- id(rows, drop = drop)
  #   row_labels <- split_labels(rows, row_id, drop = drop)
  #   rownames(row_labels) <- NULL
  # }

  # overall <- id(list(col_id, row_id), drop = FALSE)
  # n <- attr(overall, "n")
  # # Check that each output value occurs in unique location
  # if (anyDuplicated(overall)) {
  #   groups <- split(seq_along(overall), overall)
  #   groups <- groups[map_int(groups, length) > 1]

  #   str <- map_chr(groups, function(x) paste0("(", paste0(x, collapse = ", "), ")"))
  #   rows <- paste(str, collapse = ", ")
  #   abort(glue("Duplicate identifiers for rows {rows}"))
  # }
  # # browser()

  # # Add in missing values, if necessary
  # if (length(overall) < n) {
  #   overall <- match(seq_len(n), overall, nomatch = NA)
  # } else {
  #   overall <- order(overall)
  # }

  # value <- data[[value_var]]
  # ordered <- value[overall]
  # if (!is.na(fill)) {
  #   ordered[is.na(ordered)] <- fill
  # }

  # if (convert && !is_character(ordered)) {
  #   ordered <- as.character(ordered)
  # }
  # dim(ordered) <- c(attr(row_id, "n"), attr(col_id, "n"))
  # colnames(ordered) <- enc2utf8(col_names(col_labels, sep = sep))


  # # browser()

  # ordered <- as_tibble_matrix(ordered)

  if (convert) {
    abort("not yet done")
    # ordered[] <- map(ordered, type.convert, as.is = TRUE)
  }
  
  out
  # out <- append_df(row_labels, ordered)
  reconstruct_tibble(data, out, c(key_var, value_var))
}

