# unexported stuff from dplyr

# sym_dollar <- quote(`$`)
# sym_brackets2 <- quote(`[[`)
# is_data_pronoun <- function(expr) {
#   is_lang(expr, list(sym_dollar, sym_brackets2)) &&
#     identical(node_cadr(expr), quote(.data))
# }
# tidy_text <- function(quo, width = 60L) {
#   expr <- f_rhs(quo)
#   if (is_data_pronoun(expr)) {
#     as_string(node_cadr(node_cdr(expr)))
#   } else {
#     quo_text(quo, width = width)
#   }
# }
# named_quos <- function(...) {
#   quos <- quos(...)
#   exprs_auto_name(quos, printer = tidy_text)
# }


# library(dtplyr)
# library(dplyr)  # why not depends?
# library(data.table) 
# mutate(as.data.table(iris), b = Sepal.Length * 2, Sepal.Length * 3)
# mutate2(as.data.table(iris), b = Sepal.Length * 2, Sepal.Length * 3)


# library(rlang)


# - should substitute dtplyr::mutate.data.table
# - remove mutate_.data.table and use dplyr::mutate_.data.frame

#' @export
mutate2 <- function(.data, ...) {

  # see above
  dots <- dplyr:::named_quos(...)

  names <- lapply(names(dots), as.name)

  for(i in seq_along(dots)) {
    # For each new variable, generate a call of the form df[, new := expr]
    j <- substitute(lhs := rhs, list(lhs = names[[i]], rhs = get_expr(dots[[i]])))
    .data <- dt_subset(.data, , j,  get_env(dots[[i]]))
  }

  # Need to use this syntax to make the output visible (#11).
  .data[]
}

