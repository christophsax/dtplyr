# library(dplyr)
# library(data.table)
# library(tidyr)

# # From http://stackoverflow.com/questions/1181060
# stocks <- data.frame(
#   time = as.Date('2009-01-01') + 0:9,
#   X = rnorm(10, 0, 1),
#   Y = rnorm(10, 0, 2),
#   Z = rnorm(10, 0, 4)
# )
# gather(stocks, stock, price, -time)
# gather(as.data.table(stocks), stock, price, -time)


#' @importFrom tidyr gather
gather.data.table <- function(data, key = "key", value = "value", ...,
                              na.rm = FALSE, convert = FALSE,
                              factor_key = FALSE) {

  stopifnot(inherits(data, "data.table"))

  key_var <- quo_name(enexpr(key))
  value_var <- quo_name(enexpr(value))

  quos <- quos(...)
  if (is_empty(quos)) {
    gather_vars <- setdiff(names(data), c(key_var, value_var))
  } else {
    gather_vars <- unname(tidyselect::vars_select(names(data), !!! quos))
  }
  if (is_empty(gather_vars)) {
    return(data)
  }

  gather_idx <- match(gather_vars, names(data))
  if (anyNA(gather_idx)) {
    missing_cols <- paste(gather_vars[is.na(gather_idx)], collapse = ", ")
    abort(glue("Unknown column names: ", missing_cols))
  }
  id_idx <- setdiff(seq_along(data), gather_idx)

  # ## Get the attributes if common, NULL if not.
  # args <- normalize_melt_arguments(data, gather_idx, factorsAsStrings = TRUE)
  # valueAsFactor <- "factor" %in% class(args$attr_template)

  out <- melt(data, 
       id.vars = names(data)[id_idx], 
       measure.vars =  names(data)[gather_idx],
       variable.name = as.character(key_var), 
       value.name = as.character(value_var), 
       na.rm = na.rm, 
       variable.factor = as.logical(factor_key), 
       value.factor = FALSE   # as.logical(valueAsFactor)
    )
  

  # out <- melt_dataframe(data,
  #   id_ind = id_idx - 1L,
  #   measure_ind = gather_idx - 1L,
  #   variable_name = as.character(key_var),
  #   value_name = as.character(value_var),
  #   attrTemplate = args$attr_template,
  #   factorsAsStrings = args$factorsAsStrings,     
  #   valueAsFactor = as.logical(valueAsFactor),
  #   variableAsFactor = as.logical(factor_key)
  # )

  # if (na.rm && anyNA(out)) {
  #   missing <- is.na(out[[value_var]])
  #   out <- out[!missing, ]
  # }

  if (convert) {
    out[[key_var]] <- type.convert(as.character(out[[key_var]]), as.is = TRUE)
  }

  tidyr:::reconstruct_tibble(data, out, gather_vars)
}



