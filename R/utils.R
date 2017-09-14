
dt_subset <- function(dt, i, j, env = caller_env(), sd_cols = NULL) {

  # # print(env)

  # if (missing(j)) {
  #   expr <- expr(dt[!! maybe_missing(i)])
  # } else {
  #   expr <- expr(dt[
  #     !! maybe_missing(i),
  #     !! j,
  #     by = quote(list(!!! groups(dt)))
  #   ])
  #   expr$.SDcols <- sd_cols
  # }
  
  # expr <- dt_replace2(expr)
  # expr <- quo_expr(expr, warn = TRUE)


  # print(expr)
  # return(eval_bare(expr, env))


  env <- new.env(parent = env, size = 2L)
  env$`_dt` <- dt
  env$`_vars` <- deparse_all(groups(dt))




  args <- list(
    i = if (missing(i)) quote(expr =) else dt_replace(i),
    j = if (missing(j)) quote(expr =) else dt_replace(j)
  )

  if (missing(j)) {
    call <- substitute(`_dt`[i], args)
  } else {
    call <- substitute(`_dt`[i, j, by = `_vars`], args)
    call$.SDcols = sd_cols
  }
  print(call)

  eval(call, env)
}


dt_replace2 <- function(x) {
  dplyr:::expr_substitute(x,
    . ~ .SD,
    n() ~ .N
  )
}

maybe_missing <- function(x) {
  if (is_missing(x) || quo_is_missing(x)) {
    missing_arg()
  } else {
    x
  }
}


dt_replace <- function(x) {
  if (is.atomic(x)) {
    x
  } else if (is.name(x)) {
    if (identical(x, quote(.))) {
      quote(.SD)
    } else {
      x
    }
  } else if (is.call(x)) {
    if (identical(x, quote(n()))) {
      quote(.N)
    } else {
      x[] <- lapply(x, dt_replace)
      x
    }
  } else {
    # User supplied incorrect input
    stop("Don't know how to handle type ", typeof(x),
      call. = FALSE)
  }
}

commas <- function(...) paste0(..., collapse = ", ")

deparse_all <- function(x) {
  deparse2 <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")
  vapply(x, deparse2, FUN.VALUE = character(1))
}


drop_last <- function(x) {
  if (length(x) <= 1L) return(NULL)
  x[-length(x)]
}


names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

"%||%" <- function(x, y) if(is.null(x)) y else x

