#' Select top (or bottom) n rows
#'
#' @description This function selects the top or bottom rows defined by an value (n) from a data frame.
#' @param x A data frame
#' @param n Number of rows to returns for 'top_n()'. If 'n' is positive, selects the top rows. If negative, selects the bottom rows.
#' @param wt (Optional) Variable used for ordering. If not specified, defaults to the last variable in the tbl.
#'
#' @examples
#' df <- data.frame(x = c(7, 10, 20, 13, 5, 22))
#' df %>% top_n(3)
#'
#'
#' @import rlang
#' @import glue
#' @import lifecycle
#' @export
top_n_terms <- function(x, n, wt) {
  lifecycle::signal_superseded("1.0.0", "top_n()", "slice_max()")
  wt <- enquo(wt)
  if (quo_is_missing(wt)) {
    vars <- tbl_vars(x)
    wt_name <- vars[length(vars)]
    inform(glue::glue("Selecting by ", wt_name))
    wt <- sym(wt_name)
  }

  filter(x, top_n_rank({{ n }}, !!wt))
}

top_n_rank <- function(n, wt) {
  if (n > 0) {
    min_rank(desc(wt)) <= n
  } else {
    min_rank(wt) <= abs(n)
  }
}
