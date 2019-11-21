
# gibble -------------------------------------------------------------

#' @export
new_gibble <- function(data, groups) {
  structure(
    dplyr::grouped_df(data, groups),
    class = c("gibble_df", "sticky_df", "data.frame")
  )
}

#' @export
print.gibble_df <- function(x, ...) {
  cat("<gibble>\n")
  groups <- names(sticky_cols(x))
  print(dplyr::grouped_df(x, groups))
}


# Sticky -------------------------------------------------------------

#' @export
sticky_cols.gibble_df <- function(x, ...) {
  class(x) <- c("grouped_df", "data.frame")
  vars <- dplyr::group_vars(x)
  pos <- match(vars, names(x))
  set_names(pos, vars)
}

#' @export
restore_sticky_names.gibble_df <- function(x, to, new) {
  groups <- names(x)[sticky_cols(to)]
  new_gibble(x, groups)
}
