
# gibble -------------------------------------------------------------

#' @export
new_gibble <- function(data, groups) {
  # Compute groups in dplyr format
  if (is.numeric(groups)) {
    groups_vars <- names(data)[groups]
    grouped <- dplyr::grouped_df(data, groups_vars)
  } else {
    grouped <- dplyr::grouped_df(data, groups)
  }
  dplyr_groups <- attr(grouped, "groups")

  new_sticky(
    data,
    sticky = groups,
    groups = dplyr_groups,
    class = c("gibble_df", "grouped_df")
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
restore_sticky_names.gibble_df <- function(x, to, new) {
  groups <- names(x)[sticky_cols(to)]
  new_gibble(x, groups)
}
