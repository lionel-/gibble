
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

  out <- new_sticky(
    data,
    sticky = groups,
    groups = dplyr_groups,
    class = "gibble_df"
  )

  # As a workaround, insert `grouped_df` after `sticky_df` so dplyr
  # methods do not take over. That shouldn't be necessary, but we are
  # kind of merging two classes together for experimentation purposes.
  class <- class(out)
  class <- append(
    class,
    "grouped_df",
    after = match("data.frame", class) - 1L
  )
  class(out) <- class

  out
}

#' @export
print.gibble_df <- function(x, ...) {
  cat("<gibble>\n")
  groups <- names(sticky_pos(x))
  print(dplyr::grouped_df(x, groups))
}


# vctrs --------------------------------------------------------------

#' @export
vec_restore.gibble_df <- function(x, to, ...) {
  new_gibble(x, groups = sticky_pos(to))
}


# sticky -------------------------------------------------------------

#' @export
restore_sticky_names.gibble_df <- function(x, to, new) {
  groups <- names(x)[sticky_pos(to)]
  new_gibble(x, groups)
}
