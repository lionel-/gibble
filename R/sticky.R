
#' @export
sticky_cols <- function(x, ...) {
  UseMethod("sticky_cols")
}

#' @export
`[.sticky_df` <- function(x, i) {
  if (!all(sticky_cols(x) %in% i)) {
    abort("Can't unselect sticky columns")
  }
  new_gibble(NextMethod(), sticky_cols(x))
}


#' @export
`names<-.sticky_df` <- function(x, value) {
  out <- NextMethod()

  nms <- names(out)
  changed <- which(names(x) != value)
  pos <- sticky_cols(x)
  pos <- pos[pos %in% changed]

  if (length(pos)) {
    restore_sticky_names(out, x, pos)
  } else {
    out
  }
}

#' @export
restore_sticky_names <- function(x, to, new) {
  UseMethod("restore_sticky_names")
}
