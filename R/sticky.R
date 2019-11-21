
#' @export
sticky_cols <- function(x, ...) {
  UseMethod("sticky_cols")
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
