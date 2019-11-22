
#' @export
new_sticky <- function(x, sticky, ..., class = NULL) {
  if (is_character(sticky)) {
    sticky <- match(sticky, names(x))
    if (anyNA(sticky)) {
      abort("Can't find `sticky` columns in data frame.")
    }
  }
  vctrs::new_data_frame(
    x,
    sticky = sticky,
    ...,
    class = c(class, "sticky_df")
  )
}

#' @export
sticky_pos <- function(x, ...) {
  pos <- attr(x, "sticky")
  set_names(pos, names(x)[pos])
}

# vctrs --------------------------------------------------------------

#' @importFrom vctrs vec_restore
#' @export
vec_restore.sticky_df <- function(x, to, ...) {
  new_sticky(x, sticky = sticky_pos(to))
}


# base ---------------------------------------------------------------

#' @export
`[.sticky_df` <- function(x, i) {
  if (!all(sticky_pos(x) %in% i)) {
    abort("Can't unselect sticky columns")
  }
  vec_restore(NextMethod(), x)
}


#' @export
`names<-.sticky_df` <- function(x, value) {
  out <- NextMethod()

  nms <- names(out)
  changed <- which(names(x) != value)
  pos <- sticky_pos(x)
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
