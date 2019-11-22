
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
`[.sticky_df` <- function(x,
                          i,
                          ...,
                          sticky = c("assert", "drop", "keep")) {
  ellipsis::check_dots_empty()

  i <- vctrs::vec_as_index(i, n = length(x), names = names(x))

  sticky_pos <- sticky_pos(x)
  if (!all(sticky_pos %in% i)) {
    sticky <- match.arg(sticky, c("assert", "drop", "keep"))
    i <- switch(sticky,
      assert = abort("Can't unselect sticky columns"),
      drop = i,
      keep = union(sticky_pos, i)
    )
  }

  # Don't call NextMethod() because it doesn't support `sticky`
  # argument
  out <- as.data.frame(x)[i]

  vec_restore(out, x)
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
