#' @import rlang
NULL

set_diff <- function(x, y) {
  vctrs::vec_unique(vctrs::vec_slice(x, !vctrs::vec_in(x, y)))
}
set_union <- function(x, y) {
  vctrs::vec_unique(vctrs::vec_c(x, y))
}
