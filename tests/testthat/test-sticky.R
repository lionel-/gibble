
test_that("sticky cols are restored after renaming", {
  df <- new_gibble(mtcars, c("cyl", "vs"))

  names(df)[2] <- "foo"
  expect_identical(sticky_cols(df), c(foo = 2L, vs = 8L))

  colnames(df)[2] <- "bar"
  expect_identical(sticky_cols(df), c(bar = 2L, vs = 8L))
})
