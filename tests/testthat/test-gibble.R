
test_that("gibble has sticky cols", {
  df <- new_gibble(mtcars, c("cyl", "vs"))
  expect_identical(sticky_pos(df), c(cyl = 2L, vs = 8L))
})
