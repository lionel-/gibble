
test_that("sticky df have sticky cols", {
  df <- new_gibble(mtcars, "cyl")
  expect_identical(sticky_pos(df), c(cyl = 2L))
  expect_true(inherits_all(df, c("gibble_df", "sticky_df", "data.frame")))
})

test_that("sticky df is restored after subsetting", {
  out <- new_gibble(mtcars, "cyl")[1:3]
  expect_true(inherits_all(out, c("gibble_df", "sticky_df", "data.frame")))
})

test_that("can't unselect sticky columns if sticky is 'assert'", {
  df <- new_gibble(mtcars, "cyl")

  out <- df[1:3]
  expect_is(out, "gibble_df")
  expect_identical(unstructure(out), unstructure(mtcars[1:3]))
  expect_identical(sticky_pos(out), c(cyl = 2L))

  expect_error(df[1], "Can't unselect sticky columns")
})

test_that("can unselect sticky columns if sticky is 'drop'", {
  df <- new_gibble(mtcars, "cyl")
  out <- df["disp", sticky = "drop"]

  # Upcoerces to bare data frame
  expect_true(inherits_only(out, "data.frame"))
  expect_identical(unstructure(out), unstructure(mtcars["disp"]))
})

test_that("sticky columns are always kept if sticky is 'keep'", {
  df <- new_gibble(mtcars, "cyl")
  out <- df["disp", sticky = "keep"]

  expect_is(out, "gibble_df")
  expect_named(out, c("cyl", "disp"))
  expect_identical(sticky_pos(out), c(cyl = 1L))

  df <- new_gibble(mtcars, c("cyl", "vs"))
  out <- df["disp", sticky = "keep"]

  expect_is(out, "gibble_df")
  expect_named(out, c("cyl", "vs", "disp"))
  expect_identical(sticky_pos(out), c(cyl = 1L, vs = 2L))
})

test_that("sticky cols are restored after renaming", {
  df <- new_gibble(mtcars, c("cyl", "vs"))

  names(df)[2] <- "foo"
  expect_identical(sticky_pos(df), c(foo = 2L, vs = 8L))

  colnames(df)[2] <- "bar"
  expect_identical(sticky_pos(df), c(bar = 2L, vs = 8L))
})
