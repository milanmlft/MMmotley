test_that("Output of format_percentage() is correct", {
	x <- seq(0, 1, length.out = 5)
	p <- format_percentage(x)
	expect_true(is.character(p))
	expect_equal(length(p), length(x))
})

test_that("format_cols() returns character columns", {
  mtcars_tbl <- tibble::as_tibble(mtcars)
  res <- format_cols(mtcars_tbl)
  expect_true(all(sapply(res, is.character)))
})

test_that("format_cols() works for selected columns", {
  mtcars_tbl <- tibble::as_tibble(mtcars)
  selected_cols <- c("wt", "qsec")
  res <- format_cols(mtcars_tbl, selected_cols)
  expect_true(all(sapply(res[selected_cols], is.character)))

  ## tidyselect
  # FIXME: res <- format_cols(mtcars_tbl, tidyr::ends_with("t"))
})
