test_that("Output of format_percentage() is correct", {
	x <- seq(0, 1, length.out = 5)
	p <- format_percentage(x)
	expect_true(is.character(p))
	expect_equal(length(p), length(x))
})

# TODO: add tests for `format_cols`
