test_that("gg_pval_hist() fails for impossible p-values", {
	expect_error(gg_pval_hist(c(-0.1, runif(10))))
	expect_error(gg_pval_hist(c(1.1, runif(10))))
})

test_that("Output of gg_pval_hist() is stable", {
  # Create synthetic p-values, includes the value 0, so first bin [0, 0.05] will
  # contain 6 values, while all others will contain 5
  pvals <- seq(0, 1, by = 0.01)
  vdiffr::expect_doppelganger("p-value histogram", gg_pval_hist(pvals),
  														path = "")
})

test_that("save_plots() creates PDF", {
	plot_list <- lapply(1:3, function(x) ggplot())
	file_name <- "test-save_plots.pdf"
	save_plots(plot_list, file_name)
	expect_true(file.exists(file_name))
	expect_equal(file.size(file_name), 4601)
	unlink(file_name)
})
