test_that("save_plots() creates PDF", {
	plot_list <- lapply(1:3, function(x) ggplot())
	file_name <- "test-save_plots.pdf"
	save_plots(plot_list, file_name)
	expect_true(file.exists(file_name))
	expect_equal(file.size(file_name), 4601)
	unlink(file_name)
})
