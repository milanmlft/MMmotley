test_that("gg_pval_hist() fails for impossible p-values", {
	expect_error(gg_pval_hist(c(-0.1, runif(10))))
	expect_error(gg_pval_hist(c(1.1, runif(10))))
	expect_error(gg_pval_hist(c(NA, runif(10))))
})

test_that("gg_pval_hist() fails for input types other than numeric", {
	expect_error(gg_pval_hist(character(10)))
	expect_error(gg_pval_hist(integer(10)))
	expect_error(gg_pval_hist(logical(10)))
})

test_that("Output of gg_pval_hist() is stable", {
  # Create synthetic p-values, includes the value 0, so first bin [0, 0.05] will
  # contain 6 values, while all others will contain 5
  pvals <- seq(0, 1, by = 0.01)
  vdiffr::expect_doppelganger("gg_pval_hist", gg_pval_hist(pvals), path = "")
})

test_that("Output of geom_pval_hist() is stable", {
  df <- data.frame(pval = seq(0, 1, by = 0.01))
  p <- ggplot2::ggplot(df, aes(pval)) +
    geom_pval_hist(fill = "grey65", col = "black") +
    scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
  vdiffr::expect_doppelganger("geom_pval_hist", p, path = "")
})
