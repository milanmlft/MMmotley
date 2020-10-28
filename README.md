
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MMmotley

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/milanmlft/MMmotley/branch/master/graph/badge.svg)](https://codecov.io/gh/milanmlft/MMmotley?branch=master)
[![R build
status](https://github.com/milanmlft/MMmotley/workflows/R-CMD-check/badge.svg)](https://github.com/milanmlft/MMmotley/actions)
<!-- badges: end -->

This package contains various, not necessarily related, functions to
make some data analysis tasks a little easier.

Why “motley”? Because “misc” or “miscellaneous” is already used [too
often](https://github.com/search?q=misc+language%3AR&type=Repositories),
so I went looking for a
[synonym](https://www.lexico.com/synonym/miscellaneous).

Also, this is more like an educational sandbox for myself to learn more
about R package development and everything related to that.

## Installation

You can install MMmotley from
[GitHub](https://github.com/milanmlft/MMmotley) with:

    # install.packages("devtools")
    devtools::install_github("milanmlft/MMmotley")

## Contents

    library(MMmotley)

### `gg_pval_hist`: ggplot2-based p-value histograms with better default layout

    ## Generate some random uniform p-values
    p_values <- runif(1000)

By default, `ggplot2::geom_histogram()` centers the first and last bins
of the histogram on the x-axis lower and upper limits, respectively.

    library(ggplot2)
    ggplot(mapping = aes(x = p_values)) +
      geom_histogram(binwidth = 0.05)

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

In case of p-values, which lie in the interval \[0, 1\], it makes more
sense to set the boundaries of the first and last bins at 0 and 1. This
is what `gg_pval_hist` does, with some additional tweaks to improve the
layout. By default, the binwidths are set at 0.05, commonly used as a
cut-off for significance, so that the height of the first bar represents
the number of p-values lying between 0 and 0.05, i.e. the number of
cases that would be deemed significant.

    p <- gg_pval_hist(p_values, binwidth = 0.05)
    p

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

### `save_plots`: save multiple plots to a single PDF

This is a convenience function if you have a list of plots and want to
save them to a PDF file with each plot ending up on its own page.

    # Generate list of plots
    mtcars_split <- split(mtcars, mtcars$cyl)
    plot_list <- lapply(mtcars_split, function(d) {
      ggplot(d, aes(mpg, wt)) +
        geom_point()
    })

    # Save plots
    file <- tempfile()
    save_plots(plot_list, file)

### `format_percentage`: Format numbers as percentages

This is a simple wrapper around `scales::label_percent()` to convert
numeric input to percentages and return them as characters.

    x <- seq(0, 1, by = 0.25)
    format_percentage(x)
    #> [1] "0%"   "25%"  "50%"  "75%"  "100%"
