library(grid)
library(gtable)
library(ggplot2)

context("vdiffr")

test_that("multiplication works", {
  test_draw_axis <- function(add_labels = FALSE) {
    theme <- theme_test() + theme(axis.line = element_line(size = 0.5))
    positions <- c("top", "right", "bottom", "left")

    n_breaks <- 3
    break_positions <- seq_len(n_breaks) / (n_breaks + 1)
    labels <- if (add_labels) as.character(seq_along(break_positions))

    # create the axes
    axes <- lapply(positions, function(position) {
      ggplot2:::draw_axis(break_positions, labels, axis_position = position, theme = theme)
    })
    axes_grob <- gTree(children = do.call(gList, axes))

    # arrange them so there's some padding on each side
    gt <- gtable(
      widths = unit(c(0.05, 0.9, 0.05), "npc"),
      heights = unit(c(0.05, 0.9, 0.05), "npc")
    )
    gt <- gtable_add_grob(gt, list(axes_grob), 2, 2, clip = "off")
    plot(gt)
  }

  vdiffr::expect_doppelganger("test1", test_draw_axis(FALSE))
  vdiffr::expect_doppelganger("test2", test_draw_axis(TRUE))
})
