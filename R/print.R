print_handwriting <- function(
  nrows = 9,
  paper_width = 21, # cm
  paper_height = 29.7 # cm
) {
  path <- tempfile(fileext = ".pdf")
  single_row <- function(
    offset = 0
  ) {
    list(
      ggplot2::geom_polygon(ggplot2::aes(x, y), data = tibble::tibble(x = c(0, 1, 1, 0), y = rep(c(11/30 + offset,  19/30 + offset), each = 2)),
                        fill = colorspace::lighten("cadetblue1", 0.4)),
      ggplot2::geom_hline(
        yintercept = offset + seq(0.1, 0.9, length.out = 4),
        colour = c("gray60", "gray40", "gray40", "gray60"),
        size = 0.4,
      )
    )
  }
  all_rows <- purrr::map(c(1:nrows) - 1, single_row)
  p <- ggplot2::ggplot() +
    all_rows +
    ggplot2::theme_void() +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme(plot.margin = grid::unit(rep(2, 4), "cm"))
  ggplot2::ggsave(path, p, width = paper_width, height = paper_height, units = "cm")
  shell.exec(path)
  invisible()
}
