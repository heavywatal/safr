#' Dot plot
#'
#' @param data data.frame in PAF format.
#' @param linewidth numeric.
#' @param breaks numeric width.
#' @rdname plot-dot
#' @export
plot_dot = function(data, linewidth = 1, breaks = 1e7) {
  ggplot2::ggplot(data) +
    ggplot2::geom_segment(ggplot2::aes(
      .data$start, .data$qstart,
      xend = .data$end, yend = .data$qend, color = .data$strand
    ), linewidth = linewidth) +
    ggplot2::annotate("point", 0, 0, alpha = 0) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::scale_x_continuous(labels = labels_mega, breaks = max, minor_breaks = breaker(breaks)) +
    ggplot2::scale_y_continuous(labels = labels_mega, breaks = max, minor_breaks = breaker(breaks)) +
    ggplot2::labs(x = "target position (Mbp)", y = "query position (Mbp)")
}

#' @rdname plot-dot
#' @export
facet_grid_chr = function() {
  ggplot2::facet_grid(
    dplyr::vars(.data$qchr), dplyr::vars(.data$chr),
    scale = "free", space = "free",
    as.table = FALSE, switch = "both"
  )
}

#' @inheritParams ggplot2::theme_bw
#' @rdname plot-dot
#' @export
theme_dotplot = function(
    base_size = 12, base_family = "",
    base_line_size = base_size / 24, base_rect_size = base_size / 24) {
  ggplot2::theme_light(base_size, base_family, base_line_size, base_rect_size) +
    ggplot2::theme(panel.spacing = grid::unit(0, "mm"))
}

labels_mega = function(breaks) {
  round(breaks * 1e-6, digits = 1)
}

breaker = function(width = 1e7) {
  function(range) {
    .max = range[2L]
    if (.max < width) {
      numeric(0)
    } else {
      seq(width, .max, width)
    }
  }
}
