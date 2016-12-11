
options(
    # Stop R from being too helpful
    warn = 1,
    warnPartialMatchDollar = TRUE,
    # warnPartialMatchArgs = TRUE,  # raises too many warnings
    warnPartialMatchAttr = TRUE
)


PLOT_THEME <- ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = NA),
    panel.border     = ggplot2::element_blank(),
    axis.line.x      = ggplot2::element_line(color = 'gray5', lineend = 'round'),
    axis.line.y      = ggplot2::element_line(color = 'gray5', lineend = 'round'),
    panel.grid.major = ggplot2::element_line(color = "gray95"),
    panel.grid.minor = ggplot2::element_line(color = "gray98"),
    axis.ticks       = ggplot2::element_line(color = 'gray5', lineend = 'round'),
    axis.text        = ggplot2::element_text(color = "black", size = 10),
    axis.title       = ggplot2::element_text(color = "black", size = 12),
    legend.key       = ggplot2::element_blank(),
    legend.background= ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
)


source('common_functions.r')
