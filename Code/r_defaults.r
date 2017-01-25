
options(
    # Stop R from being too helpful
    warn = 1,
    warnPartialMatchDollar = TRUE,
    # warnPartialMatchArgs = TRUE,  # raises too many warnings
    warnPartialMatchAttr = TRUE
)

source('common_functions.r')

if (length(.libPaths()) < 2) {
    if (get_os() == 'win') {
        # get the major.minor, so 3.3.2 becomes 3.3
        r_version <- paste(R.Version()$major,
                           gsub('\\..*', '', R.Version()$minor, perl = TRUE),
                           sep = '.')
        my_library <- file.path(path.expand('~'), 'Documents', 'R', 'win-library',
                                r_version)
        .libPaths(my_library)
        rm(my_library)
    } else {
        # It'll be similar to the windows version, but should probably include
        # R.Version()$platform
        stop('Not sure what to do here.')
    }
}


install_lazy('ggplot2', verbose = FALSE)

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
# Colors from http://haas.berkeley.edu/style-guide/colors.html
# BLUE_AND_YELLOW <- c('#003A70', '#FFBD17')
BLUE_AND_YELLOW <- c('#0066CC', '#FFBD17')
