
options(
    # Stop R from being too helpful
    warn = 1,
    warnPartialMatchDollar = TRUE,
    # warnPartialMatchArgs = TRUE,  # raises too many warnings
    warnPartialMatchAttr = TRUE
)

source('common_functions.r')

# Prevent R from forgetting about the user's home folder (when calling from the command
# line in Windows)
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


####     Memoise stuff from common_functions.r    ####
if (! methods::existsFunction('find_match_states_crude')) {
    install_lazy('memoise', FALSE)
    find_match_states_crude <- memoise::memoize(find_match_states_crude_unmemoized)
}


####    Make joins better    ####
# Unlike the dplyr versions, these don't (or at least shouldn't) allow many-to-many joins.
install_lazy('dplyr', verbose = FALSE)
left.join <- make_join_safer(dplyr::left_join)
right.join <- make_join_safer(dplyr::right_join)
full.join <- make_join_safer(dplyr::full_join)
inner.join <- make_join_safer(dplyr::inner_join)
anti.join <- dplyr::anti_join  # already safe, just doing this for consistency
semi.join <- dplyr::semi_join  # already safe, just doing this for consistency

####    Plotting stuff!    ####
install_lazy(c('ggplot2', 'devtools'), verbose = FALSE)
# PLOT_THEME <- ggplot2::theme(
#     panel.background = ggplot2::element_rect(fill = NA),
#     panel.border     = ggplot2::element_blank(),
#     axis.line.x      = ggplot2::element_line(color = 'gray5', lineend = 'round'),
#     axis.line.y      = ggplot2::element_line(color = 'gray5', lineend = 'round'),
#     panel.grid.major = ggplot2::element_line(color = "gray95"),
#     panel.grid.minor = ggplot2::element_line(color = "gray98"),
#     axis.ticks       = ggplot2::element_line(color = 'gray5', lineend = 'round'),
#     axis.text        = ggplot2::element_text(color = "black", size = 10),
#     axis.title       = ggplot2::element_text(color = "black", size = 12),
#     legend.key       = ggplot2::element_blank(),
#     legend.background= ggplot2::element_blank(),
#     strip.background = ggplot2::element_blank()
# )
if (! is_pkg_installed('hrbrthemes')) {
    devtools::install_github("hrbrmstr/hrbrthemes")
}
# Instead of specifying all the things above, just use the hrbrmstr hrbrthemes, but then
# add back in axis lines and make the grid lines grey.
    PLOT_THEME <- hrbrthemes::theme_ipsum_rc(base_family = "Carlito",
        subtitle_family = "Carlito", caption_family = "Carlito") +
    ggplot2::theme(
        # Make legend text smaller
        legend.title     = ggplot2::element_text(size = 9),
        axis.line.x      = ggplot2::element_line(color = 'gray15', lineend = 'round'),
        axis.line.y      = ggplot2::element_line(color = 'gray15', lineend = 'round'),
        panel.grid.major = ggplot2::element_line(color = "gray86"),
        panel.grid.minor = ggplot2::element_line(color = "gray95"),
        # Make plots with thin margins (top, right, bottom, and left)
        plot.margin      = ggplot2::unit(c(0.1, 0.2, 0.2, 0.2), "cm")
    )

# Colors from http://haas.berkeley.edu/style-guide/colors.html
# BLUE_AND_YELLOW <- c('#003A70', '#FFBD17')
BLUE_AND_YELLOW <- c('#0066CC', '#FFBD17')

# Colorblind-friendly palette with black:
# Borrowed from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# The palette with black:
PALETTE_8_COLOR_START_WITH_BLACK <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                      "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
