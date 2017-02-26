
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


install_lazy('dplyr', verbose = FALSE)
# Establish the Postgres connection and tables.
POSTGRES_DB <- 'second_year_paper'
POSTGRES_CLEAN_TABLE <- 'auctions_cleaned'
POSTGRES_VIN_DECODER_TABLE <- 'vin_decoder'
# These tbl names should be all-caps, but I'm lazy.
if (! exists('con')) {
    pg_user <- tolower(Sys.info()[["user"]])
    con <- dplyr::src_postgres(POSTGRES_DB, user = pg_user, password = pg_user)
}
auctions    <- dplyr::tbl(con, POSTGRES_CLEAN_TABLE)
states      <- dplyr::tbl(con, 'states')
vin_decoder <- dplyr::tbl(con, POSTGRES_VIN_DECODER_TABLE)  # rows identified by vin_pattern


####     Memoise stuff from common_functions.r    ####
install_lazy('memoise', FALSE)
if (! methods::existsFunction('find_match_states_crude')) {
    find_match_states_crude <- memoise::memoize(find_match_states_crude_unmemoized)
}
if (! methods::existsFunction('aggregate_sales_dd')) {
    aggregate_sales_dd <- memoise::memoize(aggregate_sales_dd_unmemoized)
}
if (! methods::existsFunction('get_state_by_time_variation')) {
    get_state_by_time_variation <- memoise::memoize(
        get_state_by_time_variation_unmemoized)
}

####    Make joins better    ####
# Unlike the dplyr versions, these don't (or at least shouldn't) allow many-to-many joins.
left.join <- make_join_safer(dplyr::left_join)
right.join <- make_join_safer(dplyr::right_join)
full.join <- make_join_safer(dplyr::full_join)
inner.join <- make_join_safer(dplyr::inner_join)
anti.join <- dplyr::anti_join  # already safe, just doing this for consistency
semi.join <- dplyr::semi_join  # already safe, just doing this for consistency

####    Plotting stuff!    ####
install_lazy('ggplot2', verbose = FALSE)
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

PLOT_THEME <- ggplot2::theme_minimal(base_family = "Carlito", base_size = 11) +
    ggplot2::theme(
    # Make legend text smaller
    legend.title       = ggplot2::element_text(size = 9),
    strip.text         = ggplot2::element_text(size = 9, color = 'black'),
    legend.text        = ggplot2::element_text(size = 8),
    axis.text          = ggplot2::element_text(color = 'black'),
    axis.line.x        = ggplot2::element_line(color = 'black', lineend = 'round'),
    axis.line.y        = ggplot2::element_line(color = 'black', lineend = 'round'),
    # These grid lines look a little dark on the screen, but they're okay in print
    panel.grid.major   = ggplot2::element_line(color = "gray60", size = 0.15),
    panel.grid.minor   = ggplot2::element_line(color = "gray75", size = 0.13),
    # Make plots with thin margins (top, right, bottom, and left)
    plot.margin        = ggplot2::margin(t = 0.1, r = 0.2, b = 0.1, l = 0.1, unit="cm"),
    legend.margin      = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
    legend.box.margin  = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
    legend.box.spacing = ggplot2::unit(0, 'cm'),
    panel.spacing.y    = ggplot2::unit(1.4, 'lines')
    )

# Colors from http://haas.berkeley.edu/style-guide/colors.html
# BLUE_AND_YELLOW <- c('#003A70', '#FFBD17')
BLUE_AND_YELLOW <- c('#0066CC', '#FFBD17')

# Colorblind-friendly palette with black:
# Borrowed from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# The palette with black:
PALETTE_8_COLOR_START_WITH_BLACK <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                      "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Define some variable lists
OUTCOME_VARS <- c('sale_tot' = 'Sale total',
                 'sale_tot_log' = 'Log sale total',
                 'sale_count' = 'Cars sold',
                 'sale_count_log' = 'Log cars sold',
                 'sales_pr_mean' = 'Average sales price',
                 'sales_pr_mean_log' = 'Average log sales price',
                 'fuel_cons' = 'Fuel consumption',
                 'fuel_cons_log' = 'Log fuel consumption',
                 'msrp_mean' = 'MSRP',  # Nominal MSRP
                 'msrp_mean_log' = 'Log MSRP'  # Nominal MSRP
                 )
# TODO: a better way to do this would be have one function that handles all vars.
GET_SALES_COUNTS_VARS <- c('sales_pr_mean', 'sale_tot', 'sale_count',
    'sales_pr_mean_log', 'sale_tot_log', 'sale_count_log', 'msrp_mean',
    'msrp_mean_log')
GET_SALES_EFFICIENCY_VARS <- c('fuel_cons', 'fuel_cons_log')
stopifnot(setequal(c(GET_SALES_COUNTS_VARS, GET_SALES_EFFICIENCY_VARS),
                   names(OUTCOME_VARS)))
# For some applications, log outcomes are treated differently. Ensure that these two ways
# of selecting log variables are the same.
stopifnot(all(grepl('log', names(OUTCOME_VARS), fixed = TRUE) ==
              endsWith(names(OUTCOME_VARS), 'log')))
