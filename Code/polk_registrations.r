source('r_defaults.r')

install_lazy(c('readxl', 'dplyr', 'ggplot2', 'feather'), verbose = FALSE)
library(magrittr)
library(dplyr)
library(ggplot2)
library(feather)

try(
EXCEL_DIR <- file.path(box_home()[1],
                     'second_year_paper_data/Polk')
, silent = TRUE
)
if (! (exists('EXCEL_DIR') && dir.exists(EXCEL_DIR))) {
    EXCEL_DIR <- '~/Everything_else/Box_local/second_year_paper_data/Polk'
}
stopifnot(dir.exists(EXCEL_DIR))

load_regs_data <- function() {
    filename <- file.path(EXCEL_DIR, 'OPP-12322003_University of California Berkley.xlsx')
    df <- readxl::read_excel(filename, skip=5) %>%
        dplyr::as.tbl() %>%
        setNames(tolower(names(.))) %>%
        dplyr::filter(! is.na(state_name),  # NAs induced from reading too many excel rows
                      state_name != 'UNKNOWN STATE'  # Actual missing data (N=8)
                      ) %>%
        dplyr::mutate(year = as.integer(substring(tp, 1, 4)),
                      quarter = as.integer(substring(tp, 10, 10)),
                      date = as.Date(sprintf("%s-%s-01", year, ((quarter - 1) * 3 + 1))),
                      state = state.abb[match(state_name, toupper(state.name))],
                      state = if_else(state_name == 'DISTRICT OF COLUMBIA', 'DC', state),
                      alaska = factor(state == 'AK', levels=c(TRUE, FALSE), labels=c('Alaska', 'All others'))
                     ) %>%
        dplyr::select(-quarter, -tp) %>%
        dplyr::group_by(state, county_name) %>%
        dplyr::arrange(state, county_name, date) %>%
        dplyr::mutate(lag_cnt = dplyr::lag(cnt)) %>%
        dplyr::ungroup()

    return(df)
}

make_plots <- function() {
    df <- load_regs_data()
    pop_df <- load_pop_data()

    # check that we have complete coverage by state
    stopifnot(nrow(dplyr::anti_join(df, pop_df, by=c('state_name', 'year'))) == 0)
    df_years <- sort(unique(lubridate::year(df$date)))
    q4_dates <- as.Date(sprintf("%s-10-01", df_years))
    state_regs_df <- dplyr::filter(df, ! is.na(state)) %>%
        dplyr::group_by(date, alaska) %>%
        dplyr::summarize(cnt = sum(cnt)) %>%
        dplyr::mutate(year = lubridate::year(date))
    # Merge pop and cars data:
    state_pop_df <- pop_df %>%
        mutate(alaska = factor(state_name == 'ALASKA', levels=c(TRUE, FALSE),
                               labels=c('Alaska', 'All others'))) %>%
        dplyr::group_by(alaska, year) %>%
        dplyr::summarize(population = sum(population))
    state_df <- dplyr::left_join(state_regs_df, state_pop_df, by=c('alaska', 'year')) %>%
        mutate(count_per_capita = cnt / population)

    alaska_vs_other_states <- ggplot(state_df, aes(x=date, y=count_per_capita*1000,
                                                   color=alaska)) +
        geom_line() +
        geom_vline(xintercept = as.integer(q4_dates), color='black',
                   linetype='dashed', alpha = 0.1, size=0.2) +
        ylim(0, NA) +
        labs(x='', y='New registrations per 1000 people', title='', color='') +
        PLOT_THEME +
        # Remove minor gridlines because we have so many vertical lines going on
        theme(panel.grid.minor = element_blank(), legend.position = c(.9, .9))

    save_plot(alaska_vs_other_states, 'vehicle_registrations_alaska_vs_notitle.pdf')
}


load_pop_data <- function() {
    local_data_dir <- '../Data'
    stopifnot(dir.exists(local_data_dir))
    feather_filename <- file.path(local_data_dir, 'us_county_by_year_population.feather')
    if (! file.exists(feather_filename)) {
        err_msg <- "Error: county population data file doesn't exist. Please run parse_county_data.r"
        stop(err_msg)
    }
    county_pop_data <- read_feather(feather_filename) %>%
        mutate(state_name = toupper(stname)) %>%
        select(-stname)
    return(county_pop_data)
}


make_plots()
