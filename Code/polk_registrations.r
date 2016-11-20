if (!existsFunction('install_lazy')) {
    source('r_default_functions.r')
}

source('common_functions.r')

install_lazy(c('readxl', 'dplyr', 'ggplot2'), verbose = FALSE)
library(magrittr)
library(dplyr)
library(ggplot2)

PLOT_THEME <- theme(panel.background = element_rect(fill = NA),
                    panel.border = element_rect(fill = NA, color = 'black'),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.ticks = element_line(color = 'gray5'),
                    axis.text = element_text(color = 'black', size = 10),
                    axis.title = element_text(color = 'black', size = 12),
                    legend.key = element_blank(),
                    strip.background = element_blank())

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
                      state = ifelse(state_name == 'DISTRICT OF COLUMBIA', 'DC', state),
                      alaska = factor(state == 'AK', levels=c(TRUE, FALSE), labels=c('Alaska', 'Other states'))
                     ) %>%
        dplyr::select(-year, -quarter, -tp) %>%
        dplyr::group_by(state, county_name) %>%
        dplyr::arrange(state, county_name, date) %>%
        dplyr::mutate(lag_cnt = dplyr::lag(cnt)) %>%
        dplyr::ungroup()

    return(df)
}

make_plots <- function(df) {

    df_years <- sort(unique(lubridate::year(df$date)))
    q4_dates <- as.Date(sprintf("%s-10-01", df_years))
    state_df <- dplyr::filter(df, ! is.na(state)) %>% dplyr::group_by(date, alaska) %>% dplyr::summarize(cnt = sum(cnt))
    alaska_vs_other_states <- ggplot(state_df, aes(x=date, y=cnt/1000)) +
        geom_line() +
        geom_vline(xintercept = as.integer(q4_dates), color='red', linetype='dotted', alpha = 0.3) +
        ylim(0,NA) +
        facet_grid(alaska~., scales = 'free_y') +
        labs(x='', y='Thousands of vehicles', title='') +
        PLOT_THEME


    plot_dir <- '../Text/Plots'
    stopifnot(dir.exists(plot_dir))

    file.path(plot_dir, 'vehicle_registrations_alaska_vs_notitle.pdf') %>%
    ggsave(alaska_vs_other_states, width=6.3, height=3.54)



}


df <- load_regs_data()
make_plots(df)
