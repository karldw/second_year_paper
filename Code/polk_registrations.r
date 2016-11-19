if (!existsFunction('install_lazy')) {
    source('r_default_functions.r')
}

source('common_functions.r')

install_lazy(c('readxl', 'dplyr', 'ggplot2'), verbose = FALSE)
library(magrittr)
library(dplyr)
library(ggplot2)
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
        dplyr::filter(! is.na(state_name)) %>%
        dplyr::mutate(state_name = ifelse(state_name == 'UNKNOWN STATE', NA, state_name),
                      county_name = ifelse(county_name == 'UNKNOWN COUNTY', NA, county_name),
                      year = as.integer(substring(tp, 1, 4)),
                      quarter = as.integer(substring(tp, 10, 10)),
                      date = as.Date(sprintf("%s-%s-01", year, ((quarter - 1) * 3 + 1))),
                      state = state.abb[match(state_name, toupper(state.name))],
                      state = ifelse(state_name == 'DISTRICT OF COLUMBIA', 'DC', state),
                      alaska = factor(state == 'AK', levels=c(TRUE, FALSE), labels=c('Alaska', 'Not-Alaska'))
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
    alaska_vs_other_states <- ggplot(state_df, aes(x=date, y=cnt)) +
        geom_line() +
        geom_vline(xintercept = as.integer(q4_dates), color='red') +
        facet_grid(alaska~., scales = 'free_y')




    plot_list <- list(alaska_vs_other_states)
    return(plot_list)
}


# df <- load_regs_data()
plt_list <- make_plots(df)
print(plt_list[[1]])

