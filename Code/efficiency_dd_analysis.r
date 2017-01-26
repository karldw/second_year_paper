source('r_defaults.r')
# library(RPostgreSQL)
install_lazy(c('dplyr', 'ggplot2', 'magrittr', 'lfe', 'memoise', 'lubridate'), verbose = FALSE)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
library(magrittr)
suppressPackageStartupMessages(library(lfe))
library(memoise)

POSTGRES_DB <- 'second_year_paper'
POSTGRES_CLEAN_TABLE <- 'auctions_cleaned'
POSTGRES_VIN_DECODER_TABLE <- 'vin_decoder'
if (! exists('con')) {
    pg_user <- Sys.info()[["user"]] %>% tolower()
    con <- src_postgres(POSTGRES_DB, user = pg_user, password = pg_user)
}
auctions    <- tbl(con, POSTGRES_CLEAN_TABLE)
states      <- tbl(con, 'states')
vin_decoder <- tbl(con, POSTGRES_VIN_DECODER_TABLE)  # uniquely identified by vin_pattern


count_mpg_merge_matches <- function() {  # testing stuff...
    auctions_nrow <- force_nrow(auctions)
    # do a few extra selects in here to cut down on copying columns around
    vin_decoder_msrp <- vin_decoder %>% select(vin_pattern, msrp)
    auctions_unmatched <- auctions %>%
        select(vin_pattern) %>%
        dplyr::left_join(vin_decoder_msrp, by = 'vin_pattern') %>%
        select(msrp) %>%
        filter(is.na(msrp)) %>%
        force_nrow()
    print(paste0(100 * auctions_unmatched / auctions_nrow, "% of sales unmatched."))
}

if (! exists('state_day_mpg_avg')) {
    state_day_mpg_avg <- auctions %>%
        filter(! is.na(buy_state)) %>%
        select(sale_date, buy_state, vin_pattern) %>%
        inner_join(vin_decoder, by = 'vin_pattern')  %>%
        select(sale_date, buy_state, combined) %>%
        group_by(sale_date, buy_state) %>%
        # important to take 1/combined before mean()
        summarize(combined_gpm = mean(1 / combined), count = n()) %>%
        collect(n = Inf)
}

efficiency_plot <- state_day_mpg_avg %>% filter(buy_state %in% c('WA', 'AK')) %>%
    filter_event_window(2002) %>%
    ggplot(aes(x = sale_date, y = combined_mpg, color = factor(buy_state))) +
    # geom_point(alpha = 0.2) +
    geom_smooth(method = 'loess', span = 0.05) +
    labs(x = '', y = 'Mean daily GPM') +
    PLOT_THEME
save_plot(efficiency_plot, 'test_efficiency_plot.pdf', 2)
