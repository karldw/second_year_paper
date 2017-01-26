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

mpg_to_L100km_coef <- (100 * 3.785411784 / 1.609344)  # 3.7 L/gal, 1.6 km/mi


count_mpg_merge_matches <- function() {  # testing stuff, no need to run this.
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


if (! exists('state_day_mpg_avg')) {  # This is expensive; only do it once.
    state_day_mpg_avg <- auctions %>%
        filter(! is.na(buy_state)) %>%
        select(sale_date, buy_state, vin_pattern) %>%
        inner_join(vin_decoder, by = 'vin_pattern')  %>%
        select(sale_date, buy_state, combined) %>%
        group_by(sale_date, buy_state) %>%
        # important to take 1/combined before mean()
        # combined highway and city efficiency
        summarize(combined = mean(mpg_to_L100km_coef / combined), count = n()) %>%
        collect(n = Inf)
}



efficiency_plot <- lapply_bind_rows(2002:2005, filter_event_window,
                                    .data = state_day_mpg_avg) %>%
    filter(buy_state %in% c('WA', 'AK', 'OR','NV')) %>%
    add_event_time() %>%
    add_sale_year() %>%
    ggplot(aes(x = event_time, y = combined, color = factor(buy_state))) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = 'loess', span = 0.2, se = FALSE) +
    facet_grid(sale_year ~ .) +
    labs(x = 'Event time (days)', y = 'Mean daily L/100km', color = 'State') +
    PLOT_THEME
save_plot(efficiency_plot, 'test_efficiency_plot.pdf', scale_mult = 3)
