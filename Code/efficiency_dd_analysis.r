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


if (! exists('STATE_DAY_CONS_AVG')) {  # This is expensive; only do it once.
    STATE_DAY_CONS_AVG <- auctions %>%
        filter(! is.na(buy_state)) %>%
        select(sale_date, buy_state, vin_pattern) %>%
        inner_join(vin_decoder, by = 'vin_pattern')  %>%
        select(sale_date, buy_state, combined) %>%
        group_by(sale_date, buy_state) %>%
        # important to take 1/combined before mean()
        # combined is combined highway and city efficiency
        summarize(combined = mean(mpg_to_L100km_coef / combined), count = n()) %>%
        ungroup() %>%
        collect(n = Inf)
}


control_states <- find_match_states_crude()

make_fuel_cons_plot <- function(freq) {

    base_df <- lapply_bind_rows(2002:2005, filter_event_window, days_before = 60,
            .data = STATE_DAY_CONS_AVG, rbind_src_id = 'sale_year') %>%
        filter(buy_state %in% c('AK', control_states)) %>%
        add_event_time()

    if (freq == 'daily') {
        grouped_df <- base_df %>% group_by(buy_state, sale_year, event_time)
    } else if (freq == 'weekly') {
        grouped_df <- base_df %>% group_by(buy_state, sale_year, event_week) %>%
            # aggregate from mean daily to mean weekly (weighted because unequal counts)
            summarize(combined = weighted.mean(combined, w = count))
    } else {
        err_msg <- sprintf("Bad value of freq: '%s'", paste(freq, collapse = "', '"))
        stop(err_msg)
    }
    # then standardize the daily or weekly mean within each state/year
    to_plot <- grouped_df %>%
        group_by(buy_state, sale_year) %>%
        mutate(combined = scale(combined))

    if (freq == 'daily') {
        out_plot <- ggplot(to_plot, aes(x = event_time, y = combined,
                           color = factor(buy_state)))
        loess_span <- 0.15
        lab_x <- 'Event time (days)'
        lab_title <- 'Efficiency of cars sold in top Alaska-buyer states, daily averages'
    } else if (freq == 'weekly') {
        out_plot <- ggplot(to_plot, aes(x = event_week, y = combined,
                           color = factor(buy_state)))
        loess_span <- 0.25
        lab_x <- 'Event time (weeks)'
        lab_title <- 'Efficiency of cars sold in top Alaska-buyer states, weekly averages'
    }

    # Add common plot stuff
    out_plot <- out_plot +
        geom_point(alpha = 0.1)  +
        geom_smooth(method = 'loess', span = loess_span, se = FALSE) +
        facet_grid(sale_year ~ .) +
        labs(x = lab_x, title = lab_title, y = 'Fuel consumption (standardized)',
             color = 'State') +
        PLOT_THEME
    return(out_plot)
}



save_plot(make_fuel_cons_plot('daily'), 'test_efficiency_plot.pdf', scale_mult = 2)
# save_plot(make_fuel_cons_plot('weekly'), 'test_efficiency_plot2.pdf', scale_mult = 2)
