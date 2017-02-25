source('r_defaults.r')  # set common parameters and call common_functions.r
# NB: con, auctions, states and vin_decoder are defined in r_defaults.r

install_lazy(c('dplyr', 'ggplot2', 'magrittr', 'lfe', 'memoise', 'lubridate'))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
library(magrittr)
suppressPackageStartupMessages(library(lfe))
library(memoise)


count_mpg_merge_matches <- function() {  # testing stuff, no need to run this.
    auctions_nrow <- force_nrow(auctions)
    # do a few extra selects in here to cut down on copying columns around
    vin_decoder_msrp <- vin_decoder %>% select(vin_pattern, msrp)
    auctions_unmatched <- auctions %>%
        select(vin_pattern) %>%
        left.join(vin_decoder_msrp, by = 'vin_pattern') %>%
        select(msrp) %>%
        filter(is.na(msrp)) %>%
        force_nrow()
    print(paste0(100 * auctions_unmatched / auctions_nrow, "% of sales unmatched."))
}


## No longer needed:
# add_vin_pattern <- function(.tbl) {
#     con_psql <- .tbl$src$con
#     from <- dplyr::sql_subquery(con_psql, dplyr::sql_render(.tbl), name = NULL)
#     select_sql <- "*, concat(substring(vin FROM 1 FOR 8), substring(vin FROM 10 FOR 2)) AS vin_pattern"
#
#     select_query <- dplyr::sql_select(con = con_psql, select = sql(select_sql),
#         from = from)
#     out_tbl <- dplyr::tbl(.tbl$src, select_query)
#     return(out_tbl)
# }


# pull_efficiency_data_unmemoized <- function(df_base = auctions,
#         date_var = 'sale_date', id_var = 'buy_state',
#         id_var_filter_set = c('AK', find_match_states_crude()),
#         years = 2002:2014) {
#     stopifnot(length(date_var) == 1, length(id_var) == 1, 'tbl' %in% class(df_base))
#     if (id_var != 'buy_state') {
#         stop("Not implemented")
#     }
#     if (! all(years == 2002:2014)) {
#         stop("Not implemented")
#     }
#
#     group_vars <- c(id_var, date_var)
#     if (date_var == 'event_week') {
#         # Add sale_year because event_week is just a week integer, something like -10,
#         # and I don't want to total over year.
#         df_base <- df_base %>% add_sale_year()
#         group_vars <- c(group_vars, 'sale_year')
#     } else if (date_var == 'event_time') {
#         # Add sale_date because (1) we need to differentiate years, same as event_week,
#         # and (2) it's convenient to have sale_date included in the output variables,
#         # without actually changing the level of aggregation. (Put differently,
#         # event_time and sale_year identify days exactly as well as sale_date.)
#         group_vars <- c(group_vars, 'sale_year', 'sale_date')
#     }
#
#     if (is.null(id_var_filter_set) || is.na(id_var_filter_set)) {
#         # do no special filtering here, just NAs
#         filter_call <- lazyeval::interp(~! is.na(var), var = as.name(id_var))
#     } else {
#         filter_call <- lazyeval::interp(~var %in% x,
#             var = as.name(id_var), x = id_var_filter_set)
#     }
#     df_base <- filter_(df_base, .dots = filter_call)
#
#     df_base %>%
#     select_(.dots = c(group_vars, 'vin_pattern')) %>%
#     inner.join(vin_decoder, by = 'vin_pattern') %>%
#     select_(.dots = c(group_vars, 'combined')) %>%
#     group_by_(.dots = group_vars) %>%
#     # important to take 1/combined before mean()
#     # combined is combined highway and city efficiency
#     summarize(fuel_cons = mean(mpg_to_L100km_coef / combined), count = n()) %>%
#     ungroup() %>%
#     collect(n = Inf) %>%
#     return()
# }
# if (! existsFunction('pull_efficiency_data') || ! is.memoized(pull_efficiency_data)) {
#     pull_efficiency_data <- memoize(pull_efficiency_data_unmemoized)
# }

control_states <- find_match_states_crude()

make_fuel_cons_plot <- function(freq) {
    # state_day_cons_avg <- pull_efficiency_data()
    # base_df <- lapply_bind_rows(2002:2005, filter_event_window_one_year, days_before = 60,
    #         .data = STATE_DAY_CONS_AVG, rbind_src_id = 'sale_year') %>%
    #     filter(buy_state %in% c('AK', control_states)) %>%
    #     add_event_time()

    base_df <- aggregate_sales_dd(years = 2002:2005, agg_var = 'event_week',
        days_before = 70, aggregate_fn = get_sales_efficiency)

    if (freq == 'daily') {
        grouped_df <- base_df %>% group_by(buy_state, sale_year, event_time)
    } else if (freq == 'weekly') {
        grouped_df <- base_df %>% group_by(buy_state, sale_year, event_week) %>%
            # aggregate from mean daily to mean weekly (weighted because unequal counts)
            summarize(fuel_cons = weighted.mean(fuel_cons, w = sale_count))
    } else {
        err_msg <- sprintf("Bad value of freq: '%s'", vec2string(freq))
        stop(err_msg)
    }
    # then standardize the daily or weekly mean within each state/year
    to_plot <- grouped_df %>%
        group_by(buy_state, sale_year) %>%
        mutate(fuel_cons = scale(fuel_cons))

    if (freq == 'daily') {
        out_plot <- ggplot(to_plot, aes(x = event_time, y = fuel_cons,
                           color = factor(buy_state)))
        loess_span <- 0.15
        lab_x <- 'Event time (days)'
        lab_title <- 'Efficiency of cars sold in top Alaska-buyer states, daily averages'
    } else if (freq == 'weekly') {
        out_plot <- ggplot(to_plot, aes(x = event_week, y = fuel_cons,
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


make_all_plot_types <- function(outcome, aggregation_level = 'weekly', verbose = FALSE) {
    if (verbose) {
        message(sprintf('Making plots for %s', outcome))
    }
    plot_effects_by_anticipation(outcome, aggregation_level, title = FALSE)
    run_dd_pick_max_effect(outcome, aggregation_level = aggregation_level)
    if (aggregation_level == 'daily') {
        plot_effects_individual_period(outcome, 'daily',
            fixed_effects = c('sale_year', 'sale_dow'))
    } else {
        plot_effects_individual_period(outcome, aggregation_level)
    }
    invisible(NULL)
}
all_outcomes <- c('fuel_cons', 'fuel_cons_log')
lapply(all_outcomes, make_all_plot_types)


# pull_efficiency_data()
# make_fuel_cons_plot('weekly')
# x <- aggregate_sales_dd(years = 2002:2005, days_before = 60, agg_var = 'event_week',
    # aggregate_fn = get_sales_efficiency)

# save_plot(make_fuel_cons_plot('daily'), 'test_efficiency_plot.pdf', scale_mult = 2)
# save_plot(make_fuel_cons_plot('weekly'), 'test_efficiency_plot2.pdf', scale_mult = 2)
