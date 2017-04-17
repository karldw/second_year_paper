
source('r_defaults.r')  # set common parameters and call common_functions.r
# NB: con, auctions, states and vin_decoder are defined in r_defaults.r

install_lazy(c('dplyr', 'ggplot2', 'magrittr', 'lfe', 'memoise', 'lubridate', 'purrr', 'repr'))
stopifnot(is_pkg_installed('dplyr', '0.5.0'))  # dplyr 0.6.0 is going to change things that I don't want to change yet.
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
library(magrittr)
suppressPackageStartupMessages(library(lfe))
library(memoise)

# Previously (in clean_car_auctions.r), I've made and indexd a "rand" variable
# take a 1% sample (that's 1% in expectation; it might vary slightly)
# if (! exists('auctions_sample')) {
#     auctions_sample <- auctions %>%
#         filter(auction_state %in% top_10_states, rand < 0.01) %>%
#         select(sale_date, sales_pr, buy_state, auction_state, sell_state) %>%
#         collect(n = Inf)
# }

get_top_auction_states_ak_buyers_unmemoized <- function(top_n) {
    # Top 10:
    #    auction_state count
    # 1             WA 13397
    # 2             NV  3332
    # 3             OR  2287
    # 4             CA  1697
    # 5             AZ   774
    # 6             UT   306
    # 7             CO   239
    # 8             MN   181
    # 9             TX   159
    # 10            FL   107
    get_top_auction_states_table(top_n, 'AK') %>%
        collect() %$%
        auction_state %>%
        return()
}
if (! existsFunction('get_top_auction_states_ak_buyers')) {
    get_top_auction_states_ak_buyers<-memoize(get_top_auction_states_ak_buyers_unmemoized)
}


get_top_auction_states_table <- function(top_n, buy_state_code) {
    auctions %>% select(buy_state, auction_state) %>%
        filter(buy_state == buy_state_code) %>%
        select(auction_state) %>%
        filter(! is.na(auction_state)) %>%
        group_by(auction_state) %>%
        summarize(count = n()) %>%
        arrange(-count) %>%
        head(top_n) %>%
        return()
}


pull_data_one_year <- function(year, days_before = 30L, days_after = days_before) {
    # top_n_auction_states limits the data returned to auction states in the top n,
    # for alaskan buyers (default of NULL doesn't limit)

    data_one_year <- auctions %>%
        select(sale_date, model_yr, sales_pr, bid_ct,
               veh_type, buy_state, sell_state, auction_state,
               # seller_type, slrdlr_type,
               buyer_id, seller_id) %>%
        filter_event_window_one_year(year = year, days_before = days_before,
                            days_after = days_after) %>%
        filter(! is.na(buy_state))
    return(data_one_year)
}


adjust_per_capita <- function(.data, na.rm = TRUE) {
    state_pop <- states %>%
        select(state, year, population) %>%
        mutate(population = population / 1e6) %>%
        compute()

    if (! is_varname_in(.data, 'sale_year')) {
        .data <- add_sale_year(.data)
    }
    out <- left.join(.data, state_pop,
                     by = c('buy_state' = 'state', 'sale_year' = 'year')) %>%
        # NB: This is sale count and sale total per *million* people
        mutate(sale_count_pc = sale_count / population,
               sale_tot_pc = sale_tot / population)
    if (na.rm) {
        out <- out %>% filter(! is.na(sale_count_pc), ! is.na(sale_tot_pc))
    }

    return(out)
}


sales_counts_one_year_unmemoized <- function(year, ...) {
    stopifnot( (! missing(year)) && (length(year) == 1) && (is.numeric(year)) )

    dividend_day <- first_thursday_in_october(year)
    # calculate sales counts (in sql)
    row_limit <- Inf
    sales_counts <- pull_data_one_year(year, ...) %>%
        select(sale_date, buy_state, buyer_id, sales_pr) %>%

        add_event_time() %>%
        get_sales_counts('event_week') %>%
        # NB: This is sale count and sale total per *million* people
        adjust_per_capita() %>%
        # mutate(alaskan_buyer_post = buy_state == 'AK' & sale_date >= dividend_day) %>%
        # select(alaskan_buyer_post, sale_count, sale_tot, sale_count_pc,
        #        sale_tot_pc, sale_date, buy_state, event_week) %>%
        select(sale_year, event_week, buy_state, sale_count, sale_tot,
               sale_count_pc, sale_tot_pc) %>%
        collect(n = row_limit)
    return(sales_counts)
}
if (! existsFunction('sales_counts_one_year')) {
    sales_counts_one_year <- memoize(sales_counts_one_year_unmemoized)
}


run_dd_one_year_old <- function(year) {
    stopifnot( (! missing(year)) && (length(year) == 1) && (is.numeric(year)) )
    dividend_day <- first_thursday_in_october(year)

    sales_counts <- sales_counts_one_year(year, days_before = 30)
    #%>% mutate(sale_week = lubridate::week(sale_date))

    # DD with LHS of daily sale_count, buyer_id and sale_date FE and buy_state clusters
    sales_counts_formula <- (sale_count_pc ~ 0 |
                             sale_date | 0 | buy_state)
    sales_counts_reg <- my_felm(formula = sales_counts_formula, data = sales_counts)
    print(summary(sales_counts_reg))
    # fixed_effects <- getfe(sales_counts_reg) %>% as.tbl()

    # To graph fixed effects:
    to_graph <- getfe(sales_counts_reg, se = TRUE) %>%
        as.tbl() %>%
        filter(fe == 'sale_date') %>%
        mutate(sale_date = as.Date(idx))

    error_confidence <- 0.95
    conf_mult <- (-1) * qnorm((1 - error_confidence) / 2)
    sale_date_plot <- ggplot(to_graph, aes(x = sale_date, y = effect)) +
        geom_point() +
        geom_vline(xintercept = as.integer(dividend_day), color = 'red3') +
        geom_errorbar(aes(ymin = effect - conf_mult * clusterse,
                          ymax = effect + conf_mult * clusterse)) +
        labs(x = '', y = 'Day FE (cars per 1mm people)') +
        PLOT_THEME #+ ylim(-1, 1)
    save_plot(sale_date_plot, paste0('sale_date_fe_', year, '.pdf'))

    # get the residuals
    # sales_counts_formula2 <- (sale_count ~ 1 | buyer_id)
    # sales_counts_reg2 <- my_felm(formula = sales_counts_formula2, data = sales_counts)
    # sales_counts_resid2 <- residuals(sales_counts_reg2) %>% as.numeric()
    # print(head(sales_counts_resid2))
    # sales_counts <- mutate(sales_counts, resid_buyer_fe = sales_counts_resid2)
}


plot_alaska_sales <- function() {
    # I could just calculate sale volume/counts, but there are huge day-of-week effects
    # So just partial those out, then work with the residuals.

    years <- 2002:2014

    aggregate_one_year <- function(.tbl) {
        .tbl %>%
        filter(buy_state == 'AK') %>%
        group_by(sale_date) %>%
        summarize(sale_count = n(), sale_volume = sum(sales_pr)) %>%
        collect() %>%
        mutate(sale_dow = lubridate::wday(sale_date)) %>%
        return()
    }

    sales_near_windows <- lapply(years, pull_data_one_year, days_before = 60) %>%
        lapply(aggregate_one_year) %>%
        bind_rows() %>%
        ungroup()
    sale_count_reg_dow_resid <- (sale_count ~ 0 | sale_dow) %>%
        my_felm(data = sales_near_windows) %>%
        residuals() %>%
        as.numeric()
    sale_volume_reg_dow_resid <- (sale_volume ~ 0 | sale_dow) %>%
        my_felm(data = sales_near_windows) %>%
        residuals() %>%
        as.numeric()
    sales_resids_long <- sales_near_windows %>%
        select(sale_date) %>%
        mutate(sale_volume_resid = sale_volume_reg_dow_resid / 1e3,
               sale_count_resid  = sale_count_reg_dow_resid) %>%
        reshape2::melt(id.vars = 'sale_date') %>%
        as.tbl() %>%
        mutate(variable = as.character(variable),
               variable = if_else(variable == 'sale_volume_resid',
                                  'Volume residual ($1000s)',
                                  if_else(variable == 'sale_count_resid',
                                  'Count residual', NA_character_)))

    sale_volume_resid_plot <- ggplot(sales_resids_long, aes(x = sale_date, y = value)) +
        geom_point(alpha = 0.5) +
        geom_smooth(span = 0.2) +
        facet_grid(variable ~ ., scales = 'free_y') +
        geom_vline(xintercept = as.integer(first_thursday_in_october(years)),
                   color = 'red3', alpha = 0.4) +
        labs(x = '', y = '', title = 'Sales in Alaska, residualized by day-of-week') +
        PLOT_THEME
    save_plot(sale_volume_resid_plot, 'sale_date_resids_wday_adj.pdf', scale_mult = 2)
}


quality_control_graphs <- function() {

    plot_buyer_id_counts <- function(year) {
        # Moved to a sub-function because it's a lot of extra processing for a less
        # important graph.
        event_year <- pull_data_one_year(year)
        total_days <- event_year %>% ungroup() %>% distinct(sale_date) %>%
            tally() %>% collect() %$% n
        to_plot <- event_year %>% ungroup() %>% distinct(sale_date, buyer_id) %>%
            group_by(buyer_id) %>% summarize(buyer_id_days = n()) %>%
            collect(n = Inf)
        buyer_id_count_plot <- ggplot(to_plot, aes(x = buyer_id_days)) +
            geom_histogram(bins = 30) +
            labs(x = sprintf('Number of days each buyer is present (%s day period)',
                             total_days),
                 y = 'Count', title = sprintf('Buyer counts for %s', year)) +
            PLOT_THEME
        return(buyer_id_count_plot)
    }

    total_sales_by_day <- auctions %>% group_by(sale_date) %>%
        summarize(daily_sales = sum(sales_pr), count = n()) %>% collect(n = Inf)
    sales_by_month_plt <- total_sales_by_day %>%
        mutate(sale_year  = lubridate::year(sale_date),
               sale_month = lubridate::month(sale_date)) %>%
        group_by(sale_year, sale_month) %>%
        summarize(total_sales = sum(daily_sales) / 1e9, sale_date = first(sale_date)) %>%
        ggplot(aes(x = sale_date, y = total_sales)) +
        geom_line() +
        labs(x = 'Date', y = 'Monthly total ($ billions)') +
        ylim(c(0, NA)) +
        PLOT_THEME

    sales_by_week_plt <- total_sales_by_day %>%
        mutate(sale_year = lubridate::year(sale_date),
               sale_week = lubridate::week(sale_date)) %>%
        group_by(sale_year, sale_week) %>%
        summarize(total_sales = sum(daily_sales) / 1e9, sale_date = first(sale_date)) %>%
        ggplot(aes(x = sale_date, y = total_sales)) +
        geom_line() +
        labs(x = 'Date', y = 'Weekly total ($ billions)') +
        ylim(c(0, NA)) +
        PLOT_THEME

    total_sales_by_day_ak_vs <- auctions %>%
        filter(! is.na(buy_state)) %>%
        tag_alaskan_buyer() %>%
        group_by(sale_date, alaskan_buyer) %>%
        summarize(daily_sales = sum(sales_pr), count = n()) %>% collect(n = Inf)
    thursdays_int <- total_sales_by_day_ak_vs %$% sale_date %>%
        lubridate::year() %>% unique() %>% sort() %>%
        first_thursday_in_october() %>%
        as.integer()
    sales_by_month_plt_ak_vs <- total_sales_by_day_ak_vs %>%
        mutate(sale_year  = lubridate::year(sale_date),
               sale_month = lubridate::month(sale_date)) %>%
        group_by(sale_year, sale_month, alaskan_buyer) %>%
        summarize(total_sales = sum(daily_sales) / 1e6, sale_date = first(sale_date)) %>%
        ggplot(aes(x = sale_date, y = total_sales)) +
        geom_line() +
        geom_vline(xintercept = thursdays_int, color = 'red3', alpha = 0.3) +
        facet_grid(alaskan_buyer ~ ., scales = 'free_y') +
        labs(x = 'Date', y = 'Monthly total ($ millions)') +
        ylim(c(0, NA)) +
        PLOT_THEME
    save_plot(sales_by_month_plt_ak_vs, 'total_sales_monthly_total_AK_vs.pdf')
    sales_by_week_plt_ak_vs <- total_sales_by_day_ak_vs %>%
        mutate(sale_year = lubridate::year(sale_date),
               sale_week = lubridate::week(sale_date)) %>%
        group_by(sale_year, sale_week, alaskan_buyer) %>%
        summarize(total_sales = sum(daily_sales) / 1e6, sale_date = first(sale_date)) %>%
        ggplot(aes(x = sale_date, y = total_sales)) +
        geom_line() +
        geom_vline(xintercept = thursdays_int, color = 'red3') +
        facet_grid(alaskan_buyer ~ ., scales = 'free_y') +
        labs(x = 'Date', y = 'Weekly total ($ millions)') +
        ylim(c(0, NA)) +
        PLOT_THEME

    sales_by_day_of_week <- total_sales_by_day %>%
        mutate(day_of_week = lubridate::wday(sale_date, label = TRUE)) %>%
        select(-sale_date) %>%
        group_by(day_of_week) %>%
        summarise_all(sum) %>%
        reshape2::melt(id.vars = 'day_of_week') %>%
        as.tbl() %>%
        ggplot(aes(x = day_of_week, y = value)) +
        geom_bar(stat = 'identity') +
        facet_grid(variable ~ ., scales = 'free_y') +
        labs(x = '') +
        PLOT_THEME
    save_plot(sales_by_day_of_week, 'sales_by_day_of_week.pdf')

    sale_count_comparison <- total_sales_by_day_ak_vs %>%
        mutate(sale_year = lubridate::year(sale_date),
               sale_week = lubridate::week(sale_date)) %>%
        group_by(sale_year, sale_week, alaskan_buyer) %>%
        summarize(count = sum(count), sale_date = first(sale_date)) %>%
        ggplot(aes(x = sale_date, y = count)) +
        geom_point(alpha = 0.8) +
        geom_vline(xintercept = thursdays_int, color = 'red3', alpha = 0.3) +
        facet_grid(alaskan_buyer ~ ., scales = 'free_y') +
        labs(x = 'Date', y = 'Weekly sale count') +
        ylim(c(0, NA)) +
        PLOT_THEME
    save_plot(sale_count_comparison, 'overall_sale_count_ak_vs.pdf')
}


verify_constant_ids <- function() {
    # This is really slow, but it works fine now.
    # There's no need to run it, I just don't want to delete it.

    buyers_with_multiple_states <- auctions %>%
        select(buyer_id, buy_state) %>%
        filter(! is.na(buyer_id), ! is.na(buy_state)) %>%
        distinct(buyer_id, buy_state) %>%
        group_by(buyer_id) %>%
        mutate(state_count = n()) %>%
        filter(state_count > 1) %>%
        ungroup() %>%
        collect(n = Inf)
    if (tbl_has_rows(buyers_with_multiple_states)) {
        message("ERROR:")
        print(buyers_with_multiple_states)
    }

    sellers_with_multiple_states <- auctions %>%
        select(seller_id, sell_state) %>%
        filter(! is.na(seller_id), ! is.na(sell_state)) %>%
        distinct(seller_id, sell_state) %>%
        group_by(seller_id) %>%
        mutate(state_count = n()) %>%
        filter(state_count > 1) %>%
        ungroup() %>%
        collect(n = Inf)
    if (tbl_has_rows(sellers_with_multiple_states)) {
        message("ERROR:")
        print(sellers_with_multiple_states)
    }


}


regression_adjust <- function(.data, varname, formula_rhs) {
    warning("If you use this function, make sure you're making dates factors.")
    # e.g.
    # regression_adjust(sales_counts, sale_count_pc, ~ buy_state + sale_dow)
    if (! is.character(formula_rhs)) {
        formula_rhs <- deparse(substitute(formula_rhs))
    }
    df <- .data %>% collect() %>% ungroup()
    stopifnot(length(varname) == 1,
              is.character(formula_rhs),
              length(formula_rhs) == 1,
              tbl_has_rows(df),
              varname %in% names(df))

    reg_formula <- as.formula(paste(varname, formula_rhs))
    resid <- my_felm(reg_formula, df) %>%
        residuals() %>% as.numeric()
    stopifnot(length(resid) == nrow(df))
    df[[varname]] <- resid
    return(df)
}


adjust_by_weekday <- function(.data, varname) {
    stop('Not used!')
    # Run a regression to demean by weekday, return the demeaned variable
    # in the same dataframe.
    varname <- as.character(substitute(varname))
    df <- .data %>% ungroup() %>% add_sale_dow() %>%
        regression_adjust(varname, '~ 0 | sale_dow')
    return(df)
}


adjust_by_state_and_weekday <- function(.data, varname) {
    # Run a regression to demean by weekday and buy_state (but not the interaction).
    # Return the demeaned variable in the same dataframe.
    varname <- as.character(substitute(varname))
    df <- .data %>% ungroup() %>% add_sale_dow() %>%
        regression_adjust(varname, '~ 0 | sale_dow + buy_state')
    return(df)
}


adjust_by_state <- function(.data, varname) {
    # Run a regression to demean by weekday and buy_state (but not the interaction).
    # Return the demeaned variable in the same dataframe.
    varname <- as.character(substitute(varname))
    df <- .data %>% ungroup() %>%
        regression_adjust(varname, '~ 0 | buy_state')
    return(df)
}


pick_maximizing_window <- function(years = 2002:2014) {
    stop("not implemented yet")
    # Eventually, write this to run the estimation, varying the anticipation window
}


plot_dd_sales <- function(years = 2002:2014) {
    # control_states <- find_match_states_crude()
    # Top-10 controls (sort of) picked by synthetic_controls_analysis.r
    control_states <- c("VT", "WA", "WY", "MT", "IL", "NM", "NH", "WV", "WI", "DC")
    # Iterate over the years, pulling and aggregating data for each.
    # Use a window of 70 days before and after (but aggregate to weeks)
    # Then add a column called sale year with the year variable.
    sales_near_windows <- lapply_bind_rows(years, sales_counts_one_year,
                                           days_before = 70) %>%
        filter(buy_state %in% c('AK', control_states)) %>%
        # tag_alaskan_buyer(as_factor = TRUE) %>%
        # Regression-adjust for weekly patterns. sale_tot_pc and sale_count_pc become
        # the residuals after regressing on dummies for day of week across all included
        # states. NB: This is sale count and sale total per *million* people (otherwise
        # the numbers are really small and R struggles with numerical precision)
        # winsorize(c('sale_count', 'sale_tot')) %>%
        # adjust_by_state_and_weekday(sale_count) %>%
        # adjust_by_state_and_weekday(sale_tot) %>%
        select(sale_year, event_week, buy_state, sale_count, sale_count_pc,
               sale_tot, sale_tot_pc) %>%
        group_by(sale_year, event_week, buy_state) %>%
        summarise_all(sum) %>%
        group_by(sale_year, buy_state) %>%
        mutate_at(vars(sale_count, sale_count_pc, sale_tot, sale_tot_pc), scale)
        # add_sale_year()

    # sales_near_windows_avg <- sales_near_windows %>%
    #     select(event_time, alaskan_buyer, sale_count_pc, sale_tot_pc) %>%
    #     group_by(event_time, alaskan_buyer) %>%
    #     summarise_all(mean)
    # event_lines <- geom_vline(xintercept = as.integer(first_thursday_in_october(years)),
    #                           color = 'red3', alpha = 0.4)

    add_common_plot_details <- function(plot_base) {
        # take a base and add the common elements.
        plot_base +
        geom_point(alpha = 0.3) +
        geom_smooth(method = 'loess', span = 0.3, se = FALSE) +
        facet_grid(sale_year ~ ., scales = 'free_y')+
        labs(x = 'Event time (weeks)', color = 'State') +
        geom_vline(xintercept = 0, color = 'red3', alpha = 0.4) +
        PLOT_THEME
    }
    sale_count_year_facet_plot <- sales_near_windows %>%
        ggplot(aes(x = event_week, y = sale_count, color = buy_state)) %>%
        add_common_plot_details() +
        labs(y = 'Sales counts (weekly total, standardized by state)')

    sale_tot_year_facet_plot <- sales_near_windows %>%
        ggplot(aes(x = event_week, y = sale_tot, color = buy_state)) %>%
        add_common_plot_details() +
        labs(y = 'Sales volume (weekly total, standardized by state)')

    save_plot(sale_count_year_facet_plot, 'dd_plot_sales_counts_year_facet.pdf',
              scale_mult = 3)
    save_plot(sale_tot_year_facet_plot, 'dd_plot_sales_volume_year_facet.pdf',
              scale_mult = 3)
}


# plot_effects_by_anticipation_variable_start_and_end <- function(outcome,
#         aggregation_level = 'daily', days_before_limit = 70) {
#     stop("This is an old function.  You probably don't want it.")
#     if (aggregation_level == 'daily') {
#         loop_start <- (-days_before_limit) + 1
#         min_window_length <- 7
#     } else if (aggregation_level == 'weekly'){
#         loop_start <- ((-days_before_limit) %/% 7) + 1
#         min_window_length <- 1
#     } else {
#         stop("aggregation_level must be 'daily' or 'weekly'")
#     }
#     stopifnot(outcome %in% c('sale_count', 'sale_tot'), days_before_limit > 2)
#
#     windows <- purrr::cross2(
#         # Form the cross of all possible window starts
#         seq.int(loop_start, -1, by = 1),
#         # crossed by all possible window ends:
#         seq.int(loop_start, -1, by = 1),
#         .filter = function(start, end) {
#             # and then filter combos we don't want
#             end - start < min_window_length
#         }
#     )
#     get_results_one_window <- function(start_end_list) {
#         start <- start_end_list[[1]]
#         end <- start_end_list[[2]]
#         reg_results <- run_dd(outcomes = outcome, aggregation_level = aggregation_level,
#             anticipation_window = c(start, end), days_before_limit = days_before_limit)
#
#         # rse is apparently the robust standard error, though it's not well documented.
#         # e.g. identical(sqrt(diag(reg_results$robustvcv)), reg_results$rse)
#         # TODO: use broom::tidy here instead.
#         df <- data_frame(start = start, end = end,
#             coef = reg_results$coefficients['alaskan_buyer_anticipationTRUE', ],
#             se   = reg_results$rse[['alaskan_buyer_anticipationTRUE']],
#             pval = reg_results$rpval[['alaskan_buyer_anticipationTRUE']])
#         return(df)
#
#     }
#
#     to_plot <- purrr::map_df(windows, get_results_one_window)
#
#     # Define a bunch of labels.
#     lab_x <- 'Window end (%s)'
#     lab_y <- 'Window start (%s)'
#     if (aggregation_level == 'daily') {
#         lab_x <- sprintf(lab_x, 'days')
#         lab_y <- sprintf(lab_y, 'days')
#     } else if (aggregation_level == 'weekly') {
#         lab_x <- sprintf(lab_x, 'weeks')
#         lab_y <- sprintf(lab_y, 'weeks')
#     } else {
#         stop("Error!")
#     }
#     if (outcome == 'sale_tot') {
#         lab_color_coef = 'Estimated additional Alaskan anticipation cars sold'
#         lab_color_se = 'SE on additional Alaskan anticipation cars sold'
#     } else if (outcome == 'sale_count') {
#         lab_color_coef = 'Estimated additional Alaskan anticipation sale volume'
#         lab_color_se = 'SE on additional Alaskan anticipation sale volume'
#     } else if (outcome == 'sales_pr_mean') {
#         lab_color_coef = 'Estimated additional Alaskan anticipation average sale price'
#         lab_color_se = 'SE on additional Alaskan anticipation average sale price'
#     } else {
#         stop("Error!")
#     }
#
#     coef_plot <- ggplot(to_plot, aes(x = end, y = start, fill = coef)) +
#         geom_tile() +
#         scale_fill_distiller(palette = 'RdBu') +
#         labs(x = lab_x, y = lab_y, color = lab_color_coef) +
#         PLOT_THEME
#     se_plot <- ggplot(to_plot, aes(x = end, y = start, fill = se)) +
#         geom_tile() +
#         scale_fill_distiller(palette = 'RdBu') +
#         labs(x = lab_x, y = lab_y, color = lab_color_se) +
#         PLOT_THEME
#
#     save_plot(coef_plot, sprintf('anticipation_window_%s_tile_coef.pdf', outcome))
#     save_plot(se_plot,   sprintf('anticipation_window_%s_tile_se.pdf', outcome))
#
#     return(to_plot)
# }

pull_alaska_vs_pooled_mean_sd <- function(outcomes = NULL) {
    if (is.null(outcomes)) {
        # Don't bother with the log outcomes
        outcomes <- names(OUTCOME_VARS)[! endsWith(names(OUTCOME_VARS), '_log')]
    }
    # TODO: consider tweaking this so it reuses the cache from the other plotting fns
    get_variation_once <- function(params) {
        states_included <- params[[1]]
        vars_to_summarize <- params[[2]]
        summary_fn <- params[[3]]
        all_year <- params[[4]]
        stopifnot(is.character(states_included), length(states_included) >= 1,
            is.character(vars_to_summarize), length(vars_to_summarize) > 0,
            is.character(summary_fn), length(summary_fn) == 1,
            is.logical(all_year), length(all_year) == 1)
        get_state_by_time_variation(vars_to_summarize = vars_to_summarize,
            states_included = states_included, summary_fn = summary_fn,
            all_year = all_year,
            aggregation_level = 'weekly') %>%
        reshape2::melt(id.vars = c()) %>%
        as.tbl() %>%
        mutate(
            variable = gsub('_mean$|_sd$', '', as.character(variable), perl = TRUE),
            summary_fn = summary_fn,
            all_year = all_year,
            alaska_pooled = if_else(identical(states_included, 'AK'), 'Alaska-only', 'Pooled')
            ) %>%
        return()
    }
    # Alaska vs pooled
    # Top-10 controls (sort of) picked by synthetic_controls_analysis.r
    control_states <- c("VT", "WA", "WY", "MT", "IL", "NM", "NH", "WV", "WI", "DC")
    states_incl_list <- list(c('AK', control_states), 'AK')
    v1 <- intersect(outcomes, GET_SALES_COUNTS_VARS)
    v2 <- intersect(outcomes, GET_SALES_EFFICIENCY_VARS)
    df <- list(states_incl_list, list(v1, v2), c('sd', 'mean'), c(TRUE, FALSE)) %>%
        purrr::cross_n() %>% purrr::map_df(get_variation_once)
    return(df)
}


plot_alaska_vs_pooled_mean_sd <- function(outcomes = NULL) {
    to_plot <- pull_alaska_vs_pooled_mean_sd(outcomes = outcomes)
    all_year_pooled_vals <- filter(to_plot, all_year==TRUE, alaska_pooled == 'Pooled') %>%
        select(variable, value, summary_fn) %>%
        rename(value_default = value) %>%
        ensure_id_vars(variable, summary_fn)
    to_plot <- to_plot %>%
        inner.join(all_year_pooled_vals, by = c('variable', 'summary_fn')) %>%
        mutate(value_frac_default = value / value_default,
            all_year = if_else(all_year, 'all year', 'event window'),
            alaska_pooled_by_all_year = factor(
                paste(alaska_pooled, all_year, sep = ', '),
                levels = c('Pooled, all year', 'Alaska-only, all year',
                          'Pooled, event window', 'Alaska-only, event window')),
            variable = gsub('(consumption|sales price)', '\n\\1', OUTCOME_VARS[variable],
                            perl = TRUE),
            variable = factor(variable, levels = c("Cars sold", "Sale total",
                "Average \nsales price", "MSRP", "Fuel \nconsumption"))
            )
    proper_summary_fn <- c('mean' = 'Mean', 'sd' = 'Standard deviation')

    comparison_plot <- ggplot(to_plot,
        aes(x = variable, y = value_frac_default, color = alaska_pooled_by_all_year)) +
        # geom_bar(stat = 'identity', position = 'dodge') +
        geom_point(alpha = 0.7, size = 3.5, shape = 18) +
        # TODO: consider using shape on alaska_pooled_by_all_year too.
        # geom_point(alpha = 0.7, size = 3.5) +
        # geom_jitter(height = 0, width = 0.05) +
        facet_grid(summary_fn ~ .,# scales = 'free_y',
            labeller = labeller(summary_fn = proper_summary_fn)) +
        ylim(c(0, NA)) +
        labs(x = '', y = 'Value relative to pooled, all year', color = '' ) +
        scale_color_manual(values = PALETTE_8_COLOR_START_WITH_BLACK) +
        scale_shape_manual(values = c(18, 21, 0, 4)) +
        PLOT_THEME +
        guides(shape = 'none') +
        theme(panel.grid.major.y =  element_line(color = 'gray75', lineend = 'round'),
        legend.position = 'bottom',
        legend.margin = margin(t = 0.15, r = 2, b = 0, l = 0, unit = "cm")
        )
    save_plot(comparison_plot, 'comparison_alaska_vs_pooled_mean_sd.pdf')
    invisible(to_plot)
}


generate_snippets <- function() {
    summary_stats <- pull_alaska_vs_pooled_mean_sd()
    # Don't be lazy here because there's not point. We've already expensively calcuated
    # summary_stats, so might as well use it.
    snippet <- purrr::partial(make_snippet, lazy = FALSE)
    # standard deviation of sales volumes in thousands, weekly
    summary_stats %>% filter(variable == 'sale_tot', summary_fn == 'sd',
        all_year == TRUE, alaska_pooled == 'Pooled') %>%
        mutate(value = value / 1000) %>%
        extract2('value') %>%
        snippet('sales_tot_weekly_thousands_std_dev.tex')

    # standard deviation of sales counts, weekly
    summary_stats %>% filter(variable == 'sale_count', summary_fn == 'sd',
        all_year == TRUE, alaska_pooled == 'Pooled') %>%
        extract2('value') %>%
        snippet('sales_count_weekly_std_dev.tex')

    # Repeat for window only
    summary_stats %>% filter(variable == 'sale_tot', summary_fn == 'sd',
        all_year == FALSE, alaska_pooled == 'Pooled') %>%
        mutate(value = value / 1000) %>%
        extract2('value') %>%
        snippet('sales_tot_weekly_thousands_std_dev_window_only.tex', dollars = TRUE)
    summary_stats %>% filter(variable == 'sale_count', summary_fn == 'sd',
        all_year == FALSE, alaska_pooled == 'Pooled') %>%
        extract2('value') %>%
        snippet('sales_count_weekly_std_dev_window_only.tex', sig_figs = 3)

    # Do pooled means for all year
    summary_stats %>% filter(variable == 'sale_tot', summary_fn == 'mean',
        all_year == TRUE, alaska_pooled == 'Pooled') %>%
        mutate(value = value / 1000) %>%
        extract2('value') %>%
        snippet('sales_tot_weekly_thousands_mean.tex', dollars = TRUE)
    summary_stats %>% filter(variable == 'sale_count', summary_fn == 'mean',
        all_year == TRUE, alaska_pooled == 'Pooled') %>%
        extract2('value') %>%
        snippet('sales_count_weekly_mean.tex', sig_figs = 3)
}


make_control_state_comparison_table <- function(control_states =
        c("VT", "WA", "WY", "MT", "IL", "NM", "NH", "WV", "WI", "DC")) {
    states_deomographics_base <- states %>%
        filter(state %in% c('AK', control_states),
               gdp_method == "NAICS (new)",
               between(year, 2002, 2014)) %>%
        select(state, year, population, gdp_pc) %>%
        ensure_id_vars(state, year) %>%
        mutate(alaska_pooled = if_else(state == 'AK', 'Alaska-only', 'Pooled')) %>%
        group_by(alaska_pooled)
    states_deomographics_means <- states_deomographics_base %>%
        summarize(population = mean(population), gdp_pc = mean(gdp_pc)) %>%
        ungroup() %>%
        collect() %>%
        reshape2::melt(id.vars = 'alaska_pooled') %>%
        mutate(summary_fn = 'mean')
    states_deomographics_sd <- states_deomographics_base %>%
        summarize(population = sd(population), gdp_pc = sd(gdp_pc)) %>%
        ungroup() %>%
        collect() %>%
        reshape2::melt(id.vars = 'alaska_pooled') %>%
        mutate(summary_fn = 'sd')
    states_deomographics <- bind_rows(states_deomographics_means,
        states_deomographics_sd)

    summary_stats <- pull_alaska_vs_pooled_mean_sd() %>%
        # select all year, rather than just the window
        filter(all_year == TRUE) %>% select(-all_year) %>%
        bind_rows(states_deomographics)
    #
    # mpg_to_L100km_coef <- (100 * 3.785411784 / 1.609344)  # 3.7 L/gal, 1.6 km/mi
    # sales_stats <- auctions %>%
    #     select(buy_state, sale_date, sales_pr) %>%
    #     filter(buy_state %in% c('AK', control_states)) %>%
    #     add_sale_year() %>%
    #     group_by(sale_year, buy_state) %>%
    #     summarize(sales_pr_mean = mean(sales_pr),
    #               sales_pr_mean_log = mean(ln(sales_pr)),
    #               annual_sale_count = n()) %>%
    #     ungroup() %>%
    #     collapse()
    #
    # fuel_cons_stats <- auctions %>%
    #     select(sale_date, buy_state, vin_pattern) %>%
    #     filter(buy_state %in% c('AK', control_states)) %>%
    #     add_sale_year() %>%
    #     inner_join(vin_decoder, by = 'vin_pattern') %>%
    #     group_by(sale_year, buy_state) %>%
    #     summarize(fuel_cons = mean(mpg_to_L100km_coef / combined)) %>%
    #     ungroup() %>%
    #     collapse()
    #
    # all_stats <- full_join(sales_stats, fuel_cons_stats,
    #     by = c('sale_year', 'buy_state')) %>%
    #     left_join(states_deomographics, by = c('buy_state' = 'state', 'sale_year' = 'year')) %>%
    #
    #     collect(n = Inf)

    # Make a table, then remove the header rows
    label_vec <- c('sale_tot' = 'Sale total',
                   'sale_tot_log' = 'Log sale total',
                   'sale_count' = 'Cars sold',
                   'sale_count_log' = 'Log cars sold',
                   'sales_pr_mean' = 'Average sales price',
                   'sales_pr_mean_log' = 'Average log sales price',
                   'fuel_cons' = 'Fuel consumption (L/100km)',
                   'fuel_cons_log' = 'Log fuel consumption',
                   'msrp_mean' = 'MSRP',  # Nominal MSRP
                   'msrp_mean_log' = 'Log MSRP',  # Nominal MSRP
                   'population' = 'Population (thousands)',
                   'gdp_pc' = 'GDP per capita'
    )

    table_order = c('sale_count', 'fuel_cons','sales_pr_mean', 'msrp_mean', 'sale_tot', 'population', 'gdp_pc')
    stats_table <- summary_stats %>%
      mutate(value = if_else(variable == 'population', value / 1e3, value),
             value = if_else(variable == 'sale_tot',   value / 1e6, value),
             # variable = renaming_vec[variable],
             alaska_pooled_summary_fn = paste(alaska_pooled, summary_fn)) %>%
      select(-summary_fn,-alaska_pooled) %>%
      tidyr::spread_(key_col = 'alaska_pooled_summary_fn', value_col = 'value') %>%
      mutate(Difference = `Alaska-only mean` - `Pooled mean`) %>%
      # need proper R names to use mutate_if, but make.names adds periods
      setNames(gsub('.', '_', make.names(names(.)), fixed = TRUE)) %>%
      # make all numeric into character so I can more easily make some cells blank
      mutate_if(is.numeric, as.character) %>%
      mutate(Alaska_only_sd = if_else(variable %in% c('gdp_pc', 'population'), '', Alaska_only_sd),
             Pooled_sd = if_else(variable %in% c('gdp_pc', 'population'), '', Pooled_sd),
             variable = factor(variable, levels = table_order, labels = label_vec[table_order])) %>%
      arrange(variable)

    make_tabular(stats_table) %>%
        make_snippet('outcome_means_comparison_table_contents.tex', lazy = FALSE)

    invisible(stats_table)
}


make_all_plot_types <- function(outcome, aggregation_level = 'weekly', verbose = FALSE) {
    # Note that daily is much slower because there are a lot more regressions to run.
    if (verbose) {
        message(sprintf('Making plots for %s', outcome))
    }
    plot_effects_by_anticipation(outcome, aggregation_level, title = FALSE)

    if (aggregation_level == 'daily') {
        plot_effects_individual_period(outcome, 'daily',
            fixed_effects = c('sale_year', 'sale_dow'))
    } else {
        plot_effects_individual_period(outcome, aggregation_level)
        # Don't bother to run this for daily; in the current state there are too many bubbles
        # for anything to be visible.
        run_dd_pick_max_effect(outcome, aggregation_level = aggregation_level)
    }
    invisible(NULL)
}


run_dd_simplest <- purrr::partial(run_dd, years = 2002:2014, aggregation_level = 'weekly',
    anticipation_window = c(-1, -1),
    fixed_effects = c('sale_year', 'buy_state'), cluster = 'buy_state_X_sale_year',
    controls = c('post', 'alaskan_buyer_post', 'pfd_payment_X_alaska'))

stargazer <- purrr::partial(stargazer::stargazer, omit.stat = c("f", "ser"),
                            table.placement = "htbp", label = sample(letters, 10),
                            dep.var.caption = "", column.sep.width = "0pt")

# Now dropping some of the log regressions.
all_outcomes <- c(
    'sale_tot', #'sale_tot_log',
    'sale_count', 'sale_count_log',
    'sales_pr_mean', #'sales_pr_mean_log',
    'msrp_mean')#, 'msrp_mean_log')
# ~4 minutes for first run (if weekly, ~5 hours if daily)  (timings are old)
print(system.time(lapply(all_outcomes, make_all_plot_types)))
simple_dd_results <- purrr::map(all_outcomes, function(outcome) {
    run_dd_simplest(outcome = outcome)
    })
simple_dd_results_df <- purrr::map_df(seq_along(all_outcomes), function(i) {
  broom::tidy(simple_dd_results[[i]]) %>% mutate(outcome = all_outcomes[i])
})

stargazer(simple_dd_results)  # copy and paste this (Karl is lazy)
plot_alaska_vs_pooled_mean_sd()
# generate_snippets is fast, as long as find_match_states_crude and
# get_state_by_time_variation have been run.
generate_snippets()
make_control_state_comparison_table()
# quality_control_graphs()
# plot_dd_sales(2002)

# plot_alaska_sales()
# df <- run_dd_one_year(2003)
