source('r_defaults.r')
# library(RPostgreSQL)
install_lazy(c('dplyr', 'ggplot2', 'magrittr', 'lfe', 'memoise', 'lubridate', 'purrr'),
             verbose = FALSE)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
library(magrittr)
suppressPackageStartupMessages(library(lfe))
library(memoise)

POSTGRES_DB <- 'second_year_paper'
POSTGRES_CLEAN_TABLE <- 'auctions_cleaned'
if (! exists('con')) {
    pg_user <- Sys.info()[["user"]] %>% tolower()
    con <- src_postgres(POSTGRES_DB, user = pg_user, password = pg_user)
}
auctions <-  tbl(con, POSTGRES_CLEAN_TABLE)
states <- tbl(con, 'states')

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


get_sales_counts <- function(df_base, date_var = 'sale_date', id_var = 'buyer_id') {
    # Aggregate sale counts and sales_pr at some level of date and ID.
    stopifnot(length(date_var) == 1, length(id_var) == 1,
              date_var %in% c('sale_date', 'sale_week', 'event_time', 'event_week'),
              # other IDs are possible, but haven't been written yet:
              id_var %in% c('buyer_id', 'buy_state', 'sell_state', 'auction_state'))

    if (startsWith(date_var, 'event')) {
        df_base <- df_base %>% add_sale_year()
        group_vars <- c(id_var, 'sale_year', date_var)
    } else {
        group_vars <- c(id_var, date_var)
    }
    sales_counts <- df_base %>% group_by_(.dots = group_vars) %>%
        summarize(sale_count = n(), sale_tot = sum(sales_pr)) %>%
        ungroup() %>% collapse()

    if (id_var == 'buyer_id') {  # Merge back in buy_state.
        # We've previously ensured that each buyer_id has at most one state.

        # dplyr bug (#2290) means this doesn't work:
        # sales_counts <- df_base %>% group_by(sale_date, buyer_id) %>%
        #     summarize(sale_count = n(),
        #               sale_tot = sum(sales_pr),
        #               buy_state = first(buy_state),
        #               alaskan_buyer = first(alaskan_buyer),
        #               post_dividend = first(post_dividend)) %>%
        #     collect()
        # Instead, do the aggregation, then join buy_state back in.
        #
        # (the collapse() here tells dplyr to think hard about the sql query (but not
        # actually go and process in the database), preventing it from getting confused in
        # the merge any trying to rename sale_date to sale_date.y.)
        buyer_info <- df_base %>% ungroup() %>%
            distinct(buyer_id, buy_state) %>% collapse()
        sales_counts <- inner.join(sales_counts, buyer_info, by = 'buyer_id') %>%
            collapse()
    } else if (id_var == 'seller_id') {
        stop("Not implemented yet.")
    }
    return(sales_counts)
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
    sales_counts_reg <- felm(formula = sales_counts_formula, data = sales_counts)
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
    # sales_counts_reg2 <- felm(formula = sales_counts_formula2, data = sales_counts)
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
        felm(data = sales_near_windows) %>%
        residuals() %>%
        as.numeric()
    sale_volume_reg_dow_resid <- (sale_volume ~ 0 | sale_dow) %>%
        felm(data = sales_near_windows) %>%
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
    resid <- felm(reg_formula, df) %>%
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
    control_states <- find_match_states_crude()
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

aggregate_sales_dd_unmemoized <- function(years, agg_var, days_before = 70) {
    # Create the dataset for the difference-in-differences analysis.
    # This is split out in a separate function before creating the ancitipation variables
    # so it can be cached and we quickly test different values of the anticipation
    # window.

    stopifnot(is.numeric(years), length(years) >= 1,
              is.character(agg_var), length(agg_var) == 1)
    control_states <- find_match_states_crude()

    df <- auctions %>%
        select(sale_date, buy_state, sales_pr) %>%
        filter_event_window(years = years, days_before = days_before) %>%
        filter(buy_state %in% c('AK', control_states)) %>%
        add_event_time() %>%
        get_sales_counts(date_var = agg_var, id_var = 'buy_state') %>%
        tag_alaskan_buyer() %>%  # Make TRUE/FALSE alaskan_buyer variable
        collect(n = Inf)
    return(df)
}
if (!existsFunction('aggregate_sales_dd')) {
    aggregate_sales_dd <- memoize(aggregate_sales_dd_unmemoized)
}


run_dd <- function(years = 2002:2014,
    aggregation_level = 'daily',
    # How many days/weeks relative to the event day do we think dealers are anticipating?
    # NB: if aggregation_level is daily, anticipation_window is in days. If
    # aggregation_level is weekly, anticipation_window is in weeks. Zero is the day/week
    # when the dividend is sent (so -1 is the last period before the dividend).
    anticipation_window = NULL,
    outcomes = c('sale_count', 'sale_tot'),
    # controls, including DD vars, excluding fixed effects
    controls = c('alaskan_buyer', 'anticipation', 'alaskan_buyer_anticipation', 'post',
                 'alaskan_buyer_post'),
    # Which fixed effects should we have?
    fixed_effects = c('sale_year', 'buy_state'),
    # what clusters should we have?
    # (remember that cluster covariance relies on asymptotics!)
    clusters = 0,
    days_before_limit = 70
    ) {

    stopifnot(aggregation_level %in% c('daily', 'weekly'),
              length(outcomes) >= 1,
            #   length(controls) == 1,
            #   length(fixed_effects) == 1,
            #   length(clusters) == 1,
              length(years) >= 1
              )
    if (is.null(anticipation_window)) {
        if (aggregation_level == 'daily') {
            anticipation_window <- c(-14, -1)
        } else if (aggregation_level == 'weekly') {
            anticipation_window <- c(-2, -1)
        }
    }
    stopifnot(length(anticipation_window) == 2, all(anticipation_window <= 0),
              anticipation_window[1] <= anticipation_window[2],
              (-days_before_limit) < anticipation_window[1])

    pull_dd_data <- function() {
        # This is a sub-function for organizational ease, but it relies heavily on
        # variables defined in the parent function. Be careful before moving it.

        # look up varname (will error if aggregation_level is not in there)
        agg_var <- c(daily = 'event_time', weekly = 'event_week')[[aggregation_level]]
        # grouping_vars <- c('sale_year', 'buy_state', agg_var)

        # Set up mutate_ calls.
        # Create two variables, anticipation_period and post_period.  anticipation_period
        # will be true for rows where agg_var (event_time or event_week) is between
        # within anticipation_window, and post_period is true when it's greater.
        mutate_calls_time <- list(
            anticipation = lazyeval::interp(~ between(agg_var, low, high),
                agg_var = as.name(agg_var), low = anticipation_window[1],
                high = anticipation_window[2]),
            post = lazyeval::interp(~ agg_var > high,
                agg_var = as.name(agg_var), high = anticipation_window[2])
            )
        aggregate_sales_dd(years, agg_var, days_before = days_before_limit) %>%
            # Make these variables even if they're not used (it's fast)
            mutate_(.dots = mutate_calls_time) %>% # Make anticipation and post
            mutate(alaskan_buyer_anticipation = alaskan_buyer & anticipation,
                   alaskan_buyer_post = alaskan_buyer & post) %>%
            collect(n = Inf) %>%
            return()
    }

    # Avoid collinearity
    if ('buy_state' %in% fixed_effects) {
            controls <- setdiff(controls, 'alaskan_buyer')
    }
    df <- pull_dd_data()  # finally collect the data

    # Make the felm multi-part formula, something like
    # sale_tot | sale_count ~ anticipation + alaskan_buyer_anticipation + post +
    #     alaskan_buyer_post | buy_state + sale_year | 0 | buy_state
    # Using paste like this reduces some flexibility for interactions, but I can just
    # create those variables manually.
    reg_formula <- paste(paste(outcomes, collapse = ' | '), '~',
                         paste(controls, collapse = ' + '), '|',
                         paste(fixed_effects, collapse = ' + '),
                         '| 0 |',  # no IV vars
                         paste(clusters, collapse = ' + ')) %>% as.formula()
    reg_results <- felm(reg_formula, df)
    return(reg_results)
}


plot_effects_by_anticipation_variable_start_and_end <- function(outcome,
        aggregation_level = 'daily', days_before_limit = 70) {
    stop("This is an old function.  You probably don't want it.")
    if (aggregation_level == 'daily') {
        loop_start <- (-days_before_limit) + 1
        min_window_length <- 7
    } else if (aggregation_level == 'weekly'){
        loop_start <- ((-days_before_limit) %/% 7) + 1
        min_window_length <- 1
    } else {
        stop("aggregation_level must be 'daily' or 'weekly'")
    }
    stopifnot(outcome %in% c('sale_count', 'sale_tot'), days_before_limit > 2)

    windows <- purrr::cross2(
        # Form the cross of all possible window starts
        seq.int(loop_start, -1, by = 1),
        # crossed by all possible window ends:
        seq.int(loop_start, -1, by = 1),
        .filter = function(start, end) {
            # and then filter combos we don't want
            end - start < min_window_length
        }
    )
    get_results_one_window <- function(start_end_list) {
        start <- start_end_list[[1]]
        end <- start_end_list[[2]]
        reg_results <- run_dd(outcomes = outcome, aggregation_level = aggregation_level,
            anticipation_window = c(start, end), days_before_limit = days_before_limit)

        # rse is apparently the robust standard error, though it's not well documented.
        # e.g. identical(sqrt(diag(reg_results$robustvcv)), reg_results$rse)
        df <- data_frame(start = start, end = end,
            coef = reg_results$coefficients['alaskan_buyer_anticipationTRUE', ],
            se   = reg_results$rse[['alaskan_buyer_anticipationTRUE']],
            pval = reg_results$rpval[['alaskan_buyer_anticipationTRUE']])
        return(df)

    }

    to_plot <- purrr::map_df(windows, get_results_one_window)

    # Define a bunch of labels.
    lab_x <- 'Window end (%s)'
    lab_y <- 'Window start (%s)'
    if (aggregation_level == 'daily') {
        lab_x <- sprintf(lab_x, 'days')
        lab_y <- sprintf(lab_y, 'days')
    } else if (aggregation_level == 'weekly') {
        lab_x <- sprintf(lab_x, 'weeks')
        lab_y <- sprintf(lab_y, 'weeks')
    } else {
        stop("Error!")
    }
    if (outcome == 'sale_tot') {
        lab_color_coef = 'Estimated additional Alaskan anticipation cars sold'
        lab_color_se = 'SE on additional Alaskan anticipation cars sold'
    } else if (outcome == 'sale_count') {
        lab_color_coef = 'Estimated additional Alaskan anticipation sale volume'
        lab_color_se = 'SE on additional Alaskan anticipation sale volume'
    } else {
        stop("Error!")
    }

    coef_plot <- ggplot(to_plot, aes(x = end, y = start, fill = coef)) +
        geom_tile() +
        scale_fill_distiller(palette = 'RdBu') +
        labs(x = lab_x, y = lab_y, color = lab_color_coef) +
        PLOT_THEME
    se_plot <- ggplot(to_plot, aes(x = end, y = start, fill = se)) +
        geom_tile() +
        scale_fill_distiller(palette = 'RdBu') +
        labs(x = lab_x, y = lab_y, color = lab_color_se) +
        PLOT_THEME

    save_plot(coef_plot, sprintf('anticipation_window_%s_tile_coef.pdf', outcome))
    save_plot(se_plot,   sprintf('anticipation_window_%s_tile_se.pdf', outcome))

    return(to_plot)
}


get_state_by_time_variation_unmemoized <- function(aggregation_level = 'daily',
        winsorize_pct = NULL, states_included = NULL) {

    # Parameters:
    # aggregation_level: the aggregation level of the counts and dollar amounts.
    #   daily/weekly, defaults to daily.
    # winsorize_pct: the amount by which the aggregated variables (counts or $) should be
    #   winsorized before calculating the std dev. Defaults to no winsorization.
    # states_included: the states included in the standard deviation calculation.
    #   Defaults to Alaska and the control states from find_match_states_crude().
    #   Note that the outcomes are demeaned by state before calculating the std dev.

    if (aggregation_level == 'daily') {
        time_var <- 'sale_date'
    } else if (aggregation_level == 'weekly') {
        time_var <- 'sale_week'
    } else {
        stop("Bad aggregation_level, should be daily or weekly.")
    }

    if (is.null(states_included)) {
        # What states get included in the std dev calculations?
        control_states <- find_match_states_crude()
        states_included <- c('AK', control_states)
    }
    df <- auctions %>% select(sale_date, buy_state, sales_pr)
    if (length(states_included) <= 1) {
        # Necessary because if states_included has length 1, we encounter dplyr bug #511.
        df <- df %>% filter(buy_state == states_included)
    } else {
        df <- df %>% filter(buy_state %in% states_included)
    }
    if (time_var == 'sale_week') {
        # NB: This is not exactly the same as event weeks
        df <- df %>% mutate(sale_week = date_part('week', sale_date))
    }
    df <- df %>% get_sales_counts(date_var = time_var, id_var = 'buy_state') %>%
        # Then demean so we're not getting huge standard deviations by looking across
        # states
        group_by(buy_state) %>%
        mutate(sale_tot = sale_tot - mean(sale_tot),
               sale_count = sale_count - mean(sale_count)) %>%
        ungroup()
    if (! is.null(winsorize_pct)) {
        df <- df %>% winsorize(c('sale_tot', 'sale_count'), winsorize_pct)
    }
    df <- df %>% summarize(sale_tot_sd = sd(sale_tot), sale_count_sd = sd(sale_count)) %>%
        collect()

    return(df)
}
# TODO: uncomment this back
# if (! existsFunction('get_state_by_time_variation')) {
    get_state_by_time_variation <- memoize(get_state_by_time_variation_unmemoized)
# }
get_state_by_time_variation <- get_state_by_time_variation_unmemoized


plot_effects_by_anticipation <- function(outcome,
        aggregation_level = 'daily', days_before_limit = 70, title = TRUE) {

    if (aggregation_level == 'daily') {
        loop_start <- (-days_before_limit) + 1
        min_window_length <- 7
    } else if (aggregation_level == 'weekly'){
        loop_start <- ((-days_before_limit) %/% 7) + 1
        min_window_length <- 1
    } else {
        stop("aggregation_level must be 'daily' or 'weekly'")
    }
    stopifnot(outcome %in% c('sale_count', 'sale_tot'), days_before_limit > 2,
              loop_start < min_window_length)


    get_results_one_window <- function(start) {
        reg_results <- run_dd(outcomes = outcome, aggregation_level = aggregation_level,
            anticipation_window = c(start, -1), days_before_limit = days_before_limit)

        # rse is apparently the robust standard error, though it's not well documented.
        # e.g. identical(sqrt(diag(reg_results$robustvcv)), reg_results$rse)
        df <- data_frame(start = start,
            coef = reg_results$coefficients['alaskan_buyer_anticipationTRUE', ],
            se   = reg_results$rse[['alaskan_buyer_anticipationTRUE']],
            pval = reg_results$rpval[['alaskan_buyer_anticipationTRUE']])
        return(df)

    }
    windows <- seq.int(loop_start, -min_window_length, by = 1)
    to_plot <- purrr::map_df(windows, get_results_one_window)
    # For weekly, force ggplot to label all weeks, rather than having ridiculous
    # half-weeks.
    if (aggregation_level == 'weekly') {
        to_plot <- to_plot %>% mutate(start = factor(start))
    }


    sale_tot_divisor <- 1000
    if (outcome == 'sale_tot') {
        to_plot <- to_plot %>% mutate(coef = coef / sale_tot_divisor,
                                      se = se / sale_tot_divisor)
    }
    to_plot <- to_plot %>% mutate(conf95_lower = coef - (1.96 * se),
                                  conf95_upper = coef + (1.96 * se))

    # Now also grab the std dev of the sample we're looking at.
    sd_varname <- paste0(outcome, '_sd')  # either sale_tot_sd or sale_count_sd
    data_sd <- get_state_by_time_variation(aggregation_level)[[sd_varname]]
    control_states <- find_match_states_crude()
    individual_state_std_dev <- lapply_bind_rows(c('AK', control_states),
        get_state_by_time_variation,
        aggregation_level = aggregation_level, winsorize_pct = NULL,
        rbind_src_id = 'state', parallel_cores = 1) %>%
        select_(.dots = c('state', sd_varname)) %>%
        # use a common name regardless of outcome so I don't have to fuss with dplyr and
        # ggplot standard evaluation later
        setNames(c('state', 'outcome_sd'))
    std_devs <- data_frame(state = 'Pooled', outcome_sd = data_sd) %>%
        bind_rows(individual_state_std_dev) %>%
        # Explicitly set the factor and its ordered levels for a better plot legend
        mutate(state = factor(state, levels = c('Pooled', 'AK', control_states),
                              labels = c('Pooled', 'AK', control_states), ordered = TRUE))
    if (outcome == 'sale_tot') {
        # data_sd <- data_sd / sale_tot_divisor
        std_devs <- std_devs %>% mutate(outcome_sd = outcome_sd / sale_tot_divisor)
    #    data_sd_ak_only <- data_sd_ak_only / sale_tot_divisor
    }

    # Define a bunch of labels.
    if (aggregation_level == 'daily') {
        aggregation_level_noun <- 'day'
    } else if (aggregation_level == 'weekly') {
        aggregation_level_noun <- 'week'
    } else {
        stop("Error!")
    }

    if (outcome == 'sale_tot') {
        lab_y <- 'Thousands of dollars per %s'
    } else if (outcome == 'sale_count') {
        lab_y <- 'Cars per %s'
    } else {
        stop("Error!")
    }
    lab_x <- sprintf('Window start (event %ss)', aggregation_level_noun)
    lab_y <- sprintf(lab_y, aggregation_level_noun)

    coef_plot <- ggplot(to_plot, aes(x = start, y = coef)) +
        geom_point() +
        geom_errorbar(aes(ymin = conf95_lower, ymax = conf95_upper)) +
        labs(x = lab_x, y = lab_y, color = 'State\nstd dev') +
        scale_color_manual(values = PALETTE_8_COLOR_START_WITH_BLACK) +
        PLOT_THEME
    if (title){
        coef_plot <- coef_plot + labs(title =
            'Anticipation window treatment coefficient for varying window starts')
        title_pattern <- ''
    } else {
        title_pattern <- '_notitle'
    }
    # Then make the versions with lines for the standard deviations
    # First, the one with a single, pooled std dev
    coef_plot_with_pooled_sd <- coef_plot +
        geom_hline(data = filter(std_devs, state == 'Pooled'),
                   aes(yintercept = outcome_sd, color = state)) +
        theme(legend.position="none")
    # Then, to make sure it's not one state swamping the std dev calculation, do each
    # separately.
    coef_plot_with_states_sd <- coef_plot +
        geom_hline(data = std_devs, aes(yintercept = outcome_sd, color = state))


    filename_part <- sprintf('anticipation_window_%s_%s%s', outcome, aggregation_level,
                             title_pattern)
    save_plot(coef_plot,                paste0(filename_part, '.pdf'))
    save_plot(coef_plot_with_pooled_sd, paste0(filename_part, '_pooled_sd.pdf'))
    save_plot(coef_plot_with_states_sd, paste0(filename_part, '_states_sd.pdf'))
    invisible(to_plot)  # then return the data
}
sale_tot_effects_daily    <- plot_effects_by_anticipation('sale_tot')
sale_count_effects_daily  <- plot_effects_by_anticipation('sale_count')
# sale_tot_effects_weekly   <- plot_effects_by_anticipation('sale_tot', 'weekly')
# sale_count_effects_weekly <- plot_effects_by_anticipation('sale_count', 'weekly')


# quality_control_graphs()
# plot_dd_sales(2002)

# plot_alaska_sales()
# df <- run_dd_one_year(2003)
