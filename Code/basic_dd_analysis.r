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
#         collect(n=Inf)
# }

if (! existsFunction('get_top_auction_states_ak_buyers') ||
    ! is.memoised(get_top_auction_states_ak_buyers)) {
    get_top_auction_states_ak_buyers <- function(top_n) {
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

    get_top_auction_states_ak_buyers <- memoise(get_top_auction_states_ak_buyers)
}


get_top_auction_states_table <- function(top_n, buy_state_code) {
    filter(auctions, buy_state == buy_state_code) %>%
        filter(! is.na(auction_state)) %>%
        group_by(auction_state) %>%
        summarize(count = n()) %>%
        arrange(-count) %>%
        head(top_n) %>%
        return()
}


pull_data_one_year <- function(year, days_before=30L, days_after=days_before,
                               top_n_auction_states=NULL) {
    # top_n_auction_states limits the data returned to auction states in the top n,
    # for alaskan buyers (default of NULL doesn't limit)

    data_one_year <- auctions %>%
        select(sale_date, model_yr, sales_pr, bid_ct,
               veh_type, buy_state, sell_state, auction_state,
               # seller_type, slrdlr_type,
               buyer_id, seller_id) %>%
        filter_event_window(year = year, days_before = days_before,
                            days_after = days_after) %>%
        filter(! is.na(buy_state))

    if (! is.null(top_n_auction_states)) {
        top_states_table <- get_top_auction_states_table(top_n_auction_states, 'AK')
        data_one_year <- semi_join(data_one_year, top_states_table, by = 'auction_state')
    }

    # stopifnot(tbl_has_rows(data_one_year))
    return(data_one_year)
}


get_sales_counts <- function(df_base) {
    # dplyr bug means this doesn't work:
    # See https://github.com/hadley/dplyr/issues/2290
    # sales_counts <- df_base %>% group_by(sale_date, buyer_id) %>%
    #     summarize(sale_count = n(),
    #               sale_tot = sum(sales_pr),
    #               buy_state = first(buy_state),
    #               alaskan_buyer = first(alaskan_buyer),
    #               post_dividend = first(post_dividend)) %>%
    #     collect()
    # Instead, do the aggregation, then join buy_state back in.
    # (the collapse() here tells dplyr to think hard about the sql query (but not
    # actually go and process in the database), preventing it from getting confused in
    # the merge any trying to rename sale_date to sale_date.y.)

    sales_counts <- df_base %>% group_by(sale_date, buyer_id) %>%
        summarize(sale_count = n(), sale_tot = sum(sales_pr)) %>% collapse()
    # We've previously ensured that each buyer_id has at most one state.
    buyer_info <- df_base %>% distinct(buyer_id, buy_state) %>% collapse()
    sales_counts <- inner_join(sales_counts, buyer_info, by = 'buyer_id') %>% collapse()
    return(sales_counts)
        # %>%
        #
        # mutate(alaskan_buyer = factor(buy_state == 'AK', levels=c(TRUE, FALSE),
        #                               labels=c('Alaskan', 'Non-Alaskan')),
        #
        #        post_dividend = factor(sale_date >= dividend_day))
}


adjust_per_capita <- function(.data, na.rm = TRUE) {
    state_pop <- states %>%
        select(state, year, population) %>%
        mutate(population = population / 1e6) %>%
        compute()
    out <- .data %>% add_sale_year() %>%
        left_join(state_pop, by = c('buy_state' = 'state', 'sale_year' = 'year')) %>%
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
        get_sales_counts() %>%
        # NB: This is sale count and sale total per *million* people
        adjust_per_capita() %>%
        mutate(alaskan_buyer_post = buy_state == 'AK' & sale_date >= dividend_day) %>%
        select(alaskan_buyer_post, sale_count, sale_tot, sale_count_pc, sale_tot_pc, sale_date, buy_state) %>%
        collect(n = row_limit)
    return(sales_counts)
}
if (! existsFunction('sales_counts_one_year')) {
    sales_counts_one_year <- memoise(sales_counts_one_year_unmemoized)
}


run_dd_one_year <- function(year) {
    stopifnot( (! missing(year)) && (length(year) == 1) && (is.numeric(year)) )
    dividend_day <- first_thursday_in_october(year)

    sales_counts <- sales_counts_one_year(year, days_before = 30) #%>% mutate(sale_week = lubridate::week(sale_date))

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
    # sales_counts_reg2 <- felm(formula=sales_counts_formula2, data=sales_counts)
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
            collect(n=Inf)
        buyer_id_count_plot <- ggplot(to_plot, aes(x=buyer_id_days)) +
            geom_histogram(bins=30) +
            labs(x = sprintf('Number of days each buyer is present (%s day period)',
                             total_days),
                 y = 'Count', title = sprintf('Buyer counts for %s', year)) +
            PLOT_THEME
        return(buyer_id_count_plot)
    }

    total_sales_by_day <- auctions %>% group_by(sale_date) %>%
        summarize(daily_sales = sum(sales_pr), count = n()) %>% collect(n=Inf)
    sales_by_month_plt <- total_sales_by_day %>%
        mutate(sale_year  = lubridate::year(sale_date),
               sale_month = lubridate::month(sale_date)) %>%
        group_by(sale_year, sale_month) %>%
        summarize(total_sales = sum(daily_sales)/1e9, sale_date = first(sale_date)) %>%
        ggplot(aes(x=sale_date, y=total_sales)) +
        geom_line() +
        labs(x='Date', y='Monthly total ($ billions)') +
        ylim(c(0, NA)) +
        PLOT_THEME

    sales_by_week_plt <- total_sales_by_day %>%
        mutate(sale_year = lubridate::year(sale_date),
               sale_week = lubridate::week(sale_date)) %>%
        group_by(sale_year, sale_week) %>%
        summarize(total_sales = sum(daily_sales)/1e9, sale_date = first(sale_date)) %>%
        ggplot(aes(x=sale_date, y=total_sales)) +
        geom_line() +
        labs(x='Date', y='Weekly total ($ billions)') +
        ylim(c(0, NA)) +
        PLOT_THEME

    total_sales_by_day_ak_vs <- auctions %>%
        filter(! is.na(buy_state)) %>%
        tag_alaskan_buyer() %>%
        group_by(sale_date, alaskan_buyer) %>%
        summarize(daily_sales = sum(sales_pr), count = n()) %>% collect(n=Inf)
    thursdays_int <- total_sales_by_day_ak_vs %$% sale_date %>%
        lubridate::year() %>% unique() %>% sort() %>%
        first_thursday_in_october() %>%
        as.integer()
    sales_by_month_plt_ak_vs <- total_sales_by_day_ak_vs %>%
        mutate(sale_year  = lubridate::year(sale_date),
               sale_month = lubridate::month(sale_date)) %>%
        group_by(sale_year, sale_month, alaskan_buyer) %>%
        summarize(total_sales = sum(daily_sales)/1e6, sale_date = first(sale_date)) %>%
        ggplot(aes(x=sale_date, y=total_sales)) +
        geom_line() +
        geom_vline(xintercept=thursdays_int, color='red3', alpha=0.3) +
        facet_grid(alaskan_buyer ~ ., scales='free_y') +
        labs(x='Date', y='Monthly total ($ millions)') +
        ylim(c(0, NA)) +
        PLOT_THEME
    save_plot(sales_by_month_plt_ak_vs, 'total_sales_monthly_total_AK_vs.pdf')
    sales_by_week_plt_ak_vs <- total_sales_by_day_ak_vs %>%
        mutate(sale_year = lubridate::year(sale_date),
               sale_week = lubridate::week(sale_date)) %>%
        group_by(sale_year, sale_week, alaskan_buyer) %>%
        summarize(total_sales = sum(daily_sales)/1e6, sale_date = first(sale_date)) %>%
        ggplot(aes(x=sale_date, y=total_sales)) +
        geom_line() +
        geom_vline(xintercept=thursdays_int, color='red3') +
        facet_grid(alaskan_buyer ~ ., scales='free_y') +
        labs(x='Date', y='Weekly total ($ millions)') +
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


plot_dd_sales <- function(years = 2002:2014) {

    sales_near_windows <- lapply_bind_rows(years, sales_counts_one_year,
            days_before = 60, top_n_auction_states = 10) %>%
        filter(buy_state %in% c('AK', 'WA', 'NV', 'OR')) %>%
        # tag_alaskan_buyer(as_factor = TRUE) %>%
        # Regression-adjust for weekly patterns. sale_tot_pc and sale_count_pc become
        # the residuals after regressing on dummies for day of week across all included
        # states. NB: This is sale count and sale total per *million* people (otherwise
        # the numbers are really small and R struggles with numerical precision)
        # winsorize(c('sale_count', 'sale_tot')) %>%
        adjust_by_state_and_weekday(sale_count) %>%
        adjust_by_state_and_weekday(sale_tot) %>%
        select(sale_date, buy_state, sale_count, sale_count_pc,
               sale_tot, sale_tot_pc) %>%
        group_by(sale_date, buy_state) %>%
        summarise_all(mean) %>%
        ungroup() %>%  # avoid dplyr grouping error
        add_event_time() %>%
        add_sale_year()

    # sales_near_windows_avg <- sales_near_windows %>%
    #     select(event_time, alaskan_buyer, sale_count_pc, sale_tot_pc) %>%
    #     group_by(event_time, alaskan_buyer) %>%
    #     summarise_all(mean)

    common_labs <- labs(x = 'Event time (days)', color = 'State')
    sale_count_year_facet_plot <- sales_near_windows %>%
        ggplot(aes(x = event_time, y = sale_count, color = buy_state)) +
        geom_point(alpha = 0.3) +
        geom_smooth(method = 'loess', span = 0.2, se = FALSE) +
        facet_grid(sale_year ~ ., scales = 'free_y') +
        common_labs +
        labs(y = 'Sales counts (residualized by state and day of week)') +
        PLOT_THEME
    sale_tot_year_facet_plot <- sales_near_windows %>%
        ggplot(aes(x = event_time, y = sale_tot/1000, color = buy_state)) +
        geom_point(alpha = 0.3) +
        geom_smooth(method = 'loess', span = 0.2, se = FALSE) +
        facet_grid(sale_year ~ ., scales = 'free_y') +
        common_labs +
        labs(y = 'Sales volume ($1000s, residualized by state and day of week)') +
        PLOT_THEME

    save_plot(sale_count_year_facet_plot, 'dd_plot_sales_counts_year_facet.pdf',
              scale_mult = 3)
    save_plot(sale_tot_year_facet_plot, 'dd_plot_sales_volume_year_facet.pdf',
              scale_mult = 3)
}


# quality_control_graphs()
# plot_dd_sales(2002)

# plot_alaska_sales()
# df <- run_dd_one_year(2003)
