source('r_defaults.r')
# library(RPostgreSQL)
library(dplyr)
library(ggplot2)
library(magrittr)
library(lfe)
library(memoise)

POSTGRES_DB <- 'second_year_paper'
POSTGRES_CLEAN_TABLE <- 'auctions_cleaned'
if (! exists('con')) {
    con <- src_postgres(POSTGRES_DB)
}
auctions <-  tbl(con, POSTGRES_CLEAN_TABLE)






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
        top_states <- filter(auctions, buy_state == 'AK') %>%
            filter(! is.na(auction_state)) %>%
            group_by(auction_state) %>%
            summarize(count = n()) %>%
            arrange(-count) %>%
            head(top_n) %>%
            collect() %$%
            auction_state
        return(top_states)
    }

    get_top_auction_states_ak_buyers <- memoise(get_top_auction_states_ak_buyers)
}


# could this be more elegant? definitely
first_thursday_in_october <- function(years) {
    first_thursday_in_october_one_year <- function(year) {
        stopifnot(length(year) == 1L)
        current_date <- lubridate::make_date(year, 10, 1)
        # Thursday is weekday 5
        while (lubridate::wday(current_date) != 5) {
            current_date <- current_date + 1
        }
        return(current_date)
    }
    thursdays <- vapply(X = years, FUN = first_thursday_in_october_one_year,
                        FUN.VALUE = as.Date('1970-01-01')) %>%
                as.Date(origin='1970-01-01')
    return(thursdays)
}


tbl_has_rows <- function(df) {
    # Works for both local tables and remote databases
    nrow_df <- nrow(df)
    if (is.na(nrow_df)) {  # nrow() is NA for remote tables
        head1 <- ungroup(df) %>% head(1) %>% collect()
        has_rows <- nrow(head1) > 0
    } else {
        has_rows <-  nrow_df > 0
    }
    return(has_rows)
}

pull_data_one_year <- function(year, days_before=60L, days_after=days_before,
                               top_n_auction_states=NULL) {
    # top_n_auction_states limits the data returned to auction states in the top n,
    # for alaskan buyers
    dividend_day <- first_thursday_in_october(year)
    window_begin <- dividend_day - days_before
    window_end <- dividend_day + days_after
    date_filter_sql <- sprintf(
        "SELECT * FROM %s WHERE (sale_date BETWEEN DATE '%s' and DATE '%s')",
        POSTGRES_CLEAN_TABLE, window_begin, window_end)
    data_one_year <- tbl(con, sql(date_filter_sql)) %>%
        filter(! is.na(buy_state))

    if (! is.null(top_n_auction_states)) {
        top_states <- get_top_auction_states_ak_buyers(top_n_auction_states)
        data_one_year <- filter(data_one_year, auction_state %in% top_states)
    }

    data_one_year <- select(data_one_year, sale_date, model_yr, sales_pr, bid_ct,
                            veh_type, buy_state, sell_state, auction_state,
                            # seller_type, slrdlr_type,
                            buyer_id, seller_id)
        # mutate(alaskan_buyer = buy_state == 'AK',
        #        post_dividend = sale_date >= dividend_day)
    # data_one_year <- compute(data_one_year)
    # explain(data_one_year)
    data_one_year <- collapse(data_one_year)
    # explain(data_one_year)
    stopifnot(tbl_has_rows(data_one_year))
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
    buyer_info <- df_base %>% distinct(buyer_id, buy_state) %>% collapse()
    sales_counts <- inner_join(sales_counts, buyer_info, by='buyer_id') %>% collapse()
    return(sales_counts)
        # %>%
        #
        # mutate(alaskan_buyer = factor(buy_state == 'AK', levels=c(TRUE, FALSE),
        #                               labels=c('Alaskan', 'Non-Alaskan')),
        #
        #        post_dividend = factor(sale_date >= dividend_day))
}


run_dd_one_year <- function(year) {
    stopifnot( (! missing(year)) && (length(year) == 1) && (is.numeric(year)) )
    df_base <- pull_data_one_year(year)
    dividend_day <- first_thursday_in_october(year)
    # calculate sales counts (in sql), don't retrieve more than 10 million results
    sales_counts <- get_sales_counts(df_base) %>%
        mutate(alaskan_buyer_post = buy_state == 'AK' & sale_date >= dividend_day) %>%
        collect(n=1e7)
    # DD with LHS of daily sale_count, buyer_id and sale_date FE and buy_state clusters
    sales_counts_formula <- (sale_count ~ alaskan_buyer_post |
                             buyer_id + sale_date | 0 | buy_state)
    sales_counts_reg <- felm(formula=sales_counts_formula, data=sales_counts)
    print(summary(sales_counts_reg))

    fixed_effects <- getfe(sales_counts_reg) %>% as.tbl()
    # To graph fixed effects:
    # fixed_effects <- getfe(sales_counts_reg, se=TRUE) %>% as.tbl()
    # to_graph <- filter(fixed_effects, fe == 'sale_date') %>% mutate(sale_date = as.Date(idx))
    # ggplot(to_graph, aes(x=sale_date, y=effect)) + geom_point() +
    #     geom_vline(xintercept = as.integer(dividend_day)) +
    #     geom_errorbar(aes(ymin=effect - 1.96*clusterse, ymax = effect + 1.96*clusterse)) +
    #     PLOT_THEME + ylim(-1, 1)


    # get the residuals
    # sales_counts_formula2 <- (sale_count ~ 1 | buyer_id)
    # sales_counts_reg2 <- felm(formula=sales_counts_formula2, data=sales_counts)
    # sales_counts_resid2 <- residuals(sales_counts_reg2) %>% as.numeric()
    # print(head(sales_counts_resid2))
    # sales_counts <- mutate(sales_counts, resid_buyer_fe = sales_counts_resid2)
    return(sales_counts)
}


quality_control_graphs <- function() {
    year <- 2014
    event_year <- pull_data_one_year(year)
    total_days <- event_year %>% ungroup() %>% distinct(sale_date) %>%
        tally() %>% collect() %$% n
    to_plot <- event_year %>% ungroup() %>% distinct(sale_date, buyer_id) %>%
        group_by(buyer_id) %>% summarize(buyer_id_days = n()) %>%
        collect(n=Inf)
    buyer_id_count_plot <- ggplot(to_plot, aes(x=buyer_id_days)) +
        geom_histogram(bins=30) +
        labs(x=sprintf('Number of days each buyer is present (%s day period)', total_days),
             y='Count', title=sprintf('Buyer counts for %s')) +
        PLOT_THEME

    total_sales_by_day <- auctions %>% group_by(sale_date) %>%
        summarize(daily_sales = sum(sales_pr)) %>% collect(n=Inf)
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
        summarize(daily_sales = sum(sales_pr)) %>% collect(n=Inf)
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
        geom_vline(xintercept=thursdays_int, color='red', alpha=0.3) +
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
        geom_vline(xintercept=thursdays_int, color='red') +
        facet_grid(alaskan_buyer ~ ., scales='free_y') +
        labs(x='Date', y='Weekly total ($ millions)') +
        ylim(c(0, NA)) +
        PLOT_THEME

}


verify_constant_ids <- function() {
    # TODO: verify that these are gone after re-cleaning
    buyers_with_multiple_states <- auctions %>%
        filter(! is.na(buyer_id), ! is.na(buy_state)) %>%
        distinct(buyer_id, buy_state) %>%
        group_by(buyer_id) %>%
        collapse() %>%
        mutate(state_count = n()) %>%
        filter(state_count > 1) %>%
        collect(n=Inf)
    if (tbl_has_rows(buyers_with_multiple_states)) {
        message("ERROR:")
        print(buyers_with_multiple_states)
    }

    sellers_with_multiple_states <- auctions %>%
        filter(! is.na(seller_id), ! is.na(sell_state)) %>%
        distinct(seller_id, sell_state) %>%
        group_by(seller_id) %>%
        collapse() %>%
        mutate(state_count = n()) %>%
        filter(state_count > 1) %>%
        collect(n=Inf)
    if (tbl_has_rows(sellers_with_multiple_states)) {
        message("ERROR:")
        print(sellers_with_multiple_states)
    }
}

# quality_control_graphs()

# df <- run_dd_one_year(2014)
# df <- run_dd_one_year(2014)
