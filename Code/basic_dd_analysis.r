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


tbl_has_rows <- function(a_tbl) {
    nrow_head1 <- ungroup(a_tbl) %>% head(1) %>% collect() %>% nrow()
    return(nrow_head1 > 0)
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
    stopifnot(tbl_has_rows(data_one_year))
    return(data_one_year)
}

run_dd_one_year <- function(year) {
    stopifnot( (! missing(year)) && (length(year) == 1) && (is.numeric(year)) )
    df_base <- pull_data_one_year(year)
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
    sales_counts <- df_base %>% group_by(sale_date, buyer_id) %>%
        summarize(sale_count = n(), sale_tot = sum(sales_pr)) %>% collect(n=Inf)
    buyer_info <- df_base %>% distinct(buyer_id, buy_state) %>% collect(n=Inf)
    # TODO: if I don't have collect() in the previous lines, I get an error because
    # dplyr is renaming sale_date to sale_date.y.  Report this.
    dividend_day <- first_thursday_in_october(year)
    sales_counts <- inner_join(sales_counts, buyer_info, by='buyer_id') %>% # collect()
        mutate(alaskan_buyer = buy_state == 'AK',
               post_dividend = sale_date >= dividend_day)
    # TODO: start here:
    # DD with LHS of daily sale_count, buyer_id FE and buy_state clusters
    sales_counts_formula <- (sale_count ~ alaskan_buyer:post_dividend | buyer_id | 0 |
                                          buy_state)
    reg_sales_counts <- felm(formula=sales_counts_formula, data=sales_counts)
    return(reg_sales_counts)
    # felm(buyer_id + seller_id + buy_state + sell_state + auction_state, data=df)
}


verify_constant_ids <- function() {
    # TODO: figure out why these have dups!
    distinct_state_count <- auctions %>%
        distinct(buyer_id, buy_state) %>%
        group_by(buyer_id) %>%
        tally() %>%
        filter(n > 1)
    if (tbl_has_rows(distinct_state_count)) {
        message("ERROR:")
        print(distinct_state_count)
    }

    distinct_state_count <- auctions %>%
        distinct(seller_id, sell_state) %>%
        group_by(seller_id) %>%
        tally()
    if (tbl_has_rows(distinct_state_count)) {
        message("ERROR:")
        print(distinct_state_count)
    }
}

# Doesn't work:
run_dd_one_year(2014)
