source('r_defaults.r')
# library(RPostgreSQL)
install_lazy(c('dplyr', 'ggplot2', 'magrittr', 'lfe', 'memoise', 'lubridate'), verbose = FALSE)
suppressPackageStartupMessages(library(dplyr))
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


pull_data_one_year <- function(year, days_before=30L, days_after=days_before,
                               top_n_auction_states=NULL) {
    # top_n_auction_states limits the data returned to auction states in the top n,
    # for alaskan buyers (default of NULL doesn't limit)
    dividend_day <- first_thursday_in_october(year)
    window_begin <- dividend_day - days_before
    window_end <- dividend_day + days_after
    # Write custom SQL because dplyr doesn't support this BETWEEN DATE business.
    date_filter_sql <- sprintf(
        "SELECT * FROM %s WHERE (sale_date BETWEEN DATE '%s' and DATE '%s')",
        POSTGRES_CLEAN_TABLE, window_begin, window_end)
    data_one_year <- tbl(con, sql(date_filter_sql)) %>%
        filter(! is.na(buy_state))

    if (! is.null(top_n_auction_states)) {
        top_states_table <- get_top_auction_states_table(top_n_auction_states, 'AK')
        data_one_year <- semi_join(data_one_year, top_states_table, by = 'auction_state')
    }

    data_one_year <- select(data_one_year, sale_date, model_yr, sales_pr, bid_ct,
                            veh_type, buy_state, sell_state, auction_state,
                            # seller_type, slrdlr_type,
                            buyer_id, seller_id)
        # mutate(alaskan_buyer = buy_state == 'AK',
        #        post_dividend = sale_date >= dividend_day)
    data_one_year <- compute(data_one_year)
    # explain(data_one_year)
    # data_one_year <- collapse(data_one_year)
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


adjust_per_capita <- function(.data, one_year = NULL, na.rm = TRUE) {
    state_pop <- states %>%
        select(state, year, population) %>%
        mutate(population = population / 1e6)

    if ('tbl_postgres' %in% class(.data)) {
        # This is the case at the time of writing
        # Note, I'm testing with tbl_postgres rather than tbl_sql because I'm about to
        # use some postgres-specific syntax
        .data <- .data %>% mutate(sale_year = date_part('year', sale_date))
    } else if ('data.frame' %in% class(.data)){
        # this would be the case if I had collect()-ed the data.
        .data <- .data %>% mutate(sale_year = lubridate::year(sale_date))
    } else {
        stop("Sorry, I don't know how to calculate sale_year here.")
    }

    out <- .data %>% left_join(state_pop,
                               by = c('buy_state' = 'state', 'sale_year' = 'year')) %>%
        mutate(sale_count_pc = sale_count / population,
               sale_tot_pc = sale_tot / population)
    if (na.rm) {
        out <- out %>% filter(! is.na(sale_count_pc), ! is.na(sale_tot_pc))
    }
    return(out)
}


sales_counts_one_year_unmemoized <- function(year) {
    stopifnot( (! missing(year)) && (length(year) == 1) && (is.numeric(year)) )
    df_base <- pull_data_one_year(year, 60)
    dividend_day <- first_thursday_in_october(year)

    # calculate sales counts (in sql), don't retrieve more than 10 million results
    row_limit <- 1e7
    sales_counts <- df_base %>%
        get_sales_counts() %>%
        adjust_per_capita() %>%
        mutate(alaskan_buyer_post = buy_state == 'AK' & sale_date >= dividend_day) %>%
        select(alaskan_buyer_post, sale_count_pc, sale_tot_pc, sale_date, buy_state) %>%
        collect(n = row_limit)
    return(sales_counts)
}
if (! existsFunction('sales_counts_one_year')) {
    sales_counts_one_year <- memoise(sales_counts_one_year_unmemoized)
}


run_dd_one_year <- function(year) {
    stopifnot( (! missing(year)) && (length(year) == 1) && (is.numeric(year)) )
    dividend_day <- first_thursday_in_october(year)

    sales_counts <- sales_counts_one_year(year) #%>% mutate(sale_week = lubridate::week(sale_date))

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


    # Partial out day of week effects
    sales_counts_ak_dow <- sales_counts %>%
        filter(buy_state == 'AK') %>%
        mutate(sale_dow = lubridate::wday(sale_date))
    sales_counts_reg_dow_resid <- (sale_count_pc ~ 0 | sale_dow | 0 | buy_state) %>%
        felm(data = sales_counts_ak_dow) %>%
        residuals() %>%
        as.numeric()
    stopifnot(length(sales_counts_reg_dow_resid) == nrow(sales_counts_ak_dow))
    sales_counts_ak_dow$resid <- sales_counts_reg_dow_resid

    my_mean <- pryr::partial(mean, na.rm = TRUE)
    my_sd <- pryr::partial(sd, na.rm = TRUE)
    resid_means_plot <- sales_counts_ak_dow %>%
        # ungroup() %>%
        # mutate(resid = sales_counts_reg_dow_resid) %>%
        group_by(sale_date) %>%
        summarize(resid = my_mean(resid), resid_sd = my_sd(resid)) %>%
        ungroup() %>%
        ggplot(aes(x = sale_date, y = resid)) +
            geom_point() +
            geom_vline(xintercept = as.integer(dividend_day), color = 'red3') +
            geom_errorbar(aes(ymin = resid - conf_mult * resid_sd,
                              ymax = resid + conf_mult * resid_sd)) +
            PLOT_THEME
    save_plot(resid_means_plot, 'sale_date_resids_wday_adj.pdf')
    return(sales_counts)
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

quality_control_graphs()

# df <- run_dd_one_year(2014)
