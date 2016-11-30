

source('r_defaults.r')

install_lazy(c('ggplot2', 'RPostgreSQL', 'dplyr', 'magrittr'), verbose = FALSE)

POSTGRES_DB <- 'second_year_paper'
POSTGRES_CLEAN_TABLE <- 'auctions_cleaned'

library(ggplot2)
library(dplyr)
library(magrittr)


if (!exists('con')) {
    con <- src_postgres('second_year_paper')
}
auctions <- tbl(con, 'auctions_cleaned')
zipcode <- tbl(con, 'zipcode') %>% select(zip, state)

if (!exists('auctions_with_state')) {
    message('Making auctions_with_state (this takes a couple of minutes)')
    auctions_with_state <- left_join(auctions, zipcode, by=c('buy_zip'='zip')) %>%
        rename(buy_state = state) %>%
        left_join(zipcode, by=c('sell_zip'='zip')) %>%
        rename(sell_state = state) %>%
        left_join(zipcode, by=c('auction_zip'='zip')) %>%
        rename(auction_state = state) %>%
        compute()  # run the command and make a temporary table in postgres
}

row_count <- summarize(auctions, count=n()) %>% collect() %$% count

# How many buyers are there?
buyer_count <- distinct(auctions, buyer_id) %>%
    filter(buyer_id != NA) %>%
    summarize(count = n()) %>%
    collect()

# What fraction of buyer zips are missing?
sales_no_buyer_zip <- filter(auctions, is.na(buy_zip)) %>%
    summarize(count=n()) %>% collect() %$% count
# What fraction of seller zips are missing?
sales_no_seller_zip <- filter(auctions, is.na(sell_zip)) %>%
    summarize(count=n()) %>% collect() %$% count
# What fraction of auction zips are missing?
sales_no_auction_zip <- filter(auctions, is.na(auction_zip)) %>%
    summarize(count=n()) %>% collect() %$% count  # 8386
# What fraction of rows have no missing zips?
sales_complete_zips <- filter(auctions,
    ! (is.na(buy_zip) | is.na(sell_zip) | is.na(auction_zip))) %>%
    summarize(count=n()) %>% collect() %$% count

# How many zip codes are the buyers from?
buyer_zip_count <- distinct(auctions, buy_zip) %>%
    filter(buy_zip != NA) %>%
    summarize(count = n()) %>%
    collect()

# How many Alaskan buyers are there?
buyer_count_by_state <- distinct(auctions_with_state, buyer_id, buy_state) %>%
    filter(buy_state != NA) %>%
    group_by(buy_state) %>%
    summarize(count = n()) %>%
    collect()

# Where do Alaskan buyers get their vehicles?
alaska_buyer_auction_locations <- filter(auctions_with_state, buy_state == 'AK') %>%
    filter(auction_state != NA) %>%
    group_by(auction_state) %>%
    summarize(count = n()) %>%
    collect()

alaska_buyer_seller_locations <- filter(auctions_with_state, buy_state == 'AK') %>%
    filter(sell_state != NA) %>%
    group_by(sell_state) %>%
    summarize(count = n()) %>%
    collect()


# Can I track individual buyers over time?
buyer_permanence <- filter(auctions, buyer_id != NA) %>%
    group_by(buyer_id, sale_date) %>%
    summarize(count = n()) %>%
    collect(n=Inf)

# Can I match buyer and seller IDs?
# Think more about this one:
# match_buyers_sellers <- inner_join(auctions, auctions, by=c('buyer_id'='seller_id')) %>%
