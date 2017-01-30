

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

# 51,461,995 obs total (post-cleaning)
row_count <- summarize(auctions, count=n()) %>% collect() %$% count

# How many buyers are there? -- 308,185
buyer_count <- distinct(auctions, buyer_id) %>%
    filter(! is.na(buyer_id)) %>%
    summarize(count = n()) %>% collect() %$% count

# What fraction of buyer zips are missing?  2,375,581 missing obs
sales_no_buyer_zip <- filter(auctions, is.na(buy_zip)) %>%
    summarize(count=n()) %>% collect() %$% count
# What fraction of seller zips are missing?  11,039,857 missing obs
sales_no_seller_zip <- filter(auctions, is.na(sell_zip)) %>%
    summarize(count=n()) %>% collect() %$% count
# What fraction of auction zips are missing?  3,744,460 missing
sales_no_auction_zip <- filter(auctions, is.na(auction_zip)) %>%
    summarize(count=n()) %>% collect() %$% count  #
# What fraction of rows have no missing zips?  37,658,283 non-missing
sales_complete_zips <- filter(auctions,
    ! (is.na(buy_zip) | is.na(sell_zip) | is.na(auction_zip))) %>%
    summarize(count=n()) %>% collect() %$% count

# How many zip codes are the buyers from?  21,178
buyer_zip_count <- distinct(auctions, buy_zip) %>%
    filter(! is.na(buy_zip)) %>%
    summarize(count = n()) %>%
    collect() %$% count

# How many Alaskan buyers are there?  247
buyer_count_by_state <- distinct(auctions, buyer_id, buy_state) %>%
    filter(! is.na(buy_state)) %>%
    group_by(buy_state) %>%
    summarize(count = n()) %>%
    collect()

# Where do Alaskan buyers get their vehicles?
alaska_buyer_auction_locations <- filter(auctions, buy_state == 'AK') %>%
    filter(! is.na(auction_state)) %>%
    group_by(auction_state) %>%
    summarize(count = n()) %>%
    collect()
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

alaska_buyer_seller_locations <- filter(auctions, buy_state == 'AK') %>%
    filter(! is.na(sell_state)) %>%
    group_by(sell_state) %>%
    summarize(count = n()) %>%
    collect()
# Top 10:
#    sell_state count
# 1          WA  4058
# 2          CA  2439
# 3          OR  2342
# 4          NV  1048
# 5          AZ  1046
# 6          AK   937
# 7          TX   922
# 8          VA   808
# 9          OK   775
# 10         NY   618


# Can I track individual buyers over time?
buyer_permanence <- filter(auctions, ! is.na(buyer_id)) %>%
    group_by(buyer_id, sale_date) %>%
    summarize(count = n()) %>%
    collect(n=Inf)

# Can I match buyer and seller IDs?
# Think more about this one:
# match_buyers_sellers <- inner.join(auctions, auctions, by=c('buyer_id'='seller_id')) %>%
