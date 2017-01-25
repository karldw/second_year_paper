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

#
# # testing
# auctions_nrow <- force_nrow(auctions)
# auctions_unmatched <- dplyr::left_join(auctions, vin_decoder, by = 'vin_pattern') %>%
#     select(msrp) %>%
#     filter(is.na(msrp)) %>%
#     force_nrow()
# print(paste0(100 * auctions_unmatched / auctions_nrow, "% of sales unmatched."))

state_day_mpg_avg <- auctions %>%
    filter(! is.na(buy_state)) %>%
    select(sale_date, buy_state, vin_pattern) %>%
    inner_join(vin_decoder, by = 'vin_pattern')  %>%
    select(sale_date, buy_state, combined) %>%
    group_by(sale_date, buy_state) %>%
    summarize(combined_mpg = mean(combined), count = n())
