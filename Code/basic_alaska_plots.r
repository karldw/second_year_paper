
library(reshape2)
library(memoise)
library(curl)
library(ggplot2)
library(dplyr)
library(haven)
library(zipcode)
data('zipcode')
if (! memoise::is.memoised(curl_fetch_memory)) {
    curl_fetch_memory <- memoise::memoise(curl::curl_fetch_memory)
}
# if (! memoise::is.memoised(read_dta)) {
#     read_dta <- memoise::memoise(haven::read_dta)
# }


# load_package_no_attach <- function(pkg_name) {
#     load_successful <- requireNamespace(pkg_name, quietly = TRUE)
#     if (! load_successful) {
#         install.packages(pkg_name, repos = "https://cran.cnr.berkeley.edu")
#         stopifnot(requireNamespace(pkg_name))
#     }
# }
#
# load_package_no_attach('data.table')
# fread <- data.table::fread
#
# zip_state_map_url <- "http://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt"
#
# zip_state_csv_results <- curl_fetch_memory(zip_state_map_url)
# stopifnot(zip_state_csv_results$status_code == 200)  # verify the download went through
#
# # zip codes missing in the original data, not sure why...
# additional_zips <- data.table::data.table(auction_zip =
#    c('30272', '32120', '53108', '60440', '60443', '60803', '62040', '68138', '89165'),
# state = c(13,      12,      55,      17,      17,      17,      17,      31,      32))
# #         GA       FL       WI       IL       IL       IL       IL       NE       NV
#
# zip_state_map <- rawToChar(zip_state_csv_results$content) %>%
#     # use data.table's fread because it's less aggressively annoying than read_csv
#     fread(select = c("ZCTA5", "STATE"), col.names = c('auction_zip', 'state'),
#           colClasses = c(ZCTA5 = "character")) %>%
#     rbind(additional_zips, use.names = TRUE) %>%
#     as.tbl %>% # convert for dplyr
#     distinct(auction_zip, state) %>%
#     # # Alaska's FIPS number is 2
#     # # Source: https://www.census.gov/geo/reference/ansi_statetables.html
#     mutate(alaska = (state == 2L))
#     #


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
                        FUN.VALUE = lubridate::make_date(2000, 1, 1))
    # thursdays <- as.Date.numeric(thursdays, origin = '1970-01-01')

    return(thursdays)
}

load_sales_data <- function(dta_file = 'C:\\Users\\Karl\\Dropbox\\KarlJim\\CarPriceData\\MannheimDataNew_2002-2009\\data_2002_b.dta') {
    # TODO: eventually, read all years:

    # dta_file <- 'H:/cleaned_price_zip_date.dta'
    zip_state_map <- as.tbl(zipcode) %>%
        select(zip, state)  # can also get city, lat and long


    #dta_file <- 'H:/uncleaned_price_zip_date.dta'
    sales <- read_dta(dta_file) %>%
        select(sale_date, auction_zip, buy_zip, sell_zip, sales_pr) %>%
        filter(sales_pr > 0) %>% # only necessary when working with uncleaned data
        mutate(sale_date = lubridate::ymd(sale_date)) %>%
        left_join(zip_state_map, by = c('buy_zip' = 'zip')) %>%
        rename(buy_state = state) %>%
        left_join(zip_state_map, by = c('sell_zip' = 'zip')) %>%
        rename(sell_state = state) %>%
        left_join(zip_state_map, by = c('auction_zip' = 'zip')) %>%
        rename(auction_state = state) %>%
        mutate(alaskan_buyer   = buy_state     == 'AK',
               alaskan_seller  = sell_state    == 'AK',
               alaskan_auction = auction_state == 'AK') #%>%
        # select(sale_date, sales_pr, alaskan_buyer, alaskan_seller, alaskan_auction) %>%
        # melt(id.vars = c('sale_date', 'alaskan_buyer', 'alaskan_seller', 'alaskan_auction'),
        #      measure.vars = 'sales_pr')

    return(sales)
}
if (! is.memoised(load_sales_data)) {
    load_sales_data <- memoise(load_sales_data)
}


sales <- load_sales_data('H:/uncleaned_price_zip_date.dta')

thursdays <- filter(sales, ! is.na(sale_date)) %$%
    year %>%
    unique %>%
    first_thursday_in_october

#first_thursday_in_october(unique(lubridate::year(sales$sale_date)))


plt <- sales %>%
    filter(! is.na(alaskan_buyer)) %>%
    group_by(alaskan_buyer) %>%
    sample_n(1000) %>%  # take 500 from alaskan sellers and non-alaskan sellers
    ggplot(aes(x = sale_date, y = sales_pr, color = alaskan_buyer)) +
    geom_smooth(data = sales) +  # plot the smooth of the whole function
    geom_point(alpha = 0.1) +
    labs(color = 'Alaskan buyer') +
    geom_vline(xintercept = thursdays)

print(plt)
