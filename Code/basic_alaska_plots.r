
source('r_defaults.r')
source('polk_registrations.r')
install_lazy(c('ggplot2', 'RPostgreSQL', 'dplyr', 'magrittr', 'lubridate',
               'RColorBrewer'), verbose = FALSE)

POSTGRES_DB <- 'second_year_paper'
POSTGRES_CLEAN_TABLE <- 'auctions_cleaned'
VERBOSE <- FALSE
library(ggplot2)
library(dplyr)
library(magrittr)
library(RColorBrewer)

# provide the string "Non-Alaskan", but with a proper unicode hyphen
# NON_ALASKAN <- "Non\u2010Alaskan"

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
#
# load_sales_data <- function(dta_file = 'C:\\Users\\Karl\\Dropbox\\KarlJim\\CarPriceData\\MannheimDataNew_2002-2009\\data_2002_b.dta') {
#     # TODO: eventually, read all years:
#
#     # dta_file <- 'H:/cleaned_price_zip_date.dta'
#     zip_state_map <- as.tbl(zipcode) %>%
#         select(zip, state)  # can also get city, lat and long
#
#
#     #dta_file <- 'H:/uncleaned_price_zip_date.dta'
#     sales <- read_dta(dta_file) %>%
#         select(sale_date, auction_zip, buy_zip, sell_zip, sales_pr) %>%
#         filter(sales_pr > 0) %>% # only necessary when working with uncleaned data
#         mutate(sale_date = lubridate::ymd(sale_date)) %>%
#         left.join(zip_state_map, by = c('buy_zip' = 'zip')) %>%
#         rename(buy_state = state) %>%
#         left.join(zip_state_map, by = c('sell_zip' = 'zip')) %>%
#         rename(sell_state = state) %>%
#         left.join(zip_state_map, by = c('auction_zip' = 'zip')) %>%
#         rename(auction_state = state) %>%
#         mutate(alaskan_buyer   = buy_state     == 'AK',
#                alaskan_seller  = sell_state    == 'AK',
#                alaskan_auction = auction_state == 'AK') #%>%
#         # select(sale_date, sales_pr, alaskan_buyer, alaskan_seller, alaskan_auction) %>%
#         # melt(id.vars = c('sale_date', 'alaskan_buyer', 'alaskan_seller', 'alaskan_auction'),
#         #      measure.vars = 'sales_pr')
#
#     return(sales)
# }
# if (! is.memoised(load_sales_data)) {
#     load_sales_data <- memoise(load_sales_data)
# }
#
#
# sales <- load_sales_data('H:/uncleaned_price_zip_date.dta')

#
# disconnect_all <- function(){
#     lapply(DBI::dbListConnections(DBI::dbDriver('PostgreSQL')), DBI::dbDisconnect)
#     invisible(NULL)
# }
#disconnect_all()  # for repeted sourcing

load_pop_data <- function() {
    local_data_dir <- '../Data'
    stopifnot(dir.exists(local_data_dir))
    pop_filename <- file.path(local_data_dir, 'us_county_by_year_population.rda')
    if (! file.exists(pop_filename)) {
        err_msg <- "Error: county population data file doesn't exist. Please run parse_county_data.r"
        stop(err_msg)
    }
    county_pop_data <- readRDS(pop_filename) %>%
        mutate(alaskan = toupper(stname) == 'ALASKA') %>%
        group_by(year, alaskan) %>%
        summarize(population = sum(population))
    return(county_pop_data)
}


if (!exists('con')) {
    con <- src_postgres(POSTGRES_DB)
}
auctions <- tbl(con, POSTGRES_CLEAN_TABLE)
zipcode <- tbl(con, 'zipcode')
zipcode_state <- select(zipcode, zip, state)


thursdays <- first_thursday_in_october(seq(2002, 2014, by=1)) #%>%
    # as.Date(origin='1970-01-01')

#first_thursday_in_october(unique(lubridate::year(auctions$sale_date)))


# Only to the expensive operation once
if (!exists('daily_sales_totals_alaska_vs')) {
    daily_sales_totals_alaska_vs <- select(auctions,
            buy_state, sale_date, sales_pr) %>%
        filter(!is.na(buy_state)) %>%
        mutate(alaskan_buyer = buy_state == 'AK') %>%
        group_by(alaskan_buyer, sale_date) %>%
        summarize(sales_total_day = sum(sales_pr), sale_count = n()) %>%
        ungroup()
    if (VERBOSE) {
        explain(daily_sales_totals_alaska_vs)
    }
    daily_sales_totals_alaska_vs <- collect(daily_sales_totals_alaska_vs)
}

# Commented out for speed (not used for now)
# if (!exists('daily_sales_totals_by_state')) {
#     daily_sales_totals_by_state <- select(auctions, sale_date, sales_pr, buy_state) %>%
#         filter(!is.na(buy_state)) %>%
#         group_by(buy_state, sale_date) %>%
#         summarize(sales_total_day = sum(sales_pr), sale_count = n()) %>%
#         ungroup()
#     if (VERBOSE) {
#         explain(daily_sales_totals_by_state)
#     }
#     daily_sales_totals_by_state <- collect(daily_sales_totals_by_state, n=Inf)
# }

# Figre out which VINs are resales and only take the last time it appears
last_sale_dates <- auctions %>% group_by(vin) %>% summarize(sale_date = max(sale_date))
auctions_no_resale <- semi.join(auctions, last_sale_dates, by=c('vin', 'sale_date'))
if (!exists('daily_sales_totals_alaska_vs_no_resale')) {
    daily_sales_totals_alaska_vs_no_resale <- select(auctions_no_resale,
            buy_state, sale_date, sales_pr) %>%
        filter(!is.na(buy_state)) %>%
        mutate(alaskan_buyer = buy_state == 'AK') %>%
        group_by(alaskan_buyer, sale_date) %>%
        summarize(sales_total_day = sum(sales_pr), sale_count = n()) %>%
        ungroup()
    if (VERBOSE) {
        explain(daily_sales_totals_alaska_vs_no_resale)
    }
    daily_sales_totals_alaska_vs_no_resale <- daily_sales_totals_alaska_vs_no_resale %>%
        collect(n=Inf)
}


daily_sales_totals <- ungroup(daily_sales_totals_alaska_vs) %>%
    mutate(year = lubridate::year(sale_date)) %>%
    group_by(alaskan_buyer, year) %>%
    mutate(sales_total_year = sum(sales_total_day),
           sales_day_frac = sales_total_day / sales_total_year) %>%
    ungroup()


sales_comparison_2004_plot <- daily_sales_totals %>%
    mutate(alaskan_buyer = bool_to_alaska_factor(alaskan_buyer)) %>%
    ggplot(aes(x = sale_date, y = sales_total_day/10^6)) +
    geom_smooth(span=0.1, method='loess') +  # plot the smooth of the whole function
    geom_point(alpha = 0.1) +
    facet_grid(alaskan_buyer     ~ ., scales='free_y') +
    #labs(color = 'Alaskan buyer') +
    geom_vline(xintercept = thursdays) +
    xlim(as.Date(c('2004-06-01', '2005-03-01'))) +
    PLOT_THEME +
    labs(x='Sale date', y='Daily sale total (millions)',
         title='Wholesale car auctions, 2004')

save_plot(sales_comparison_2004_plot, 'auctions_2004_alaska_vs_other.pdf', scale_mult=1.5)

count_comparison_2004_plot <- filter(daily_sales_totals,
    (sale_count > 10000 & (! alaskan_buyer)) |
    (sale_count > 5   & alaskan_buyer)) %>%
    mutate(alaskan_buyer = bool_to_alaska_factor(alaskan_buyer)) %>%
    ggplot(aes(x = sale_date, y = sale_count)) +
    geom_smooth(span=0.1, method='loess') +  # plot the smooth of the whole function
    geom_point(alpha = 0.1) +
    facet_grid(alaskan_buyer ~ ., scales='free_y') +
    #labs(color = 'Alaskan buyer') +
    geom_vline(xintercept = thursdays) +
    xlim(as.Date(c('2004-06-01', '2005-03-01'))) +
    PLOT_THEME +
    labs(x='Sale date', y='Daily sale count',
         title='Wholesale car auctions, 2004')

save_plot(count_comparison_2004_plot,
          'auctions_2004_alaska_vs_other_counts.pdf', scale_mult=1.5)


# Plot sales counts per capita
daily_sales_totals_alaska_vs_no_resale_with_pop <- daily_sales_totals_alaska_vs_no_resale %>%
    mutate(year = lubridate::year(sale_date), month=lubridate::month(sale_date)) %>%
    group_by(year, month, alaskan_buyer) %>%
    summarize(sale_count = sum(sale_count), sales_total = sum(sales_total_day)) %>%
    ungroup() %>%
    left.join(load_pop_data(), by=c('alaskan_buyer'='alaskan', 'year'='year')) %>%
    ensure_id_vars(alaskan_buyer, year, month) %>%
    mutate(sale_count_per_capita = sale_count / population,
           sales_total_per_capita = sales_total / population,
           sale_date_month = lubridate::make_date(year, month, 1))


sale_count_per_capita_plot <- ggplot(daily_sales_totals_alaska_vs_no_resale_with_pop,
    aes(x=sale_date_month, y=sale_count_per_capita * 1000 * 3,
        color=bool_to_alaska_factor(alaskan_buyer))) +
    geom_line() +
    labs(x='', y='Monthly sales per 1000 people\n(quarterly rate)', color='') +
    PLOT_THEME +
    theme(legend.position = c(.9, .9)) +
    scale_color_manual(values=BLUE_AND_YELLOW)
save_plot(sale_count_per_capita_plot,
         'auction_sales_counts_per_capita_alaska_vs_no_resale_monthly_qtr_rate_notitle.pdf')


# Do a bunch of manipulation to get conformable datasets
polk_regs_data <- load_regs_data() %>% # from polk_registrations.r
    dplyr::group_by(date, alaska) %>%
    dplyr::summarize(cnt = sum(cnt)) %>%
    ungroup() %>%
    dplyr::mutate(year = lubridate::year(date), quarter = lubridate::quarter(date)) %>%
    select(-date) %>%
    ensure_id_vars(year, quarter, alaska) %>%
    mutate(count_type = 'registrations')

pop_data <- load_pop_data() %>%  # defined above
    rename(alaska = alaskan) %>%
    ensure_id_vars(year, alaska)

quarter_sales_totals_alaska_vs_no_resale <- daily_sales_totals_alaska_vs_no_resale %>%
    rename(alaska = alaskan_buyer) %>%
    mutate(year = lubridate::year(sale_date),
           quarter = lubridate::quarter(sale_date)) %>%
    select(-sale_date) %>%
    group_by(year, quarter, alaska) %>%
    summarise(cnt = sum(sale_count)) %>%
    mutate(count_type = 'auctions')

combined_regs_auctions <- bind_rows(polk_regs_data,
    quarter_sales_totals_alaska_vs_no_resale) %>%
    left.join(pop_data,  by=c('alaska', 'year')) %>%
    ensure_id_vars(alaska, count_type, year, quarter) %>%
    mutate(
         # make a variable for the quarterly date
           sale_date_quarter = lubridate::make_date(year, quarter * 3 - 2, 1),
           alaska_by_count_type = factor(paste(bool_to_alaska_factor(alaska), count_type),
            levels=c('Non-Alaskan registrations', 'Alaskan registrations',
            'Non-Alaskan auctions', 'Alaskan auctions'))
    )

combined_regs_auctions_count_per_capita_plot <- ggplot(combined_regs_auctions,
    aes(x = sale_date_quarter, y = cnt / population * 1000,
        color = alaska_by_count_type)) +
    geom_line() +
    labs(x='', y='Quarterly sales per 1000 people', color='') +
    PLOT_THEME +
    theme(legend.position = c(.85, .88)) +
    # pick four colors for Non-Alaskan registrations, Alaskan registrations,
    # Non-Alaskan auctions and Alaskan auctions, respectively
    # scale_color_manual(values=brewer.pal(9, 'RdYlBu')[c(3,1,8,9)])
    # scale_color_manual(values=brewer.pal(9, 'RdYlBu')[c(3,8,1,9)])
    scale_color_manual(values=brewer.pal(4, 'RdYlBu')[c(2,3,1,4)])
save_plot(combined_regs_auctions_count_per_capita_plot,
         'combined_regs_sales_counts_per_capita_alaska_vs_no_resale_quarterly_notitle.pdf')
