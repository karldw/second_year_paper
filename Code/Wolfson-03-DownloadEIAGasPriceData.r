#####################################################
### SYP-CARS
### AUTHOR: DEREK WOLFSON
### PURPOSE: EXTRACT EIA GAS PRICE DATA BY ZIP CODE
#####################################################
options(warn = 1)

## SET PATHS
username <- Sys.info()['user']
if (username == "dwolfson") {
    dates <- 'H:/SYP-Cars/Data/02-Clean/FuelPrices'
} else if (username == "derek_wolfson") {
    dates <- 'S:/Derek/SYP-Cars/Data/02-Clean/FuelPrices'
} else {
    stop("Please define dates in download_eia_gas_price_data.r")
}

# set EIA API KEY
# You can get your own key here: https://www.eia.gov/opendata/register.cfmS
eia_api_key <- "849898924DCC17EED77426E18C87DDC3"
zip_state_map_url <- "http://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt"
# set parameters
data_year_min <- 2002  # should match data_year_min in MASTER.do
data_year_max <- 2014  # should match data_year_max in MASTER.do

load_package_no_attach <- function(pkg_name) {
    load_successful <- requireNamespace(pkg_name, quietly = TRUE)
    if (! load_successful) {
        install.packages(pkg_name, repos = "https://cran.cnr.berkeley.edu")
        stopifnot(requireNamespace(pkg_name))
    }
}

quiet_delete <- function(filename){
    if (file.exists(filename)) {
        file.remove(filename)
    }
}

## Don't attach some of these because they bring along other packages that cause
## namespace problems.
sapply(c('sqldf', 'curl', 'lubridate', 'haven', 'EIAdata', 'dplyr', 'data.table'),
       load_package_no_attach)
library(curl)
library(lubridate)
library(haven)
library(dplyr)
library(data.table)
library(memoise)

assert_no_NA <- function(x) {
    # Can be part of a pipe chain, since it doesn't change the input.
    stopifnot(! anyNA(x))
    invisible(x)
}

# All grades, all formulations, weekly
eia_series_gasoline <- c(
    padd_1A     = "PET.EMM_EPM0_PTE_R1X_DPG.W",   # New England
    padd_1B     = "PET.EMM_EPM0_PTE_R1Y_DPG.W",   # Central Atlantic
    padd_1C     = "PET.EMM_EPM0_PTE_R1Z_DPG.W",   # Lower Atlantic
    padd_2      = "PET.EMM_EPM0_PTE_R20_DPG.W",   # Midwest
    padd_3      = "PET.EMM_EPM0_PTE_R30_DPG.W",   # Gulf Coast
    padd_4      = "PET.EMM_EPM0_PTE_R40_DPG.W",   # Rocky Mountains
    padd_5_noCA = "PET.EMM_EPM0_PTE_R5XCA_DPG.W", # West Coast, excluding CA
    CA          = "PET.EMM_EPM0_PTE_SCA_DPG.W"    # California
    )

# Diesel, all types, No 2
eia_series_diesel <- c(
    padd_1A     = "PET.EMD_EPD2D_PTE_R1X_DPG.W",    # New England
    padd_1B     = "PET.EMD_EPD2D_PTE_R1Y_DPG.W",    # Central Atlantic
    padd_1C     = "PET.EMD_EPD2D_PTE_R1Z_DPG.W",    # Lower Atlantic
    padd_2      = "PET.EMD_EPD2D_PTE_R20_DPG.W",    # Midwest
    padd_3      = "PET.EMD_EPD2D_PTE_R30_DPG.W",    # Gulf Coast
    padd_4      = "PET.EMD_EPD2D_PTE_R40_DPG.W",    # Rocky Mountains
    padd_5      = "PET.EMD_EPD2D_PTE_R50_DPG.W"     # West Coast, including CA
    # California isn't separate for diesel because the data don't go back very far.
    )



eia_series_us_gasoline <- c(gasoline = "PET.EMM_EPM0_PTE_NUS_DPG.W")
eia_series_us_diesel   <- c(diesel = "PET.EMD_EPD2D_PTE_NUS_DPG.W")

# Pull the data from EIA and Census

# memoize to prevent downloading more than necessary when running multiple times
# (https://github.com/hadley/memoise)
if ((! exists('download_eia', mode = 'function')) || (! is.memoised(download_eia))) {
    download_eia <- memoise::memoise(EIAdata::getEIA)
}
if ((! exists('download_url', mode = 'function')) || (! is.memoised(download_url))) {
    download_url <- memoise::memoise(curl::curl_fetch_memory)
}


pull_series_list <- function(series_vec) {
    MIN_DATE <- base::as.Date(paste0(data_year_min, '-01-01')) - 7  ## get the week before so we have price data for the first week of our auctions
    MAX_DATE <- base::as.Date(paste0(data_year_max, '-12-31')) + 7  ## get the week after so we have price data for the last week of our auctions
    lapply(names(series_vec), function(series_name) {
        eia_series <- series_vec[[series_name]]
        stopifnot(! is.null(eia_series))
        dt <- download_eia(ID = eia_series, key = eia_api_key) %>%
            as.data.table
        data.table::setnames(dt, c('index', eia_series), c('eia_date', 'price'))  # setnames is vastly easier than rename_
        dt[, series := series_name]  # data.table syntax for creating a new column
        return(dt[data.table::between(eia_date, MIN_DATE, MAX_DATE), ])
    }) %>%
    rbindlist %>% return
}


make_padd_zip_map <- function(separate_CA = TRUE) {
    # See https://www.census.gov/geo/reference/ansi_statetables.html and
    # https://en.wikipedia.org/wiki/Petroleum_Administration_for_Defense_Districts
  if (separate_CA) {
    padd_states_list <- list(
      padd_1A = c(9, 23, 25, 33, 44, 50),  # New England
      padd_1B = c(10, 11, 24, 34, 36, 42),  # Central Atlantic
      padd_1C = c(12, 13, 37, 45, 51, 54),  # Lower Atlantic
      padd_2  = c(17, 18, 19, 20, 21, 26, 27, 29, 31, 38, 46, 39, 40, 47, 55),  # Midwest
      padd_3  = c(1, 5, 22, 28, 35, 48),  # Gulf Coast
      padd_4  = c(8, 16, 30, 49, 56),  # Rocky Mountains
      padd_5_noCA = c(2, 4, 15, 32, 41, 53),
      CA = 6,  # California
      # "The Virgin Islands and Puerto Rico are in PADD 6; Guam, American Samoa and the Northern Mariana Islands are in PADD 7.(12/5/94)"
      padd_6 = c(72, 78),
      padd_7 = c(60, 66, 69)
    )
  } else {
    padd_states_list <- list(
      padd_1A = c(9, 23, 25, 33, 44, 50),  # New England
      padd_1B = c(10, 11, 24, 34, 36, 42),  # Central Atlantic
      padd_1C = c(12, 13, 37, 45, 51, 54),  # Lower Atlantic
      padd_2  = c(17, 18, 19, 20, 21, 26, 27, 29, 31, 38, 46, 39, 40, 47, 55),  # Midwest
      padd_3  = c(1, 5, 22, 28, 35, 48),  # Gulf Coast
      padd_4  = c(8, 16, 30, 49, 56),  # Rocky Mountains
      padd_5  = c(2, 4, 6, 15, 32, 41, 53),  # West Coast,
      # "The Virgin Islands and Puerto Rico are in PADD 6; Guam, American Samoa and the Northern Mariana Islands are in PADD 7.(12/5/94)"
      padd_6 = c(72, 78),
      padd_7 = c(60, 66, 69)
    )
  }

    padd_states <- lapply(names(padd_states_list), function(padd_name) {
      padd_state_ids <- padd_states_list[[padd_name]]
      data.table::data.table(padd = rep(padd_name, length(padd_state_ids)),
                 state = padd_state_ids) %>% return
    }) %>% rbindlist

    zip_state_csv_results <- download_url(zip_state_map_url)
    stopifnot(zip_state_csv_results$status_code == 200)  # verify the download went through

    zip_state_map <- rawToChar(zip_state_csv_results$content) %>%
        fread(select = c("ZCTA5", "STATE"), col.names = c(as.character(zipVar), 'state'),
              colClasses = c(ZCTA5 = "character"))

    # A couple of ZIPS don't get matched, or are for somewhere outside a PADD (eg PR).
    additional_zips <- data.table::data.table(ZIPS =
      c('30272', '32120', '53108', '60440', '60443', '60803', '62040', '68138', '89165'),
  state = c(13,      12,      55,      17,      17,      17,      17,      31,      32))
  #         GA       FL       WI       IL       IL       IL       IL       NE       NV

# change column name to make this work 
    # couldn't figure out how to do this directly in data.table
colnames(additional_zips)[1] <- as.character(zipVar)
    
    rbind(zip_state_map, additional_zips, use.names = TRUE) %>%
        dplyr::distinct_(zipVar, .keep_all = TRUE) %>%
        merge(padd_states, by = 'state', all.x = TRUE) %>%
        assert_no_NA %>%
        return
}


merge_auction_dates <- function(to_merge){
    # Find the closest date in to_merge to an auction date. Replace with the auction date.

    # Some dates are bad (stored as integers YYYYMMDD)
    auction_dates <- haven::read_dta(auction_dates_dta) #%>%
        #dplyr::filter(dplyr::between(saleDate, 20020000, 20200000)) %>%
        #dplyr::mutate(saleDate = lubridate::ymd(saleDate))

    to_merge <- dplyr::mutate(to_merge, eia_date_lead = dplyr::lead(eia_date))
    # stolen from http://stackoverflow.com/a/23959723
    # (I don't know a better way to do this between-date merge)
    ret <- sqldf::sqldf(
       "select series, price, saleDate from
        auction_dates inner join to_merge
        on auction_dates.saleDate between
        to_merge.eia_date and to_merge.eia_date_lead") %>%
        as.data.table
    assert_no_NA(ret)
    # Calling unique on a keyed data.table gives the rows that are unique *by the keying
    # variables*.  Because of the weird way I'm joining above, I get some duplicates from
    # neighboring weeks. I'll just sort and take the earlier one.
    setkey(ret, saleDate, series)
    return(unique(ret))
}


fuel_by_padd <- function(fuel_type) {
    if (fuel_type == 'gasoline') {
        fuel_data <- pull_series_list(eia_series_gasoline)
    } else if (fuel_type == 'diesel') {
        fuel_data <- pull_series_list(eia_series_diesel)
    } else {
        stop("unknown fuel type")
    }
    merge_auction_dates(fuel_data) %>%
    dplyr::rename(padd = series) %>%
    dplyr::select(saleDate, padd, price) %>%
    saleDate_to_int %>%
    return
}

fuel_us <- function(fuel_type){
    if (fuel_type == 'gasoline') {
        fuel_data <- pull_series_list(eia_series_us_gasoline)
    } else if (fuel_type == 'diesel') {
        fuel_data <- pull_series_list(eia_series_us_diesel)
    } else {
        stop("unknown fuel type")
    }
    fuel_data %>%
        merge_auction_dates %>%
        dplyr::select(saleDate, price) %>%
        saleDate_to_int %>%
        return
}

## calculate saleDate as an integer YYYYMMDD
saleDate_to_int <- function(df) {
    df %>%
    dplyr::mutate(saleDate = lubridate::year(saleDate)  * 10000 +
                              lubridate::month(saleDate) *   100 +
                              lubridate::day(saleDate)
        ) %>%
    return
}



for(VAR in c('auctionZip', 'buyZip', 'sellZip')){
zipVar <- VAR # set the zip variable for what you want to consider (auctionZip sellZip buyZip)

# create filenames for later
auction_dates_file <- paste('auctionDates',zipVar,'.dta', sep = "")
auction_dates_dta <- 
  file.path(dates, 'AuctionDates', auction_dates_file)
padd_zip_map_ca_in_padd5_dta <- 
  file.path(dates, zipVar, paste(zipVar,'-PADD-zipMap-CAinPADD5.dta', sep = ""))
padd_zip_map_ca_not_in_padd5_dta <- 
  file.path(dates, zipVar, paste(zipVar,'-PADD-zipMap-CAnotInPADD5.dta', sep = ""))
fuel_price_by_padd_week_gasoline_dta <- 
  file.path(dates, zipVar, paste(zipVar,'-PADD-fuelPriceByWeek-Gasoline.dta', sep = ""))
fuel_price_by_padd_week_diesel_dta <- 
  file.path(dates, zipVar, paste(zipVar,'-PADD-fuelPriceByWeek-Diesel.dta', sep = ""))
fuel_price_us_by_week_gasoline_dta <- 
  file.path(dates, zipVar, paste(zipVar,'-US-fuelPriceByWeek-Gasoline.dta', sep = ""))
fuel_price_us_by_week_diesel_dta <- 
  file.path(dates, zipVar, paste(zipVar,'-US-fuelPriceByWeek-Diesel.dta', sep = ""))

# delete existing files so we can overwrite them with new files
# delete all those datasets (or do nothing if the file doesn't exist)
deleted <- sapply(c(padd_zip_map_ca_not_in_padd5_dta, 
                    padd_zip_map_ca_in_padd5_dta,
                    fuel_price_by_padd_week_gasoline_dta, 
                    fuel_price_by_padd_week_diesel_dta,
                    fuel_price_us_by_week_diesel_dta, 
                    fuel_price_us_by_week_gasoline_dta),
                  quiet_delete)

# call programs and create files
print(paste("writing",padd_zip_map_ca_not_in_padd5_dta))
make_padd_zip_map(separate_CA = TRUE)  %>% haven::write_dta(padd_zip_map_ca_not_in_padd5_dta, version = 12)

print(paste("writing",padd_zip_map_ca_in_padd5_dta))
make_padd_zip_map(separate_CA = FALSE) %>% haven::write_dta(padd_zip_map_ca_in_padd5_dta, version = 12)

print(paste("writing",fuel_price_by_padd_week_gasoline_dta))
fuel_by_padd('gasoline') %>% haven::write_dta(fuel_price_by_padd_week_gasoline_dta, version = 12)

print(paste("writing",fuel_price_by_padd_week_diesel_dta))
fuel_by_padd('diesel')   %>% haven::write_dta(fuel_price_by_padd_week_diesel_dta, version = 12)

print(paste("writing",fuel_price_us_by_week_gasoline_dta))
fuel_us('gasoline')      %>% haven::write_dta(fuel_price_us_by_week_gasoline_dta, version = 12)

print(paste("writing",fuel_price_us_by_week_diesel_dta))
fuel_us('diesel') %>% haven::write_dta(fuel_price_us_by_week_diesel_dta, version = 12)
}
