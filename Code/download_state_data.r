
# download_state_data.r
# creates ../Data/us_state_gdp.rda

source('r_defaults.r')
install_lazy(c('readr', 'reshape2', 'dplyr', 'curl'),
             verbose = FALSE)
library(readr)
suppressPackageStartupMessages(library(dplyr))
library(reshape2)
suppressPackageStartupMessages(library(curl))
# library(ensurer)
DATA_DIR <- '../Data'


download_pop_data <- function() {
    stop('This function is no longer used.')
    pop_data_file <- file.path(DATA_DIR, 'us_state_populations.rda')
    download.file('https://github.com/ropensci/historydata/blob/cccfd02844865c6e5b120f3325bfc24179e3d038/data/us_state_populations.rda',
                      pop_data_file,
                      quiet = TRUE)
}


download_gdp_data <- function() {
    gdp_data_file <- file.path(DATA_DIR, 'us_state_gdp.rda')

    tmp <- tempdir()

    # New GDP:
    new_gdp_url <- 'http://www.bea.gov/regional/zip/gsp/gsp_naics_all_PC.zip'
    new_gdp_zipfile <- file.path(tmp, 'new_gdp.zip')
    curl_download(new_gdp_url, new_gdp_zipfile)
    unzip(new_gdp_zipfile, exdir=tmp)
    new_gdp_csv <- file.path(tmp, 'gsp_naics_all_PC.csv')
    stopifnot(file.exists(new_gdp_csv))

    cols_to_read <- cols(
        .default = col_integer(),
        GeoFIPS=col_character(),
        GeoName=col_character(),
        Region=col_character(),
        ComponentId=col_character(),
        ComponentName=col_character(),
        IndustryId=col_character(),
        IndustryClassification=col_character(),
        Description=col_character()
        )
    states <- c('Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado',
    'Connecticut', 'Delaware', 'District of Columbia', 'Florida', 'Georgia', 'Hawaii',
    'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine',
    'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 'Missouri',
    'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico',
    'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon',
    'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee',
    'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin',
    'Wyoming')
    stopifnot(length(states) == 51)  # because of DC
    suppressWarnings(new_gdp_df <- read_csv(new_gdp_csv, col_types = cols_to_read))
    new_gdp_df <- new_gdp_df %>%
        filter(GeoName %in% states) %>%
        rename(state=GeoName) %>%
        select(-GeoFIPS, -Region, -ComponentId, -ComponentName, -IndustryId,
               -IndustryClassification, -Description) %>%
        melt(id.vars='state', variable.name='year', value.name='gdp_pc') %>%
        as.tbl() %>%
        mutate(year = as.integer(levels(year))[year],
               gdp_method = 'NAICS (new)')
    new_gdp_years <- unique(new_gdp_df$year)
    expected_nrow <- length(states) * length(new_gdp_years)
    stopifnot(tbl_has_rows(new_gdp_df), nrow(new_gdp_df) == expected_nrow)


    # Old GDP:
    old_gdp_url <- 'http://www.bea.gov/regional/zip/gsp/gsp_sic_all_PC.zip'
    old_gdp_zipfile <- file.path(tmp, 'ols_gdp.zip')
    curl_download(old_gdp_url, old_gdp_zipfile)
    unzip(old_gdp_zipfile, exdir=tmp)
    old_gdp_csv <- file.path(tmp, 'gsp_sic_all_PC.csv')
    stopifnot(file.exists(old_gdp_csv))
    suppressWarnings(old_gdp_df <- read_csv(old_gdp_csv, col_types=cols_to_read,
                                            na=c('', '(NA)')))
    old_gdp_df <- old_gdp_df %>%
        filter(GeoName %in% states) %>%
        rename(state=GeoName) %>%
        select(-GeoFIPS, -Region, -ComponentId, -ComponentName, -IndustryId,
               -IndustryClassification, -Description) %>%
        melt(id.vars='state', variable.name='year', value.name='gdp_pc') %>%
        as.tbl() %>%
        mutate(year = as.integer(levels(year))[year]) %>%
        filter(year >= 1987, ! year %in% new_gdp_years) %>% # NA before that
        mutate(gdp_method = 'SIC (old)')
    expected_nrow <- length(states) * length(unique(old_gdp_df$year))
    stopifnot(tbl_has_rows(old_gdp_df), nrow(old_gdp_df) == expected_nrow)

    gdp_df <- bind_rows(old_gdp_df, new_gdp_df)

    saveRDS(gdp_df, gdp_data_file)
}


main <- function() {
    stopifnot(dir.exists(DATA_DIR))

    # Instead of this file, use the county-level data in parse_county_data.r and
    # aggregate up to the state level.
    # download_pop_data()

    download_gdp_data()
}

main()
