
DATA_DIR <- '../Data'
stopifnot(dir.exists(DATA_DIR))

population_data <- file.path(DATA_DIR, 'us_state_populations.rda')
if (! file.exists(population_data)) {
    download.file('https://github.com/ropensci/historydata/blob/cccfd02844865c6e5b120f3325bfc24179e3d038/data/us_state_populations.rda',
                  population_data,
                  quiet = TRUE)
}


state_gdp <- file.path(DATA_DIR, 'us_state_gdp.rda')
if (! file.exists(state_gdp)) {
    library(readr)
    library(dplyr)
    library(reshape2)
    tmp <- tempdir()

    # New GDP:
    new_gdp_url <- 'http://www.bea.gov/regional/zip/gsp/gsp_naics_all_PC.zip'
    new_gdp_zipfile <- file.path(tmp, 'new_gdp.zip')
    download.file(new_gdp_url, new_gdp_zipfile, quiet=TRUE)
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
    'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New
    York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania',
    'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah',
    'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')
    new_gdp_df <- read_csv(new_gdp_csv, col_types=cols_to_read) %>%
        filter(GeoName %in% states) %>%
        rename(state=GeoName) %>%
        select(-GeoFIPS, -Region, -ComponentId, -ComponentName, -IndustryId,
               -IndustryClassification, -Description) %>%
        melt(id.vars='state', variable.name='year', value.name='gdp_pc') %>%
        as.tbl() %>%
        mutate(year = as.integer(levels(year))[year])

    # Old GDP:
    old_gdp_url <- 'http://www.bea.gov/regional/zip/gsp/gsp_sic_all_PC.zip'
    old_gdp_zipfile <- file.path(tmp, 'ols_gdp.zip')
    download.file(old_gdp_url, old_gdp_zipfile, quiet=TRUE)
    unzip(old_gdp_zipfile, exdir=tmp)
    old_gdp_csv <- file.path(tmp, 'gsp_sic_all_PC.csv')
    stopifnot(file.exists(old_gdp_csv))
    old_gdp_df <- read_csv(old_gdp_csv, col_types=cols_to_read,
                           na=c('', '(NA)')) %>%
        filter(GeoName %in% states) %>%
        rename(state=GeoName) %>%
        select(-GeoFIPS, -Region, -ComponentId, -ComponentName, -IndustryId,
               -IndustryClassification, -Description) %>%
        melt(id.vars='state', variable.name='year', value.name='gdp_pc') %>%
        as.tbl() %>%
        mutate(year = as.integer(levels(year))[year]) %>%
        filter(year >= 1987)  # NA before that

    gdp_df <- bind_rows(old_gdp_df, new_gdp_df)
    saveRDS(gdp_df, state_gdp)
}
