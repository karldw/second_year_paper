
source('r_defaults.r')
install_lazy(c('RPostgreSQL', 'dplyr', 'magrittr', 'readr', 'lubridate'), verbose = FALSE)
suppressPackageStartupMessages(library(RPostgreSQL))
library(magrittr)
POSTGRES_DB <- 'second_year_paper'
POSTGRES_CPI_TABLE <- 'cpi'
DATA_TYPES <- c(year = 'int2', month = 'int2', cpi_base_2016 = 'float8')


load_cpi <- function() {
    # Downloaded from FRED, series CPIAUCSL
    # https://fred.stlouisfed.org/series/CPIAUCSL#
    filename <- '../Data/CPI/CPIAUCSL.csv'

    stopifnot(file.exists(filename))

    col_spec <- readr::cols(
        DATE = readr::col_date(format = ""),
        CPIAUCSL = readr::col_double()
    )

    df <- readr::read_csv(filename, col_types = col_spec) %>%
        setNames(tolower(names(.))) %>%
        dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date)) %>%
        dplyr::select(year, month, cpiaucsl)
    cpi_mean_2016 <- df %>% dplyr::filter(year == 2016) %>%
        dplyr::ungroup() %>%
        dplyr::summarize(cpi_mean = mean(cpiaucsl)) %$% cpi_mean

    df <- df %>% dplyr::mutate(cpi_base_2016 = cpiaucsl / cpi_mean_2016) %>%
        dplyr::select(year, month, cpi_base_2016)
    stopifnot(min(df$year) < 2002, max(df$year) > 2015)
    return(df)
}


main <- function(verbose = TRUE) {
    cpi_data <- load_cpi()
    stopifnot(all(names(cpi_data) == names(DATA_TYPES)))
    pg_user <- Sys.info()[["user"]] %>% tolower()
    con <- dbConnect("PostgreSQL", dbname = POSTGRES_DB,
                     user = pg_user, password = pg_user)
    if (DBI::dbExistsTable(con, POSTGRES_CPI_TABLE)) {
        if (verbose) {
            message('Deleting existing table.')
        }
        DBI::dbRemoveTable(con, POSTGRES_CPI_TABLE)
    }
    # returns false if unsuccessful
    successful_write <- DBI::dbWriteTable(con, POSTGRES_CPI_TABLE, cpi_data,
                                          field.types = DATA_TYPES, row.names = FALSE)
    stopifnot(successful_write)
    pg_add_primary_key(con, POSTGRES_CPI_TABLE, c('year', 'month'))
    DBI::dbDisconnect(con)
}


main()
