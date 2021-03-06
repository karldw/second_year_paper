
source('r_defaults.r')

install_lazy(c('zipcode', 'RPostgreSQL'), verbose = FALSE)
library(RPostgreSQL)
library(zipcode)
library(magrittr)
options(warn = 2)

POSTGRES_DB <- 'second_year_paper'
POSTGRES_TABLE <- 'zipcode'

DATA_TYPES <- c(zip = 'char(5)', city = 'text', state = 'char(2)',
                latitude = 'float8', longitude = 'float8')

main <- function(verbose = TRUE) {
    data('zipcode')
    stopifnot(all(names(zipcode) == names(DATA_TYPES)))
    pg_user <- Sys.info()[["user"]] %>% tolower()
    con <- dbConnect("PostgreSQL", dbname = POSTGRES_DB,
                     user = pg_user, password = pg_user)
    if (DBI::dbExistsTable(con, POSTGRES_TABLE)) {
        if (verbose) {
            message('Deleting existing table.')
        }
        DBI::dbRemoveTable(con, POSTGRES_TABLE)
    }
    # returns false if unsuccessful
    successful_write <- DBI::dbWriteTable(con, POSTGRES_TABLE, zipcode,
                                          field.types = DATA_TYPES, row.names = FALSE)
    stopifnot(successful_write)
    pg_add_primary_key(con, POSTGRES_TABLE, 'zip')
    DBI::dbDisconnect(con)
}

# Run things:
main()
