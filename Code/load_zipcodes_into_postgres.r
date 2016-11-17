
if (!existsFunction('install_lazy')) {
    source('r_default_functions.r')
}
source('common_functions.r')
install_lazy(c('zipcode', 'RPostgreSQL'), verbose = FALSE)
library(RPostgreSQL)
library(zipcode)
options(warn = 2)

POSTGRES_DB <- 'second_year_paper'
POSTGRES_TABLE <- 'zipcode'

DATA_TYPES <- c(zip='char(5)', city='text', state='char(2)',
                latitude='float8', longitude='float8')

main <- function(verbose = TRUE) {
    data('zipcode')
    stopifnot(all(names(zipcode) == names(DATA_TYPES)))
    con <- dbConnect("PostgreSQL", dbname = POSTGRES_DB)
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
    DBI::dbDisconnect(con)
}

# Run things:
main()
