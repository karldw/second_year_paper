
# load_state_info_into_postgres.r
# relies on ../Data/us_county_by_year_population.feather, which is the output of
# parse_county_data.r

source('r_defaults.r')

install_lazy(c('RPostgreSQL', 'dplyr', 'ensurer'), verbose = FALSE)
library(RPostgreSQL)
library(ensurer)
suppressPackageStartupMessages(library(dplyr))
options(warn = 2)
DATA_DIR <- '../Data'
POSTGRES_DB <- 'second_year_paper'
POSTGRES_TABLE <- 'states'

DATA_TYPES <- c(state = 'char(2)', year = 'int2', population = 'int4', gdp_pc = 'int4',
  gdp_method = 'text', state_fips = 'char(2)', state_full = 'text')


STATES <- c(
    'Alabama' = 'AL', 'Alaska' = 'AK', 'Arizona' = 'AZ', 'Arkansas' = 'AR',
    'California' = 'CA', 'Colorado' = 'CO', 'Connecticut' = 'CT', 'Delaware' = 'DE',
    'District of Columbia' = 'DC', 'Florida' = 'FL', 'Georgia' = 'GA', 'Hawaii' = 'HI',
    'Idaho' = 'ID', 'Illinois' = 'IL', 'Indiana' = 'IN', 'Iowa' = 'IA', 'Kansas' = 'KS',
    'Kentucky' = 'KY', 'Louisiana' = 'LA', 'Maine' = 'ME', 'Maryland' = 'MD',
    'Massachusetts' = 'MA', 'Michigan' = 'MI', 'Minnesota' = 'MN', 'Mississippi' = 'MS',
    'Missouri' = 'MO', 'Montana' = 'MT', 'Nebraska' = 'NE', 'Nevada' = 'NV',
    'New Hampshire' = 'NH', 'New Jersey' = 'NJ', 'New Mexico' = 'NM', 'New York' = 'NY',
    'North Carolina' = 'NC', 'North Dakota' = 'ND', 'Ohio' = 'OH', 'Oklahoma' = 'OK',
    'Oregon' = 'OR', 'Pennsylvania' = 'PA', 'Rhode Island' = 'RI',
    'South Carolina' = 'SC', 'South Dakota' = 'SD', 'Tennessee' = 'TN', 'Texas' = 'TX',
    'Utah' = 'UT', 'Vermont' = 'VT', 'Virginia' = 'VA', 'Washington' = 'WA',
    'West Virginia' = 'WV', 'Wisconsin' = 'WI', 'Wyoming' = 'WY'
    )
STATES_DF <- data_frame(state_full = names(STATES), state = unname(STATES))


get_state_fips <- function() {
    # Get a table of the names, abbreviations and fips codes.
    pop_data_file <- file.path(DATA_DIR, 'us_county_by_year_population.rda')
    stopifnot(file.exists(pop_data_file))

    readRDS(pop_data_file) %>%
        as.tbl() %>%
        distinct(state, stname, .keep_all = FALSE) %>%  # explicit false
        rename(state_fips = state, state_full = stname) %>%
        left_join(STATES_DF, by = 'state_full') %>%
        ensure(! anyNA(.)) %>%
        ensure(nrow(.) == length(STATES)) %>%
        return()
}


get_pop_data <- function() {
    pop_data_file <- file.path(DATA_DIR, 'us_county_by_year_population.rda')
    stopifnot(file.exists(pop_data_file))
    state_fips_df <- get_state_fips()
    pop_df <- readRDS(pop_data_file) %>%
        as.tbl() %>%
        rename(state_fips = state) %>%
        left_join(state_fips_df, by = 'state_fips') %>%
        select(state, year, population) %>%
        group_by(state, year) %>%
        # sum county pop to state-year
        summarize(population = sum(population)) %>%
        ensure_id_vars(state, year) %>%
        ensure(! anyNA(.))

    expected_nrow <- length(unique(pop_df$year)) * length(STATES)
    stopifnot(nrow(pop_df) == expected_nrow)
    return(pop_df)
}


get_gdp_data <- function() {
    gdp_data_file <- file.path(DATA_DIR, 'us_state_gdp.rda')
    stopifnot(file.exists(gdp_data_file))
    state_fips_df <- get_state_fips()
    readRDS(gdp_data_file) %>%
        as.tbl() %>%
        rename(state_full = state) %>%
        left_join(state_fips_df, by = 'state_full') %>%
        select(state, year, gdp_pc, gdp_method) %>%
        ensure_id_vars(state, year) %>%
        ensure(! anyNA(.)) %>%
        return()
}



main <- function(verbose = TRUE) {
    pop_df <- get_pop_data()
    gdp_df <- get_gdp_data()
    state_fips_df <- get_state_fips()
    combined_df <- full_join(pop_df, gdp_df, by = c('state', 'year')) %>%
        left_join(state_fips_df, by = 'state')
        #
    auction_years_df <- combined_df %>%
        filter(between(year, 2002, 2014)) %>%
        ensure(! anyNA(.)) %>%
        as.data.frame()  # as.data.frame because dbWriteTable doesn't handle tbls
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
    successful_write <- DBI::dbWriteTable(con, POSTGRES_TABLE, auction_years_df,
                                          field.types = DATA_TYPES, row.names = FALSE)
    stopifnot(successful_write)
    pg_add_primary_key(con, POSTGRES_TABLE, c('state', 'year'))
    DBI::dbDisconnect(con)
}

# Run things:
# df <- main()
