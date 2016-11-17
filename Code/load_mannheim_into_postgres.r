
if (!existsFunction('install_lazy')) {
    source('r_default_functions.r')
}

source('common_functions.r')
install_lazy(c('dplyr', 'magrittr', 'haven', 'jsonlite', 'RPostgreSQL', 'lubridate'), verbose = FALSE)
library(magrittr)
library(RPostgreSQL)
#compiler::enableJIT(1)
options(warn = 2)

DATA_TYPES <- c(seller_id='text', seller_type='text', slrdlr_type='text',
                sell_zip='char(5)', buyer_id='text', auction_code='text',
                auction_zip='text', sale_date='date', vin='char(17)',
                model_yr='int2', make='text', model='text',
                miles='int4', sale_type='text', salvg_flg='text', cond='text', anncmts='text', remarks='text', sales_pr='float8',
                mmr='float8', bid_ct='int2', buy_zip='char(5)')
POSTGRES_DB <- 'second_year_paper'
POSTGRES_TABLE <- 'all_years_all_sales'


make_names_legal_sql <- function(x, con) {
    # The SQL backend does this anyway, but doing myself raises errors sooner and prompts
    # the user to resolve them.
    orig_names <- names(x)
    keywords <- DBI::SQLKeywords(con) %>% tolower()
    new_names <- tolower(orig_names)

    if (any(new_names %in% keywords)) {
        stop('Names cannot be SQL keywords')
    }
    valid_name <- (grepl('^[_a-z][_a-z0-9]*', new_names, perl = TRUE) &
                   nchar(new_names) < 64)
    if (! all(valid_name)) {
        invalid_names <- new_names[! valid_name] %>% paste(collapse=', ')
        stop(paste('Some names are invalid for SQL:', invalid_names))
    }
    names(x) <- new_names
    return(x)
}


insert_into_postgres <- function(dta_file, con, verbose = TRUE) {

    if (verbose) {
        message(basename(dta_file))
    }
    df <- load_df(dta_file, con)

    DBI::dbWriteTable(con, POSTGRES_TABLE, df,
                      field.types = DATA_TYPES,
                      append = TRUE, row.names = FALSE) %>%
                      # returns false if unsuccessful
                      stopifnot()
    invisible(NULL)
}


is_valid_zip <- function(x) {
    grepl('^[0-9]{5}$', x, perl = TRUE)
}


is_valid_vin <- function(vins) {
    # See:
    # https://en.wikipedia.org/wiki/Vehicle_identification_number#Transliterating_the_numbers

    is_valid_vin_once <- function(x) {
        # Do a whitelist approach, where only letters (except "I", "O" and "Q")  and numbers are acceptable.
        proper_chars <- grepl("^[A-HJ-NPR-Z0-9]{17}$", x, perl=TRUE)
        if (proper_chars == FALSE) {
            return(FALSE)
        }

        vin_check_value <- 0L
        for (i in seq_len(17L)) {
            vin_one_digit <- substr(x, i, i)
            if (i == 9L) {
                # the ninth digit is the check; it's not included in the
                # calculation (equivalently, you could assign vin_weight = 0)
                actual_check_digit <- vin_one_digit
            }
            if (i <= 7L) {
                vin_weight <- 9L - i
            } else if (i == 8L) {
                vin_weight <- 10L
            } else {
                vin_weight <- 19L - i
            }

            if        (grepl('[0-9]', vin_one_digit, fixed=TRUE)) {
                vin_check_value <- vin_check_value + as.integer(vin_one_digit)
            } else if (grepl('[AJ]', vin_one_digit, fixed=TRUE)) {
                vin_check_value <- vin_check_value + 1L
            } else if (grepl('[BKS]', vin_one_digit, fixed=TRUE)) {
                vin_check_value <- vin_check_value + 2L
            } else if (grepl('[CLT]', vin_one_digit, fixed=TRUE)) {
                vin_check_value <- vin_check_value + 3L
            } else if (grepl('[DMU]', vin_one_digit, fixed=TRUE)) {
                vin_check_value <- vin_check_value + 4L
            } else if (grepl('[ENV]', vin_one_digit, fixed=TRUE)) {
                vin_check_value <- vin_check_value + 5L
            } else if (grepl('[FW]', vin_one_digit, fixed=TRUE)) {
                vin_check_value <- vin_check_value + 6L
            } else if (grepl('[GPX]', vin_one_digit, fixed=TRUE)) {
                vin_check_value <- vin_check_value + 7L
            } else if (grepl('[HY]', vin_one_digit, fixed=TRUE)) {
                vin_check_value <- vin_check_value + 8L
            } else if (grepl('[RZ]', vin_one_digit, fixed=TRUE)) {
                vin_check_value <- vin_check_value + 9L
            }
        }

        vin_check_value <- mod(vin_check_value, 11L)
        vin_check_str <- ifelse(vin_check_value < 10L, as.character(vin_check_value), 'X')
        return(vin_check_str == actual_check_digit)
    }

    # Do some tests:
    stopifnot(  is_valid_vin_once("1M8GDM9AXKP042788"))
    stopifnot(  is_valid_vin_once("5GZCZ43D13S812715"))
    stopifnot(! is_valid_vin_once("WP0ZZZ99ZTS392124"))
    stopifnot(! is_valid_vin_once("KLATF08Y1VB363636"))

    vapply(vins, is_valid_vin_once, logical(1)) %>% return()
}

filename_to_year <- function(filename) {
    gsub('.*(\\d{4}).*', '\\1', basename(filename), perl=TRUE) %>%
        as.integer() %>%
        return()
}


load_df <- function(dta_file, con) {
    file_year <- filename_to_year(dta_file)
    # sale date is an integer of the form YYYYMMDD
    sale_date_max <- file_year * 10000 + 1231
    sale_date_min <- file_year * 10000 + 0101
    haven::read_dta(dta_file) %>%
    dplyr::select_(.dots = names(DATA_TYPES)) %>%
    dplyr::filter(sales_pr > 0,
                  # between() ranges include both endpoints
                  dplyr::between(sale_date, sale_date_min, sale_date_max)) %>%
    dplyr::mutate(sell_zip = ifelse(is_valid_zip(sell_zip), sell_zip, NA_character_),
                  buy_zip  = ifelse(is_valid_zip(buy_zip),  buy_zip,  NA_character_),
                  vin      = ifelse(is_valid_vin(vin),      vin,      NA_character_),
                  sale_date = lubridate::ymd(sale_date)) %>%
    make_names_legal_sql(con) %>%
    return()
}


main <- function(verbose = TRUE) {
    # dta_dir <- file.path(dropbox_home()[1],
    #                      'KarlJim/CarPriceData/MannheimDataNew_2002-2009')
    dta_dir <- '~/Desktop/MannheimDataNew_2002-2009'
    stopifnot(dir.exists(dta_dir))
    all_dta_files <- list.files(dta_dir, full.names = TRUE)
    stopifnot(length(all_dta_files) == length(2002:2014))  # years I have data for

    con <- dbConnect("PostgreSQL", dbname = POSTGRES_DB)
    if (DBI::dbExistsTable(con, POSTGRES_TABLE)) {
        if (verbose) {
            message('Deleting existing table.')
        }
        DBI::dbRemoveTable(con, POSTGRES_TABLE)
    }

    lapply(all_dta_files, insert_into_postgres, con=con, verbose=verbose)
    pg_vacuum(con, POSTGRES_TABLE)
    DBI::dbDisconnect(con)
}

# Run things:
main()
