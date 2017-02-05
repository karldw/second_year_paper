
source('r_defaults.r')
install_lazy(c('dplyr', 'magrittr', 'haven', 'jsonlite', 'RPostgreSQL', 'lubridate'),
             verbose = FALSE)
library(magrittr)
library(RPostgreSQL)
#compiler::enableJIT(1)
options(warn = 2)

DATA_TYPES <- c(sale_date = 'date', seller_id = 'text', seller_type = 'text',
                slrdlr_type = 'text', sell_zip = 'char(5)', buyer_id = 'text',
                buy_zip = 'char(5)', auction_code = 'text', auction_zip = 'text',
                vin = 'char(17)', model_yr = 'int2', make = 'text', model = 'text',
                miles = 'int4', sale_type = 'text', salvg_flg = 'char(1)', cond = 'text',
                anncmts = 'text', remarks = 'text', sales_pr = 'float8', mmr = 'float8',
                bid_ct = 'int2', veh_type = 'char(1)')
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
        invalid_names <- new_names[! valid_name] %>% paste(collapse = ', ')
        stop(paste('Some names are invalid for SQL:', invalid_names))
    }
    names(x) <- new_names
    return(x)
}


insert_into_postgres <- function(dta_file, con, verbose = TRUE) {

    if (verbose) {
        message(basename(dta_file))
    }
    # load_df filters things that can't / shouldn't be added to the database, then
    # returns a list with (1) the data and (2) the counts of those unloadable rows.
    # Load the data into postgres, then return the row counts.
    loaded_results <- load_df(dta_file, con)
    df <- loaded_results[['df']]
    unloaded_counts <- loaded_results[['unloaded_counts']]
    DBI::dbWriteTable(con, POSTGRES_TABLE, df,
                      field.types = DATA_TYPES,
                      append = TRUE, row.names = FALSE) %>%
                      # returns false if unsuccessful
                      stopifnot()

    invisible(unloaded_counts)
}


is_valid_zip <- function(x) {
    grepl('^[0-9]{5}$', x, perl = TRUE)
}


is_valid_vin <- function(vins) {
    # See:
    # https://en.wikipedia.org/wiki/Vehicle_identification_number#Transliterating_the_numbers

    is_valid_vin_once_old <- function(x) {
        # Do a whitelist approach, where only letters (except "I", "O" and "Q")
        # and numbers are acceptable.
        proper_chars <- grepl("^[A-HJ-NPR-Z0-9]{17}$", x, perl = TRUE)
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
                next
            }
            if (i <= 7L) {
                vin_weight <- 9L - i
            } else if (i == 8L) {
                vin_weight <- 10L
            } else {
                vin_weight <- 19L - i
            }

            if        (grepl('[0-9]', vin_one_digit, perl = TRUE)) {
                one_digit_transliterated <- as.integer(vin_one_digit)
            } else if (grepl('[AJ]', vin_one_digit, perl = TRUE)) {
                one_digit_transliterated <- 1L
            } else if (grepl('[BKS]', vin_one_digit, perl = TRUE)) {
                one_digit_transliterated <- 2L
            } else if (grepl('[CLT]', vin_one_digit, perl = TRUE)) {
                one_digit_transliterated <- 3L
            } else if (grepl('[DMU]', vin_one_digit, perl = TRUE)) {
                one_digit_transliterated <- 4L
            } else if (grepl('[ENV]', vin_one_digit, perl = TRUE)) {
                one_digit_transliterated <- 5L
            } else if (grepl('[FW]', vin_one_digit, perl = TRUE)) {
                one_digit_transliterated <- 6L
            } else if (grepl('[GPX]', vin_one_digit, perl = TRUE)) {
                one_digit_transliterated <- 7L
            } else if (grepl('[HY]', vin_one_digit, perl = TRUE)) {
                one_digit_transliterated <- 8L
            } else if (grepl('[RZ]', vin_one_digit, perl = TRUE)) {
                one_digit_transliterated <- 9L
            }
            vin_check_value <- vin_check_value + one_digit_transliterated * vin_weight
        }

        vin_check_value <- mod(vin_check_value, 11L)
        vin_check_str <- dplyr::if_else(vin_check_value < 10L,
                                        as.character(vin_check_value), 'X')
        return(vin_check_str == actual_check_digit)
    }

    transliterate <- function(x) {
        # This function works for vectors of length >= 1
        chars_vec <- c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.',
                       'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', '.', '.', 'J',
                       'K', 'L', 'M', 'N', '.', 'P', '.', 'R', '.', '.', 'S',
                       'T', 'U', 'V', 'W', 'X', 'Y', 'Z')
        return((match(x, chars_vec) - 1) %% 10)
    }
    calculate_check_digit <- function(vin) {
        weights_vec <- c(8, 7, 6, 5, 4, 3, 2, 10, 0, 9, 8, 7, 6, 5, 4, 3, 2)
        vin_vec <- strsplit(vin, "", fixed = TRUE)[[1]]
        check_value <- sum(transliterate(vin_vec) * weights_vec) %% 11
        check_str <- dplyr::if_else(check_value < 10, as.character(check_value), 'X')
        return(check_str)
    }
    is_valid_vin_once <- function(vin) {
        # Do a whitelist approach, where only letters (except "I", "O" and "Q")
        # and numbers are acceptable, and the string must be 17 characters long.
        if (! grepl("^[A-HJ-NPR-Z0-9]{17}$", vin, perl = TRUE)) {
            return(FALSE)
        }
        return(substr(vin, 9, 9) == calculate_check_digit(vin))
    }

    # Do some tests:
    stopifnot(  is_valid_vin_once("11111111111111111"))
    stopifnot(  is_valid_vin_once("1M8GDM9AXKP042788"))
    stopifnot(  is_valid_vin_once("5GZCZ43D13S812715"))
    stopifnot(! is_valid_vin_once("WP0ZZZ99ZTS392124"))
    stopifnot(! is_valid_vin_once("KLATF08Y1VB363636"))

    vapply(vins, is_valid_vin_once, logical(1)) %>% return()
}


filename_to_year <- function(filename) {
    gsub('.*(\\d{4}).*', '\\1', basename(filename), perl = TRUE) %>%
        as.integer() %>%
        return()
}


load_df <- function(dta_file, con) {
    file_year <- filename_to_year(dta_file)
    # sale date is an integer of the form YYYYMMDD
    sale_date_max <- file_year * 10000 + 1231
    sale_date_min <- file_year * 10000 + 0101
    df_raw <- haven::read_dta(dta_file) %>%
        dplyr::select_(.dots = names(DATA_TYPES))
    unloaded_counts <- dplyr::data_frame(non_sales_count = sum(df_raw$sales_pr <= 0),
        bad_date_count = sum(! dplyr::between(df_raw$sale_date,
                                              sale_date_min, sale_date_max)))

    # Do these filtering operations here, rather than in the cleaning program, so the
    # variables fit in the table like I'm expecting.
    na_if_invalid_zip <- function(zipcode) {
        dplyr::if_else(is_valid_zip(zipcode), zipcode, NA_character_)
    }
    df_to_add <- df_raw %>%
        dplyr::filter(sales_pr > 0,
                      # between() ranges include both endpoints
                      dplyr::between(sale_date, sale_date_min, sale_date_max)) %>%
        dplyr::mutate(sell_zip    = na_if_invalid_zip(sell_zip),
                      buy_zip     = na_if_invalid_zip(buy_zip),
                      auction_zip = na_if_invalid_zip(auction_zip),
                      vin         = dplyr::if_else(is_valid_vin(vin), vin, NA_character_),
                      sale_date   = lubridate::ymd(sale_date)) %>%
        make_names_legal_sql(con)
    out <- list(df = df_to_add, unloaded_counts = unloaded_counts)
    return(out)
}


record_filtered_rows <- function(filtered_counts) {
    # Total all the numeric columns (that is exclude the filename column)
    total_counts <- dplyr::summarize_if(dplyr::ungroup(filtered_counts), is.numeric, sum)
    stopifnot(nrow(total_counts) == 1)

    # As of writing, the files created should be:
    # ../Text/Generated_snippets/load_manheim_filter_non_sales_count.tex
    # ../Text/Generated_snippets/load_manheim_filter_bad_date_count.tex
    for (i in seq_along(total_counts)) {
        count_name <- gsub('\\W', '_', names(total_counts)[[i]], perl = TRUE)
        count_filename <- paste0('load_manheim_filter_', count_name, '.tex')
        count_value <- as.integer(total_counts[[i]])
        make_snippet(count_value, count_filename, lazy = FALSE)
    }
}


main <- function(verbose = TRUE) {
    dta_dir <- file.path(dropbox_home()[1],
                         'KarlJim/CarPriceData/MannheimDataNew_2002-2009')
    # dta_dir <- '~/Desktop/MannheimDataNew_2002-2009'
    stopifnot(dir.exists(dta_dir))
    all_dta_files <- list.files(dta_dir, full.names = TRUE)
    stopifnot(length(all_dta_files) == length(2002:2014))  # years I have data for

    pg_user <- Sys.info()[["user"]] %>% tolower()
    con <- dbConnect("PostgreSQL", dbname = POSTGRES_DB,
                     user = pg_user, password = pg_user)
    if (DBI::dbExistsTable(con, POSTGRES_TABLE)) {
        if (verbose) {
            message('Deleting existing table.')
        }
        DBI::dbRemoveTable(con, POSTGRES_TABLE)
    }

    # insert_into_postgres does the data inserts, but also returns counts of rows that
    # couldn't be inserted.
    # Make a column in filtered_counts called dta_file with the source file info.
    # Don't run on multiple cores because we don't have enough memory.
    filtered_counts <- lapply_bind_rows(all_dta_files, insert_into_postgres,
        con = con, verbose = verbose, rbind_src_id = 'dta_file', parallel_cores = 1)

    pg_vacuum(con, POSTGRES_TABLE, analyze = TRUE)
    DBI::dbDisconnect(con)
    record_filtered_rows(filtered_counts)
}


test_insert_data <- function() {
    library(assertthat)
    dta_file <- 'C:/Users/Karl/Github/second_year_paper/Code/test_df_2008.dta'
    stopifnot(file.exists(dta_file))
    temp_table <- 'test_auction_table'
    con <- dbConnect("PostgreSQL", dbname = POSTGRES_DB)
    if (dbExistsTable(con, temp_table)) {
        dbRemoveTable(con, temp_table)
    }
    df <- load_df(dta_file, con)

    dbWriteTable(con, temp_table, df,
                 field.types = DATA_TYPES,
                 append = TRUE, row.names = FALSE) %>% stopifnot()

    df_read_from_sql <- dbReadTable(con, temp_table)

    # assert things
    assert_that(are_equal(names(df), names(df_read_from_sql)))
    df1 <- dplyr::arrange_(df, .dots = names(df))
    df2 <- dplyr::arrange_(df_read_from_sql, .dots = names(df_read_from_sql))
    for (nm in names(df)) {
        col1 <- df1[[nm]]
        col2 <- df2[[nm]]

        value_diffs <- sum(col1 != col2, na.rm = TRUE)
        na_diffs <- sum(xor(is.na(col1), is.na(col2)))
        if (value_diffs > 0) {
            message(sprintf("Value mismatch in column %s", nm))
        }
        if (na_diffs > 0) {
            message(sprintf("NAs mismatch in column %s", nm))
        }
    }
    # There are differences, but it's just some unicode garbage in the anncmts column
    # "Â¬U TITLE SUNAY" becomes "Ã‚Â¬U TITLE SUNAY"
    dbRemoveTable(con, temp_table)
}

# Run things:
# test_insert_data()
main()
