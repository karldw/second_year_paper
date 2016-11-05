
install_lazy(c('dplyr', 'magrittr', 'haven', 'jsonlite', 'RPostgreSQL'), verbose = FALSE)
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


dropbox_home <- function(){
    loadNamespace('jsonlite')
    .system <- .Platform$OS.type

    if (.system == 'windows') {
        appdata_paths <- Sys.getenv(c('APPDATA', 'LOCALAPPDATA'))

        info_path = file.path(appdata_paths[1], 'Dropbox', 'info.json')
        if (! file.exists(info_path)) {
            info_path = file.path(appdata_paths[2], 'Dropbox', 'info.json')
        }
    } else if (.system == 'unix') {
        info_path <- path.expand('~/.dropbox/info.json')
    } else {
        stop(paste0("Unknown system = ", .system))
    }

    if (! file.exists(info_path)) {
        err_msg = paste0("Could not find the Dropbox info.json file! (Should be here: '", info_path, "')")
        stop(err_msg)
    }

    dropbox_settings <- jsonlite::fromJSON(info_path)
    paths <- vapply(dropbox_settings, function(account) {return(account$path)}, FUN.VALUE = '')
    return(paths)
}




# create_table <- function(con) {
#     sqlCreateTable(con, POSTGRES_TABLE, row.names = FALSE, temporary = FALSE)
# }

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
    grepl('[0-9]{5}', x, perl = TRUE)
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
            vin_one_digit <- substr(x, i, i + 1)
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
        vin_check_value <- mod(vin_check_value, 11)
        vin_check_str <- ifelse(vin_check_value < 10, as.character(vin_check_value), 'X')
        return(vin_check_str == actual_check_digit)
    }


    vapply(vins, is_valid_vin_once, logical(1))
}


load_df <- function(dta_file, con) {
    haven::read_dta(dta_file) %>%
    dplyr::select_(.dots = names(DATA_TYPES)) %>%
    dplyr::filter(sales_pr > 0) %>%
    dplyr::mutate(sell_zip = ifelse(is_valid_zip(sell_zip), sell_zip, NA_character_),
                  buy_zip  = ifelse(is_valid_zip(buy_zip),  buy_zip,  NA_character_),
                  vin      = ifelse(is_valid_vin(vin),      vin,      NA_character_)) %>%
    make_names_legal_sql(con) %>%
    return()
}



main <- function(verbose = TRUE) {
    dta_dir <- file.path(dropbox_home()[1],
                         'KarlJim/CarPriceData/MannheimDataNew_2002-2009')

    con <- dbConnect("PostgreSQL", dbname = POSTGRES_DB)
    if (DBI::dbExistsTable(con, POSTGRES_TABLE)) {
        if (verbose) {
            message('Deleting existing table.')
        }
        DBI::dbRemoveTable(con, POSTGRES_TABLE)
    }
    all_dta_files <- list.files(dta_dir, full.names = TRUE)
    lapply(all_dta_files, insert_into_postgres, con=con, verbose=verbose)
    DBI::dbDisconnect(con)
}

# Run things:
main()
