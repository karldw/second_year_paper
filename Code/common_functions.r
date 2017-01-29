

.pg_assert_existence <- function(con, table_name, col_name = NULL) {
    if (! DBI::dbExistsTable(con, table_name)) {
        err_msg <- sprintf("Table name '%s' is not in the database", table_name)
        stop(err_msg)
    }
    if (! is.null(col_name)) {
        known_cols <- DBI::dbListFields(con, table_name)
        if(! all(col_name %in% known_cols)) {
            unknown_cols <- setdiff(col_name, known_cols)
            column_columns <- if (length(unknown_cols) > 1) 'Columns' else 'Column'
            unknown_cols_str <- paste(unknown_cols, collapse = ', ')
            err_msg <- sprintf("%s '%s' not found in table '%s'.",
                               column_columns, unknown_cols_str, table_name)
            stop(err_msg)
        }
    }
    invisible()
}


pg_vacuum <- function(con, table_name='all', analyze=TRUE) {
    stopifnot(length(table_name) == 1)
    if (analyze) {
        sql_cmd <- "VACUUM ANALYZE"
    } else {
        sql_cmd <- "VACUUM"
    }
    if (table_name != 'all') {
        # default w/o table name is all tables in database
        .pg_assert_existence(con, table_name)
        sql_cmd <- paste(sql_cmd, table_name)
    }

    res <- DBI::dbSendStatement(con, sql_cmd)
    stopifnot(DBI::dbHasCompleted(res))
}


pg_add_index <- function(con, table_name, indexed_col, unique_index=FALSE) {
    # This function is here so I don't have to remember the SQL index syntax and so I
    # don't do anything too dumb. However, it definitely isn't safe or sanitized.
    # Obviously don't expose it to anyone malicious.
    # Note, postgres is smart enough that you don't need to index a column that's already
    # unique, but if you want to ALTER TABLE to make a primary key, you have to start
    # with a unique index.
    stopifnot(length(table_name) == 1, length(indexed_col) >= 1)
    .pg_assert_existence(con, table_name, indexed_col)
    index_name <- paste0(paste(indexed_col, collapse = '_'), '_index')

    if (unique_index) {
        unique_cmd <- 'UNIQUE'
    } else {
        unique_cmd <- ''
    }
    drop_cmd <- sprintf("DROP INDEX IF EXISTS %s", index_name)
    DBI::dbSendStatement(con, drop_cmd)
    # If there are multiple columns, make a comma-separated list
    indexed_col_str <- paste(indexed_col, collapse = ', ')
    # fillfactor to 100 because I'm never adding rows to this table
    add_cmd <- sprintf("CREATE %s INDEX %s on %s (%s) WITH (fillfactor = 100)",
                       unique_cmd, index_name, table_name, indexed_col_str)
    res <- DBI::dbSendStatement(con, add_cmd)
    stopifnot(DBI::dbHasCompleted(res))
    return(index_name)
}


pg_add_primary_key <- function(con, table_name, key_col) {
    # This function is here so I don't have to remember the SQL key syntax and so I
    # don't do anything too dumb. However, it definitely isn't safe or sanitized.
    # Obviously don't expose it to anyone malicious.
    stopifnot(length(table_name) == 1, length(key_col) >= 1)

    existing_index <- pg_add_index(con, table_name, key_col, unique_index = TRUE)

    sql_cmd <- sprintf("ALTER TABLE %s ADD PRIMARY KEY USING INDEX %s",
                       table_name, existing_index)
    res <- DBI::dbSendStatement(con, sql_cmd)
    stopifnot(DBI::dbHasCompleted(res))
}


pg_add_foreign_key <- function(con, table_name, column_name, reftable, refcolumn) {
    .pg_assert_existence(con, table_name, column_name)
    .pg_assert_existence(con, reftable, refcolumn)
    sql_cmd <- sprintf("ALTER TABLE %s ADD FOREIGN KEY (%s) REFERENCES %s (%s)",
                       table_name, column_name, reftable, refcolumn)

    res <- DBI::dbSendStatement(con, sql_cmd)
    stopifnot(DBI::dbHasCompleted(res))
}


pg_force_foreign_key <- function(con, table_name, column_name, reftable, refcolumn) {
    stop('not implemented or tested')
    .pg_assert_existence(con, table_name, column_name)
    .pg_assert_existence(con, reftable, refcolumn)

    # First, do an anti-join to set unmatched values of column_name to NULL
    # Then set a foreign key.
    sql_antijoin_update <- paste(
        sprintf("UPDATE '%s' SET '%s' = NULL", table_name, column_name),
        "WHERE NOT EXISTS (",
        sprintf("SELECT 1 FROM '%s'", reftable),
        sprintf("WHERE ('%s'.'%s' = '%s'.'%s')",
                table_name, column_name, reftable, refcolumn),
        ")")
    res <- DBI::dbSendStatement(con, sql_cmd)
    stopifnot(DBI::dbHasCompleted(res))
    pg_add_foreign_key(con, table_name, column_name, reftable, refcolumn)
}


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


box_home <- function() {
    os <- get_os()
    if (os == 'win') {
        appdata_paths <- Sys.getenv(c('APPDATA', 'LOCALAPPDATA'))

        info_path <- file.path(appdata_paths[1], 'Box Sync', 'sync_root_folder.txt')
        if (! file.exists(info_path)) {
            info_path <- file.path(appdata_paths[2], 'Box Sync', 'sync_root_folder.txt')
        }
    } else if (os == 'mac') {
        info_path <- path.expand('~/Library/Application Support/Box/Box Sync/sync_root_folder.txt')
    } else if (os == 'unix') {
        stop("Box doesn't support Linux/unix.  What are you doing?")
    }
    if (! file.exists(info_path)) {
        err_msg <- paste0("Could not find the Box sync_root_folder.txt file! ",
                         "(Should be here: '", info_path, "')")
        stop(err_msg)
    }

    box_dir <- readLines(info_path, warn = FALSE)
    if (! dir.exists(box_dir)) {
        err_msg <- paste0("Box configuration indicated the Box directory was '", box_dir,
                          "', but that doesn't exist.")
        stop(err_msg)
    }
    return(box_dir)
}


get_os <- function() {
    if (.Platform$OS.type == "windows") {
        "win"
    } else if (Sys.info()["sysname"] == "Darwin") {
        "mac"
    } else if (.Platform$OS.type == "unix") {
        "unix"
    } else {
        stop("Unknown OS")
    }
}


install_lazy <- function(pkg_list, verbose=TRUE) {
    installed_packages <- installed.packages()[, 1]
    need_to_install <- setdiff(pkg_list, installed_packages)
    already_installed <- pkg_list[pkg_list %in% installed_packages]
    for (pkg in need_to_install) {
        try(install.packages(pkg), silent=TRUE)
    }
    if (verbose) {
        message("Already installed:")
        print(already_installed)
        newly_installed <- need_to_install[need_to_install %in% installed.packages()]
        if (length(newly_installed) > 0) {
            message("Newly installed:")
            print(newly_installed)
        }
    }
    failed_to_install <- setdiff(need_to_install, installed.packages())
    if (length(failed_to_install) > 0) {
        warning("Failed to install these packages:\n  ", paste(failed_to_install))
    }
}


clear_all <- function() {
  # clear and close any open grapics devices, then delete everything.
    while (! is.null(dev.list())) {
        while(dev.flush() > 0) {
        # do nothing.
        }
        try(dev.off(), silent = TRUE)
    }
    rm(list = ls(envir = .GlobalEnv, all.names = TRUE, sorted = FALSE), envir = .GlobalEnv)
}


save_plot <- function(plt, name, scale_mult=1) {
    plot_dir <- '../Text/Plots'
    stopifnot(dir.exists(plot_dir))

    file.path(plot_dir, name) %>%
    ggsave(plt, width=6.3 * scale_mult, height=3.54 * scale_mult, device=cairo_pdf)
}


tag_alaskan_buyer <- function(df, as_factor=FALSE) {
    mutated <- mutate(df, alaskan_buyer = buy_state == 'AK')
    if (as_factor) {
        if ('tbl_lazy' %in% class(df)) {
            stop("Can't make a factor in a a remote table.")
        }
        mutated <- mutate(mutated, alaskan_buyer = bool_to_alaska_factor(alaskan_buyer))
    }
    return(mutated)
}


bool_to_alaska_factor <- function(x, labels=c('Alaskan', 'Non-Alaskan')) {
    factor(x, levels=c(TRUE, FALSE), labels=labels)
}


ensure_id_vars_ <- function(df, claimed_id_vars) {
    not_found_vars <- setdiff(claimed_id_vars, names(df))
    if (length(not_found_vars) > 0) {
        err_msg <- sprintf("Claimed ID vars not in dataset: %s", paste(not_found_vars, collapse=', '))
        stop(err_msg)
    }
    df_id_cols_only <- dplyr::select_(df, .dots=claimed_id_vars)
    if (anyNA(df_id_cols_only)) {
        stop("ID variables cannot be NA.")
    }
    # nrow is NA for databases (not an issue here, but I may want this code later)
    # (should be rare enough that it's not worth forcing a database to count all rows)
    if ((! is.na(nrow(df))) && (nrow(df) == 0)) {
        stop("No rows!")
    }
    # anyDuplicated is faster than calling "distinct" then counting rows
    if (anyDuplicated(df_id_cols_only)) {
        err_msg <- sprintf("The variables '%s' do not uniquely identify rows.",
                           paste(claimed_id_vars, collapse="', '"))
        stop(err_msg)
    }
    # return so we can pipe this
    return(df)
}


ensure_id_vars <- function(df, ...) {
    lzydots <- lazyeval::lazy_dots(...)
    claimed_id_vars <- vapply(seq_along(lzydots),
                              function(i) {as.character(lzydots[[i]]$expr)},
                              character(1))
    ensure_id_vars_(df, claimed_id_vars) %>% return()
}


is_id <- function(df, claimed_id_vars) {
    # Note: it's probably a good idea to force computation on df, if it's a remote table
    stopifnot(is.character(claimed_id_vars) && length(claimed_id_vars) > 0)
    # select one row to get variable names
    if ('tbl_lazy' %in% class(df)) {
        df_head1 <- head(df, 1) %>% dplyr::collect(df_head1)
        df_is_local <- FALSE
    } else {
        df_head1 <- head(df, 1)
        df_is_local <- TRUE
    }

    not_found_vars <- setdiff(claimed_id_vars, names(df_head1))
    if (length(not_found_vars) > 0) {
        err_msg <- sprintf("Claimed ID vars not in dataset: %s",
                           paste(not_found_vars, collapse=', '))
        warning(err_msg)
        return(FALSE)
    }

    df_id_cols_only <- dplyr::select_(df, .dots=claimed_id_vars)
    if (df_is_local) {
        ids_have_na <- anyNA(df_id_cols_only)
    } else {
        ids_have_na <- df_id_cols_only %>%
            dplyr::ungroup() %>%
            dplyr::summarise_all(dplyr::funs(any(is.na(.)))) %>%
            collect() %>% unlist() %>% any()
    }
    if (ids_have_na) {
        warning("ID variables cannot be NA.")
        return(FALSE)
    }

    if (df_is_local) {
        # anyDuplicated is faster than calling "distinct" then counting rows, but
        # remote tables don't support anyDuplicated, so do it manually there.
        ids_are_unique <- anyDuplicated(df_id_cols_only) == 0
    } else {
        distinct_row_count <- dplyr::ungroup(df_id_cols_only) %>%
            dplyr::distinct() %>%
            force_nrow()
        total_row_count <- force_nrow(df_id_cols_only)
        ids_are_unique <- total_row_count == distinct_row_count
    }
    return(ids_are_unique)
}


make_join_safer <- function(join_fn) {
    # before doing the join, make sure that the by variables uniquely identify rows in
    # at least one of the tables
    output_fn <- function(x, y, by, ..., allow.cartesian=FALSE) {
        if (missing(by) || is.null(by) || is.na(by)) {
            stop("Please specify your 'by' variables explicitly.")
        }
        by_y <- unname(by)
        if (! is.null(names(by))) {
            by_x <- names(by)
        } else {
            by_x <- by_y
        }

        if (! allow.cartesian) {
            # force computation on x because it'll help is_id() a lot
            if ('tbl_lazy' %in% class(x)) {
                x <- dplyr::compute(x)
            }
            if (! is_id(x, by_x)) {
                # iff x isn't IDed by the by_x variables, then turn to y
                # force computation on y too
                if ('tbl_lazy' %in% class(y)) {
                    y <- dplyr::compute(y)
                }
                if (! is_id(y, by_y)) {
                    err_msg <- "Neither table is uniquely identified by their 'by' variables!"
                    stop(err_msg)
                }
            }
        }

        join_results <- join_fn(x=x, y=y, by=by, ...)
        ## A faster, but less complete way would be to count rows and throw and error
        ## if the number of results was larger than the sum of input rows.
        # nrow_x <- force_nrow(x)
        # nrow_y <- force_nrow(y)
        # nrow_join_results <- force_nrow(join_results)
        # if (nrow_join_results > (nrow_x + nrow_y)) {
        #     err_msg <- paste(
        #         sprintf("Join results in %s rows; more than %s = nrow(x)+nrow(i).",
        #                 nrow_join_results, nrow_x + nrow_y),
        #         "Check for duplicate key values your by-variables in each table,",
        #         "each of which join to the same values over and over again. If you",
        #         "are sure you wish to proceed, rerun with allow.cartesian=TRUE.",
        #         "Also see the help for data.table.")
        #     stop(err_msg)
        # }
        return(join_results)
    }
    return(output_fn)
}


force_nrow <- function(df) {
    library(magrittr)
    # get the row count.
    # for remote tables, force the row count.
    nrow_df <- nrow(df)
    if (is.na(nrow_df)) {
        nrow_df <- ungroup(df) %>% summarize(n=n()) %>% collect() %$% n %>% as.integer()
    }
    stopifnot(! anyNA(nrow_df))
    return(nrow_df)
}


tbl_has_rows <- function(df) {
    # Works for both local tables and remote databases
    nrow_df <- nrow(df)
    if (is.na(nrow_df)) {  # nrow() is NA for remote tables
        head1 <- ungroup(df) %>% head(1) %>% collect()
        has_rows <- nrow(head1) > 0
    } else {
        has_rows <-  nrow_df > 0
    }
    return(has_rows)
}


lapply_bind_rows <- function(X, FUN, ..., rbind_src_id=NULL, parallel_cores=NULL) {
    # just like lapply, but bind the results together at the end (plus parallelization)

    # Error out early if any of these packages aren't available.
    loadNamespace('parallel')  # requires parallel to be installed
    loadNamespace('dplyr')
    if (is.atomic(X)) {
        loadNamespace('lazyeval')
    }

    # First, figure out how many cores to use.
    # With windows, must use 1.
    if (is.null(parallel_cores) || is.na(parallel_cores)) {
        if (get_os() == 'win') {
            parallel_cores <- 1
        } else {
            # Find how many cores the machine has, counting only physical (rather than
            # logical) cores. That is, ignore hyperthreading.
            parallel_cores <- parallel::detectCores(logical = FALSE)
            if (is.na(parallel_cores)) {
                parallel_cores <- 1
            }
        }
    }
    stopifnot(length(parallel_cores) == 1, parallel_cores == as.integer(parallel_cores),
              length(X) >= 1)

    list_results <- parallel::mclapply(X = X, FUN = FUN, mc.cores = parallel_cores, ...)
    # TODO: implement dplyr::union_all for non-local sources.


    bound_df <- dplyr::bind_rows(list_results, .id = rbind_src_id)

    if ((! is.null(rbind_src_id)) && is.atomic(X)) {
        if (rbind_src_id %in% names(list_results[[1]])) {
            err_msg <- sprintf("Name '%s' already exists, can't add it as an rbind ID.",
                               rbind_src_id)
            stop(err_msg)
        }

        # Then mutate the values of rbind_src_id column to be the *values* of X, rather
        # than the default, which is seq_along(X).
        mutate_call <- list(lazyeval::interp(~ X[as.integer(rbind_src_id)],
            rbind_src_id = as.name(rbind_src_id), X = X))
        bound_df <- dplyr::mutate_(bound_df, .dots = setNames(mutate_call, rbind_src_id))
    }
    return(bound_df)
}


# could this be more elegant? definitely
first_thursday_in_october <- function(years) {
    first_thursday_in_october_one_year <- function(year) {
        stopifnot(length(year) == 1L)
        current_date <- lubridate::make_date(year, 10, 1)
        # Thursday is weekday 5
        while (lubridate::wday(current_date) != 5) {
            current_date <- current_date + 1
        }
        return(current_date)
    }
    first_thursday_in_october_one_year <- memoise(first_thursday_in_october_one_year)
    thursdays <- vapply(X = years, FUN = first_thursday_in_october_one_year,
                        FUN.VALUE = as.Date('1970-01-01')) %>%
                as.Date(origin='1970-01-01')
    return(thursdays)
}


filter_event_window <- function(.data, year, days_before = 30, days_after = days_before) {
    stopifnot(length(year) == 1, length(days_before) == 1, length(days_after) == 1,
              is.numeric(days_before), is.numeric(days_after), days_before > 0,
              days_after > 0, between(year, 2002, 2014))
    dividend_day <- first_thursday_in_october(year)
    window_begin <- dividend_day - days_before
    window_end <- dividend_day + days_after
    if (any(lubridate::year(c(window_begin, window_end)) != year)) {
        stop("You've selected a window that spans more than one year. The code (not ",
             "just in this function, but everywhere) wasn't designed for this and ",
             "will probably have bugs.")
    }
    if ('tbl_postgres' %in% class(.data)) {
        # First, borrow the existing SQL query (translated from the dplyr stuff)
        existing_query <- dplyr::sql_render(.data, con = .data$src$con)
        # Then write custom SQL because dplyr doesn't support BETWEEN DATE.
        date_filter <- sprintf("sale_date BETWEEN DATE '%s' and DATE '%s'",
                               window_begin, window_end)
        # Combine the queries back together, giving the table a random name.
        new_query <- paste0("SELECT * \n",
                           "FROM (", existing_query,") ",
                           '"', dplyr:::random_table_name(), '"\n',
                           "WHERE (", date_filter, ")")
        # This probably isn't the dplyr-sanctioned way to create this query,
        # but I think it works.
        data_one_year <- tbl(con, sql(new_query))
    } else if ('data.frame' %in% class(.data)) {
        # The local data case is easy:
        data_one_year <- .data %>%
            dplyr::filter(dplyr::between(sale_date, window_begin, window_end))
    } else {
        stop("Sorry, I don't know how to subset sale_date here.")
    }
    return(data_one_year)
}


explain_analyze <- function(x) {
    # Just like dplyr::explain, but running the more detailed EXPLAIN ANALYZE
    force(x)
    stopifnot('tbl_postgres' %in% class(x))
    dplyr::show_query(x)
    message("\n")
    exsql <- dplyr::build_sql("EXPLAIN ANALYZE ", dplyr::sql_render(x))
    expl_raw <- RPostgreSQL::dbGetQuery(x$src$con, exsql)
    expl <- paste(expl_raw[[1]], collapse = "\n")

    message("<PLAN>\n", expl)
    invisible(NULL)
}


winsorize <- function(df, vars_to_winsorize, quantiles=c(1, 99)) {
    quantiles <- sort(quantiles)
    if (! all(quantiles <= 1)) {
        quantiles <- quantiles / 100
    }
    if ('data.frame' %in% class(df)) {
        # can only check for local ones here
        stopifnot(all(vars_to_winsorize %in% names(df)))
    }

    df <- ungroup(df) %>% compute()
    orig_names <- head(df, 1) %>% collect() %>% names()

    stopifnot(length(quantiles) == 2,
              all(quantiles > 0 & quantiles < 1),
              all(vars_to_winsorize %in% orig_names)
              )
    for (var in vars_to_winsorize) {
        new_vec <- df %>% dplyr::select_(.dots = var) %>%
            dplyr::collect(n = Inf) %>%
            magrittr::extract2(var)
        # 8 is apparently a good type of quantile
        new_range <- unname(quantile(new_vec, quantiles, na.rm = TRUE, type = 8))
        mutate_call <- lazyeval::interp(~ if_else(var < lower, lower,
                                                  if_else(var > higher, higher, var)),
            var = as.name(var), lower = new_range[1], higher = new_range[2])
        df <- dplyr::mutate_(df, .dots = setNames(list(mutate_call), var))

        # new_vec[new_vec < new_range[1]] <- new_range[1]
        # new_vec[new_vec > new_range[2]] <- new_range[2]
        # df[[var]] <- new_vec
    }
    # reset the column order to its original state
    df %>% select_(.dots = orig_names) %>% return()
}


add_sale_year <- function(.data) {
    if ('tbl_postgres' %in% class(.data)) {
        # This is the case at the time of writing
        # Note, I'm testing with tbl_postgres rather than tbl_sql because I'm about to
        # use some postgres-specific syntax
        out <- .data %>%
            mutate(sale_year = date_part('year', sale_date))
    } else if ('data.frame' %in% class(.data)){
        stopifnot('sale_date' %in% names(.data))
        # this would be the case if I had collect()-ed the data.
        out <- .data %>% mutate(sale_year = lubridate::year(sale_date))
    } else {
        stop("Sorry, I don't know how to calculate sale_year here.")
    }
    return(out)
}


add_sale_dow <- function(.data) {
    if ('tbl_postgres' %in% class(.data)) {
        # This is the case at the time of writing
        # Note, I'm testing with tbl_postgres rather than tbl_sql because I'm about to
        # use some postgres-specific syntax
        # Note the + 1 because postgres defines numeric weekday differently than lubridate
        out <- .data %>%
            mutate(sale_dowr = date_part('dow', sale_date) + 1)
    } else if ('data.frame' %in% class(.data)){
        # this would be the case if I had collect()-ed the data.
        stopifnot('sale_date' %in% names(.data))
        # Don't recalculate if it's unnecessary.
        if (! 'sale_dow' %in% names(.data)) {
            out <- .data %>% mutate(sale_dow = lubridate::wday(sale_date))
        } else {
            out <- .data
        }
    } else {
        stop("Sorry, I don't know how to calculate sale_dow here.")
    }
    return(out)
}


add_event_time <- function(.data) {
    # First, make a table mapping sale_date to event_time for this input .data
    # Then merge back in.
    # .data can be local or in postgres.

    dates_tbl <- .data %>%
        ungroup() %>%  # avoid dplyr bug that adds a copy of the grouping column (#2109)
        select(sale_date) %>%  # avoid dplyr bug that tries to select all cols (#2359)
        distinct(sale_date) %>%
        add_sale_year() %>%
        collect() %>%
        # Note: doing it like this, based on the sale_year, assumes that my window
        # fits entirely within the year.
        mutate(dividend_day = first_thursday_in_october(sale_year),
               event_time = as.integer(sale_date - dividend_day),
               event_week = event_time %/% 7L) %>%
        select(sale_date, event_time, event_week)

    max_event_time <- max(abs(dates_tbl$event_time))
    if (max_event_time > 85) {
        # 85 because October 7 (the latest possible first Thursday) to
        # December 31 is 85 days.
        stop(sprintf("Largest magnitude of event_time is %s. ", max_event_time),
             "Values larger than 85 can span years, which is a problem as the code is ",
             "currently written.")
    }
    # take the calculated event times back to the original table.
    # copy = TRUE will copy the local dates_tbl back to postgres
    # (copy = TRUE copies the second table to the location of the first)
    out <- left_join(.data, dates_tbl, by = 'sale_date', copy = TRUE)
    return(out)
}


is_varname_in <- function(.data, varname) {
    # Pull one row to see for remote tables.
    one_row <- head(.data, 1) %>% collect()
    out <- varname %in% names(one_row)
    return(out)
}


find_match_states_crude_unmemoized <- function(n_auction_states = 3, n_buy_states = 3) {
    # Find what auction states Alaskan buyers buy in.
    # Then look at what other states buy in those states.

    stopifnot(length(n_auction_states) == 1, is.numeric(n_auction_states),
              as.integer(n_auction_states) == n_auction_states, n_auction_states > 0,
              length(n_buy_states) == 1, is.numeric(n_buy_states),
              as.integer(n_buy_states) == n_buy_states, n_buy_states > 0)
    counts_by_buy_auction_cross <- auctions %>%
        select(buy_state, auction_state) %>%
        filter(! is.na(buy_state), ! is.na(auction_state)) %>%
        group_by(buy_state, auction_state) %>%
        summarize(count = n()) %>%
        group_by(buy_state) %>%
        mutate(buy_pct = 100 * count / sum(count)) %>%
        ungroup() %>%
        compute()
    top_alaska_auction_states <- counts_by_buy_auction_cross %>%
        filter(buy_state == 'AK') %>%
        arrange(-buy_pct) %>%
        head(n_auction_states)
    min_buy_pct <- top_alaska_auction_states %>% select(buy_pct) %>%
        collect() %$% buy_pct %>% min()

    top_buyer_states <- counts_by_buy_auction_cross %>%
        filter(auction_state != buy_state, buy_state != 'AK', buy_pct > min_buy_pct) %>%
        semi_join(top_alaska_auction_states, by = 'auction_state') %>%
        arrange(-buy_pct) %>%
        collect() %>%
        # Now I'm relying on row order not changing:
        distinct(buy_state) %>%
        head(n_buy_states) %$%
        buy_state
    return(top_buyer_states)
}
if (! existsFunction('find_match_states_crude')) {
    install_lazy('memoise', FALSE)
    find_match_states_crude <- memoise::memoize(find_match_states_crude_unmemoized)
}
