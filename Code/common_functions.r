

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
            unknown_cols_str <- vec2string(unknown_cols)
            err_msg <- sprintf("%s %s not found in table '%s'.",
                               column_columns, unknown_cols_str, table_name)
            stop(err_msg)
        }
    }
    invisible()
}


pg_vacuum <- function(con, table_name = 'all', analyze = TRUE) {
    stopifnot(length(table_name) == 1)
    if (analyze) {
        sql_cmd <- "VACUUM FREEZE ANALYZE"
    } else {
        sql_cmd <- "VACUUM FREEZE"
    }
    if (table_name != 'all') {
        # default w/o table name is all tables in database
        .pg_assert_existence(con, table_name)
        sql_cmd <- paste(sql_cmd, table_name)
    }

    res <- DBI::dbSendStatement(con, sql_cmd)
    stopifnot(DBI::dbHasCompleted(res))
}


pg_add_index <- function(con, table_name, indexed_col, unique_index = FALSE,
                         drop_existing = FALSE) {
    # This function is here so I don't have to remember the SQL index syntax and so I
    # don't do anything too dumb. However, it definitely isn't safe or sanitized.
    # Obviously don't expose it to anyone malicious.
    # Note, postgres is smart enough that you don't need to index a column that's already
    # unique, but if you want to ALTER TABLE to make a primary key, you have to start
    # with a unique index.
    stopifnot(length(table_name) == 1, length(indexed_col) >= 1)
    # Doesn't work with temp tables.  Instead, just reply on SQL to complain.
    # .pg_assert_existence(con, table_name, indexed_col)
    index_name <- paste0(paste(indexed_col, collapse = '_'), '_index')

    if (unique_index) {
        unique_cmd <- 'UNIQUE'
    } else {
        unique_cmd <- ''
    }
    if (drop_existing) {
        drop_cmd <- sprintf("DROP INDEX IF EXISTS %s", index_name)
        DBI::dbSendStatement(con, drop_cmd)
    }
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
        err_msg = paste0("Could not find the Dropbox info.json file! (Should be here: '",
                         info_path, "')")
        stop(err_msg)
    }

    dropbox_settings <- jsonlite::fromJSON(info_path)
    paths <- vapply(dropbox_settings, function(account) {return(account$path)},
                    FUN.VALUE = '')
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
        mac_path <- '~/Library/Application Support/Box/Box Sync/sync_root_folder.txt'
        info_path <- path.expand(mac_path)
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


install_lazy <- function(pkg_list, verbose = FALSE) {
    # installed_packages <- installed.packages()[, 1]

    need_to_install <- pkg_list[! is_pkg_installed(pkg_list)]
    already_installed <- setdiff(pkg_list, need_to_install)
    for (pkg in need_to_install) {
        try(install.packages(pkg), silent = TRUE)
    }
    failed_to_install <- need_to_install[! is_pkg_installed(need_to_install)]
    if (verbose) {
        message("Already installed:")
        print(already_installed)
        newly_installed <- setdiff(need_to_install, failed_to_install)

        if (length(newly_installed) > 0) {
            message("Newly installed:")
            print(newly_installed)
        }
    }
    if (length(failed_to_install) > 0) {
        warning("Failed to install these packages:\n  ", paste(failed_to_install))
    }
}


is_pkg_installed <- function(pkg_list) {
    vapply(pkg_list, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
}


is_pkg_up_to_date <- function(pkg_list, desired_versions) {
    stopifnot(length(pkg_list) == length(desired_versions),
              length(pkg_list) >= 1, is.character(pkg_list[[1]]),
              is.character(desired_versions[[1]]))
    is_pkg_up_to_date_once <- function(pkg_name, desired_version) {
        pkg_ver <- as.character(packageVersion(pkg_name))
        compare_result <- compareVersion(pkg_ver, desired_version)
        up_to_date <- compare_result %in% c(0, 1)  # -1 if out of date
        return(up_to_date)
    }
    results <- mapply(is_pkg_up_to_date_once, pkg_list, desired_versions, SIMPLIFY = TRUE)
    return(results)
}


clear_all <- function() {
  # clear and close any open grapics devices, then delete everything.
    while (! is.null(dev.list())) {
        while(dev.flush() > 0) {
        # do nothing.
        }
        try(dev.off(), silent = TRUE)
    }
    rm(list = ls(envir = .GlobalEnv, all.names = TRUE, sorted = FALSE),
       envir = .GlobalEnv)
}


save_plot <- function(plt, name, scale_mult = 1, overwrite = TRUE, aspect_ratio = 16/9,
    spell_ignore = c('conf', 'dev', 'msrp', 'frac', hunspell::en_stats)) {

    # aspect_ratio is a scale factor for height vs width. The default is width:height of
    # 16:9, like a lot of TV screens. Other good choices might be 4/3 or 2/sqrt(2).
    # The default will exactly match a full screen 16:9 beamer slide.
    plot_dir <- '../Text/Plots'
    stopifnot(dir.exists(plot_dir), is.character(name), length(name) == 1,
              grepl('.+\\.pdf$', name, perl = TRUE, ignore.case = TRUE),
              is.logical(overwrite))
    if (! is_pkg_installed('hrbrthemes')) {
        install_lazy(c('devtools', 'hunspell'))
        devtools::install_github('hrbrmstr/hrbrthemes')
    }
    outfile <- file.path(plot_dir, name)
    if (file.exists(outfile) && (! overwrite)) {
        # This isn't perfect, since there's now a sliver of time between the file
        # check and the writing, but I can't see how to make cairo do that.
        err_msg <- paste("Plot destination already exists:\n  %s", outfile)
        stop(err_msg)
    }
    # Chose 6.3 in to match a 16:9 beamer slide.
    width <- 6.3 * scale_mult
    height <- width / aspect_ratio
    # Spellcheck my plot labels:
    hrbrthemes::gg_check(plt, ignore = spell_ignore)
    ggplot2::ggsave(outfile, plt, device = cairo_pdf, dpi = 600,
                    width = width, height = height, units = 'in')
}


tag_alaskan_buyer <- function(df, as_factor = FALSE) {
    mutated <- mutate(df, alaskan_buyer = buy_state == 'AK')
    if (as_factor) {
        if ('tbl_lazy' %in% class(df)) {
            stop("Can't make a factor in a a remote table.")
        }
        mutated <- mutate(mutated, alaskan_buyer = bool_to_alaska_factor(alaskan_buyer))
    }
    return(mutated)
}


bool_to_alaska_factor <- function(x, labels = c('Alaskan', 'Non-Alaskan')) {
    factor(x, levels = c(TRUE, FALSE), labels = labels)
}


ensure_id_vars_ <- function(.tbl, claimed_id_vars) {
    if (! is_id_(.tbl, claimed_id_vars)) {
        stop("Variables don't uniquely identify rows")
    }
    return(.tbl)
}

# This works, but it makes error tracing harder.
# make_nse_fn <- function(standard_eval_fn) {
#     out_fn <- function(.tbl, ...) {
#         # Note: there's probably a better way to do this
#         lzydots <- lazyeval::lazy_dots(...)
#         lazy_expressions <- vapply(seq_along(lzydots),
#             function(i) {as.character(lzydots[[i]]$expr)}, character(1))
#         return(standard_eval_fn(.tbl, lazy_expressions))
#     }
#     return(out_fn)
# }
# ensure_id_vars <- make_nse_fn(ensure_id_vars_)

dots_to_names <- function(..., strict = TRUE) {
    lzydots <- lazyeval::lazy_dots(...)
    # I don't care about the names of the lazy_dots, I just want to translate, e.g.,
    # demean(mtcars, cyl, disp) to demean_(mtcars, 'cyl', 'disp')
    out_names <- vapply(seq_along(lzydots),
        function(i) {as.character(lzydots[[i]]$expr)}, character(1))
    mismatched_names <- out_names[make.names(out_names) != out_names]
    if (strict && length(mismatched_names) > 0) {
        err_msg <- sprintf("Some names don't look right: %s",
                           vec2string(mismatched_names))
        stop(err_msg)
    }
    return(out_names)
}


ensure_id_vars <- function(df, ...) {
    claimed_id_vars <- dots_to_names(...)
    return(ensure_id_vars_(df, claimed_id_vars))
}


is_id_ <- function(df, claimed_id_vars, quiet = FALSE) {
    # TODO: this should really use classes...
    # NB It might be a good idea to force computation on df, if it's a remote table
    stopifnot(is.character(claimed_id_vars), length(claimed_id_vars) > 0,
              is.logical(quiet))
    # select one row to get variable names
    df_head1 <- head(df, 1L)
    if ('tbl_lazy' %in% class(df)) {
        df_head1 <- dplyr::collect(df_head1)
        df_is_local <- FALSE
    } else {
        df_is_local <- TRUE
    }

    not_found_vars <- setdiff(claimed_id_vars, names(df_head1))
    if (length(not_found_vars) > 0) {
        if (! quiet) {
            err_msg <- sprintf("Claimed ID vars not in dataset: %s",
                               vec2string(not_found_vars))
            warning(err_msg)
        }
        return(FALSE)
    }

    df_id_cols_only <- dplyr::select_(df, .dots = claimed_id_vars)
    if (df_is_local) {
        ids_have_na <- anyNA(df_id_cols_only)
    } else {
        # TODO (eventually): this part could probably be faster.  For one thing, finding
        # an NA in any of the columns is enough, we don't need to check all at once.
        ids_have_na <- df_id_cols_only %>%
            dplyr::ungroup() %>%
            dplyr::summarise_all(dplyr::funs(any(is.na(.)))) %>%
            collect() %>% unlist() %>% any()
    }
    if (ids_have_na) {
        if (! quiet) {
            warning("ID variables cannot be NA.")
        }
        return(FALSE)
    }
    total_row_count <- force_nrow(df_id_cols_only)
    if (total_row_count == 0) {
        if (! quiet) {
            warning("No rows!")
        }
        return(FALSE)
    }

    if (df_is_local) {
        # anyDuplicated is faster than calling "distinct" then counting rows, but
        # remote tables don't support anyDuplicated, so do it manually there.
        ids_are_unique <- anyDuplicated(df_id_cols_only) == 0
    } else if ('tbl_sql' %in% class(df)){
        con_psql <- df_id_cols_only$src$con
        from <- dplyr::sql_subquery(con_psql, dplyr::sql_render(df_id_cols_only),
                                    name = NULL)
        group_sql <- dplyr::sql(paste(claimed_id_vars, collapse = ', '))
        # Use the HAVING clause, as done here: http://stackoverflow.com/a/28157109
        # Limit to one row because we only need to know that at least one group has
        # count > 1.

        distinct_query <- dplyr::sql_select(con = con_psql, select = sql('count(*)'),
            from = from, group_by = group_sql, having = dplyr::sql('count(*) > 1'),
            limit = 1L)
        distinct_tbl <- dplyr::tbl(df_id_cols_only$src, distinct_query)
        distinct_tbl <- dplyr::collect(distinct_tbl)
        ids_are_unique <- (! tbl_has_rows(distinct_tbl))

    } else {
        distinct_row_count <- dplyr::ungroup(df_id_cols_only) %>%
            dplyr::distinct() %>%
            force_nrow()
        ids_are_unique <- total_row_count == distinct_row_count
    }
    return(ids_are_unique)
}


is_id <- function(df, ..., quiet = FALSE) {
    claimed_id_vars <- dots_to_names(...)
    return(is_id_(df, claimed_id_vars, quiet = quiet))
}


make_join_safer <- function(join_fn, fast = TRUE) {
    # If fast == TRUE, do the join, then check that the number of rows is not greater than
    # the max row count of the input tables.
    # If fast == FALSE, make sure that the by variables uniquely identify rows in at least
    # one of the tables before doing the join.

    if (fast) {
        output_fn <- function(x, y, ..., allow.cartesian = FALSE) {
            join_results <- join_fn(x = x, y = y, ...)

            # A faster, but less complete way would be to count rows and throw and error
            # if the number of results was larger than the sum of input rows.
            sum_nrow_xy <- force_nrow(x) + force_nrow(y)
            nrow_join_results <- force_nrow(join_results)
            if (nrow_join_results > sum_nrow_xy) {
                err_msg <- paste("Join results in",
                    sprintf("%s rows; more than %s = nrow(x) + nrow(y).",
                            nrow_join_results, sum_nrow_xy),
                    "Check for duplicate key values your by-variables in each table,",
                    "each of which join to the same values over and over again. If you",
                    "are sure you wish to proceed, rerun with allow.cartesian = TRUE.",
                    "Also see the help for data.table.")
                stop(err_msg)
            }
            return(join_results)
        }
    } else {
        # You can also do it by actually checking uniqueness, but that's usually not
        # necessary.
        output_fn <- function(x, y, by, ..., allow.cartesian = FALSE) {
            if (missing(by) || is.null(by) || is.na(by)) {
                stop("Please specify your 'by' variables explicitly.")
            }
            if (! allow.cartesian) {
                by_y <- unname(by)
                if (! is.null(names(by))) {
                    by_x <- names(by)
                } else {
                    by_x <- by_y
                }

                # force computation on x because it'll help is_id() a lot
                # TODO: do timing tests on the previous sentence
                if ('tbl_lazy' %in% class(x)) {
                    x <- dplyr::compute(x)
                }
                if (! is_id_(x, by_x)) {
                    # iff x isn't IDed by the by_x variables, then turn to y
                    # force computation on y too
                    if ('tbl_lazy' %in% class(y)) {
                        y <- dplyr::compute(y)
                    }
                    if (! is_id_(y, by_y)) {
                        err_msg <- paste("Neither table is uniquely identified by",
                                         "their 'by' variables!")
                        stop(err_msg)
                    }
                }
            }

            join_results <- join_fn(x = x, y = y, by = by, ...)
            return(join_results)
        }
    }
    return(output_fn)
}


force_nrow <- function(df) {
    library(magrittr)
    # get the row count.
    # for remote tables, force the row count.
    nrow_df <- nrow(df)
    if (is.na(nrow_df)) {
        nrow_df <- ungroup(df) %>% summarize(n = n()) %>% collect() %$% n %>% as.integer()
    }
    stopifnot(! anyNA(nrow_df))
    return(nrow_df)
}


tbl_has_rows <- function(df) {
    # Works for both local tables and remote databases
    nrow_df <- nrow(df)
    if (is.na(nrow_df)) {  # nrow() is tbl for remote tables
        head1 <- ungroup(df) %>% head(1) %>% collect()
        has_rows <- nrow(head1) > 0
    } else {
        has_rows <-  nrow_df > 0
    }
    return(has_rows)
}


lapply_parallel <- function(X, FUN, ..., mc.cores = NULL) {
    # Like parallel::mclapply, but with easier core detection.

    stopifnot(is_pkg_installed('parallel'), length(X) >= 1, is.function(FUN))
    # First, figure out how many cores to use.
    # With windows, must use 1.
    if (is.null(mc.cores) || is.na(mc.cores)) {
        if (get_os() == 'win') {
            mc.cores <- 1
        } else {
            # Find how many cores the machine has, counting only physical (rather than
            # logical) cores. That is, ignore hyperthreading.
            mc.cores <- parallel::detectCores(logical = FALSE)
            if (is.na(mc.cores)) {
                mc.cores <- 1
            }
        }
    }
    stopifnot(length(mc.cores) == 1, mc.cores == as.integer(mc.cores))
    # mclapply only gives a warning if scheduled cores experience errors.
    # set warn = 2 so warnings become errors
    orig_warn <- getOption('warn')
    on.exit(options(warn = orig_warn), add = TRUE)
    options(warn = 2)
    list_results <- parallel::mclapply(X = X, FUN = FUN, mc.cores = mc.cores, ...)
    return(list_results)
}


lapply_bind_rows <- function(X, FUN, ..., rbind_src_id = NULL, parallel_cores = NULL) {
    # Error out early if any of these packages aren't available.
    stopifnot(is_pkg_installed('dplyr'))
    if (is.atomic(X)) {
        stopifnot(is_pkg_installed('lazyeval'))
    }
    # just like lapply, but bind the results together at the end (plus parallelization)
    list_results <- lapply_parallel(X = X, FUN = FUN, ..., mc.cores = parallel_cores)
    list_results_class <- class(list_results[[1]])

    if ('data.frame' %in% list_results_class) {
        out <- dplyr::bind_rows(list_results, .id = rbind_src_id)

        if ((! is.null(rbind_src_id)) && is.atomic(X)) {
            # Then mutate the values of rbind_src_id column to be the *values* of X,
            # rather than the default, which is seq_along(X).
            if (rbind_src_id %in% names(list_results[[1]])) {
                err <- sprintf("Name '%s' already exists, can't add it as an rbind ID.",
                               rbind_src_id)
                stop(err)
            }
            mutate_call <- list(lazyeval::interp(~ X[as.integer(rbind_src_id)],
                rbind_src_id = as.name(rbind_src_id), X = X))
            out <- dplyr::mutate_(out, .dots = setNames(mutate_call, rbind_src_id))
        }
    } else if ('tbl_sql' %in% list_results_class){
        # As above, we want to add an ID column.  The name of the column is provided by
        # rbind_src_id.  Unlike bind_rows, we have to add it manually, even if X is not
        # atomic.  So, the add_src_id function looks at X and picks either the value
        # of X or the value of the index to add as the column name. The mutate_ call
        # is relatively simple because I'm just adding a constant.
        # Then we use union_all, which translates to SQL's UNION ALL, to bind the tables
        # into one. union_all only takes two tables, so use Reduce to bring them all
        # together.
        if (! is.null(rbind_src_id)) {
            add_src_id <- function(idx) {
                if (is.atomic(X)) {
                    X_val <- X[idx]
                } else {
                    X_val <- idx
                }
                list_item <- list_results[[idx]]
                dots <- setNames(list(X_val), rbind_src_id)
                dplyr::mutate_(list_item, .dots = dots) %>% return()
            }
            list_results <- lapply(seq_along(list_results), add_src_id)
        }
        out <- Reduce(dplyr::union_all, list_results)
    } else {
        stop(sprintf("Sorry, not sure how to bind rows for results of class '%s'.",
                     list_results_class))
    }
    return(out)
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
                as.Date(origin = '1970-01-01')
    return(thursdays)
}


filter_event_window_one_year <- function(.data, year, days_before = 30,
        days_after = days_before) {
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
    .data <- ungroup(.data)
    if ('tbl_sql' %in% class(.data)) {
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
        err_msg <- sprintf(
            "Sorry, I don't know how to subset sale_date for data of class %s",
            vec2string(class(.data)))
        stop(err_msg)
    }
    return(data_one_year)
}


filter_event_window <- function(.data, years = NULL, days_before = 30L,
        days_after = days_before) {
    if (is.null(years)) {
        years <- .data %>% ungroup() %>% select(sale_date) %>%
            add_sale_year() %>% select(sale_year) %>% distinct(sale_year) %>%
            collect() %$% sale_year
    }
    # rely on the union_all I've written into lapply_bind_rows
    out <- lapply_bind_rows(years, filter_event_window_one_year,
                            .data = .data,
                            days_before = days_before, days_after = days_after,
                            # Make a sale_year column
                            rbind_src_id = 'sale_year',
                            # this is all lazy; don't bother parallel
                            parallel_cores = 1)
    return(out)
}


explain <- function(x, analyze = FALSE) {
    # Just like dplyr::explain, but pipe-able and optionally running EXPLAIN ANALYZE
    force(x)
    stopifnot('tbl_sql' %in% class(x))
    dplyr::show_query(x)
    message("\n")
    if (! analyze) {
        explain_txt <- 'EXPLAIN '
    } else {
        explain_txt <- 'EXPLAIN ANALYZE '
    }

    exsql <- dplyr::build_sql(sql(explain_txt), dplyr::sql_render(x))
    expl_raw <- RPostgreSQL::dbGetQuery(x$src$con, exsql)
    expl <- paste(expl_raw[[1]], collapse = "\n")

    message("<PLAN>\n", expl)
    invisible(x)
}


winsorize <- function(df, vars_to_winsorize, quantiles = c(1, 99)) {
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
    if ('tbl_sql' %in% class(.data)) {
        # This is the case at the time of writing
        # Note, I'm testing with tbl_sql rather than tbl_sql because I'm about to
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
    if ('tbl_sql' %in% class(.data)) {
        # Note, I'm testing with tbl_sql because tbl_postgres is no longer a class in
        # dplyr > 0.5.0. However, I *am* about to use some postgres-specific syntax.
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
    out <- left.join(.data, dates_tbl, by = 'sale_date', copy = TRUE)
    return(out)
}


is_varname_in <- function(.data, varname) {
    # Pull one row to see for remote tables.
    stopifnot(length(varname) >= 1)
    one_row <- head(.data, 1) %>% collect()
    out <- all(varname %in% names(one_row))
    return(out)
}


find_match_states_crude_unmemoized <- function(n_auction_states = 3, n_buy_states = 3) {
    # Find what auction states Alaskan buyers buy in.
    # Then look at what other states buy in those states.
    # This is memoized in r_defaults.r

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
        semi.join(top_alaska_auction_states, by = 'auction_state') %>%
        arrange(-buy_pct) %>%
        collect() %>%
        # Now I'm relying on row order not changing:
        distinct(buy_state) %>%
        head(n_buy_states) %$%
        buy_state
    return(top_buyer_states)
}


my_felm <- function(formula, data, ..., dates_as_factor = TRUE, strict = FALSE) {
    force(formula)
    force(data)

    if (strict) {
        # Just like normal felm, but stricter about warnings.
        # (these are almost always a serious problem and should be treated as errors)
        orig_warn <- getOption('warn')
        on.exit(options(warn = orig_warn), add = TRUE)
        options(warn = 2)
    }
    if (dates_as_factor) {
        # Force date variables to be factor if they're integer/date
        date_vars <- c('sale_date', 'sale_week', 'event_time', 'event_week')
        data <- make_factor(data, date_vars, strict = FALSE)
    }
    return(lfe::felm(formula = formula, data = data, ...))
}


make_snippet <- function(x, filename, lazy = TRUE, ...) {
    # Write a number to a file.
    # (Only works for one number. Write a loop or something if you want multiple.)
    # Works quickly for remote dplyr tables because they're not evaluated unless required
    snippets_dir <- '../Text/Generated_snippets'

    base_filename <- basename(filename)
    if (filename == base_filename) {
        stopifnot(dir.exists(snippets_dir))
        outfile <- file.path(snippets_dir, base_filename)
    } else {
        stopifnot(dir.exists(dirname(filename)))
        outfile <- filename
    }

    if ((! lazy) || (! file.exists(outfile))) {
        if ('tbl' %in% class(x)) {
            x <- collect(x, n = Inf)
            stopifnot(all(dim(x) == c(1,1)))
        } else {
            stopifnot(length(x) == 1)
        }
        x <- unlist(x)
        x <- format_numbers(x, ...)
        write(x, file = outfile)
    }
}


format_numbers <- function(x, dollars = FALSE, sig_figs = NULL) {
    stopifnot(is.numeric(x), length(x) == 1)
    thin_space <- "\\\\hspace{0.1em}"  # have to double-escape thin_space
    if (is.null(sig_figs)) {
        if (is.integer(x) || x == as.integer(x)) {
            sig_figs <- 99L
        } else {
            sig_figs <- 4L
        }
    }
    stopifnot(length(sig_figs) == 1, sig_figs >= 1)
    x <- signif(x, sig_figs)  # round to 4 significant digits, if non-integer
    if (x < 0) {
        x <- abs(x)
        neg_str <- '\\ensuremath{-}'
    } else {
        neg_str <- ''
    }
    if (dollars) {
        dollar_str <- '\\$'
    } else {
        dollar_str <- ''
    }
    if (abs(x) >= 10000) {
        # Don't add commas for 1000, but do add commas for 10,000
        big.mark <- ','
    } else {
        big.mark <- thin_space
    }

    out <- prettyNum(x,
                     big.mark = big.mark, big.interval = 3,
                     small.mark = thin_space, small.interval = 3,
                     drop0trailing = TRUE)
    out <- paste0(neg_str, dollar_str, out)
    return(out)
}


is_panel_balanced <- function(.tbl, id_vars) {
    stopifnot(all(is_pkg_installed(c('dplyr', 'purrr'))))
    stopifnot(length(id_vars) >= 1, is.character(id_vars))

    count_unique_vals <- function(one_var, df) {
        df %>%
        ungroup() %>%
        select_(.dots = one_var) %>%
        distinct_(.dots = one_var) %>%
        force_nrow() %>%
        return()
    }
    if ('data.frame' %in% class(.tbl)) {
        if (is_id_(.tbl, id_vars) == FALSE) {
            return(FALSE)
        }
    }
    actual_nrow <- force_nrow(.tbl)
    stopifnot(actual_nrow > 0)
    unique_vals <- purrr::map_int(id_vars, count_unique_vals, df = .tbl)
    expected_nrow <- prod(unique_vals)

    return(actual_nrow == expected_nrow)
}


ensure_balanced_panel <- function(df, id_vars) {
    stopifnot(is_panel_balanced(df, id_vars))
    return(df)
}


cross_d <- function(.l, .filter = NULL) {
    # Define a version of purrr::cross_d that works with dates.
    # See, e.g., https://github.com/hadley/purrr/issues/251
    stopifnot(all(is_pkg_installed(c('dplyr', 'purrr'))))

    results <- purrr::cross_d(.l = .l, .filter = .filter)
    # Fix dates if necessary:
    # First figure out which columns are dates
    is_date <- function(x) {'Date' %in% class(x)}
    dates_col_idx <- which(purrr::map_lgl(.l, is_date))
    # Then if there are dates, fix them
    if (length(dates_col_idx) > 0) {
        # Make a partially completed function to apply in the mutate_at
        # (easier to reason about than passing origin into mutate_at)
        as_date <- purrr::partial(as.Date.numeric, origin = '1970-01-01')
        results <- dplyr::mutate_at(results, dates_col_idx, as_date)
    }
    return(results)
}


force_panel_balance <- function(.tbl, id_vars, fill_na = FALSE) {
    stopifnot(all(is_pkg_installed(c('dplyr', 'purrr', 'magrittr'))))
    # First, check that this is necesssary at all:
    if (is_panel_balanced(.tbl = .tbl, id_vars = id_vars)) {
        return(.tbl)
    }
    get_unique_vector <- function(one_var, df) {
        df %>% ungroup() %>%
        select_(.dots = one_var) %>%
        distinct_(.dots = one_var) %>%
        collect(n = Inf) %>%
        magrittr::extract2(1) %>%
        #unlist(recursive = FALSE, use.names = FALSE) %>%
        return()
    }
    # Get a list of the unique values in each id_var, then use cross_d to take the cross
    # product of all the unique values.
    # One could probably get the same effect by using joins without by variables to take
    # cartesian products, but that's my least favorite SQL behavior, so I'm avoiding it.
    full_df <- lapply(id_vars, get_unique_vector, df = .tbl) %>%
        setNames(id_vars) %>%
        cross_d()

    # right_join and copy = TRUE to copy the full_df we just created to wherever the
    # original .tbl happens to be (in memory or in some database)
    complete_tbl <- dplyr::right_join(.tbl, full_df, by = id_vars, copy = TRUE)

    if (fill_na) {
        complete_tbl <- fill_tbl(complete_tbl, replace_what = NA)
    }
    return(complete_tbl)
}


fill_tbl <- function(.tbl, replace_what = NA) {
    # Fill a table with zeros, or whatever else.
    stopifnot(length(replace_what) == 1)
    head1 <- head(.tbl, 1) %>% dplyr::collect()
    tbl_names <- names(head1)

    get_default_value <- function(x) {
        cls <- class(x)
        if (any(c('numeric', 'integer', 'character', 'logical') %in% cls)) {
            # use the trick that numeric(1) is 0, integer(1) is 0L etc.
            default_val <- match.fun(class(x)[1])(1)
        } else if ('Date' %in% cls) {
            default_val <- as.Date('1970-01-01')
        } else {
            stop(sprintf('Default value for class %s not written yet.', vec2string(cls)))
        }
        return(default_val)
    }

    if_else_once <- function(var, replacement, replace_what) {
        # If this is going into a database, if_else and ifelse are identical.
        # If it's a local data.frame, I'm going to rely on the auto-coersion of ifelse.
        stopifnot(length(replacement) == 1, length(var) == 1)
        if (is.na(replace_what)) {
            mutate_call <- lazyeval::interp(~ ifelse(is.na(var), replacement, var),
                var = as.name(var))
        } else {
            mutate_call <- lazyeval::interp(~ ifelse(var == val, replacement, var),
                var = as.name(var), val = replace_what, replacement = replacement)
        }
        return(mutate_call)
    }
    default_vals <- lapply(head1, get_default_value)  # get a named list of defaults
    mutate_calls <- purrr::map2(tbl_names, default_vals, if_else_once,
                                replace_what = replace_what)

    return(dplyr::mutate_(.tbl, .dots = setNames(mutate_calls, tbl_names)))
}


print_pipe <- function(x) {
    # This is for debugging, easier than stepping through the function.
    print(x)
    return(x)
}


make_factor <- function(.tbl, varnames, strict = TRUE) {
    # If varname is in the table, make it into a factor. Otherwise, don't bother.
    stopifnot('data.frame' %in% class(.tbl), length(varnames) >= 1,
              is.character(varnames), is.logical(strict))

    varnames_in_tbl <- intersect(varnames, names(.tbl))
    if (strict && length(varnames_in_tbl) < length(varnames)) {
        missing_vars <- vec2string(setdiff(varnames, varnames_in_tbl))
        err_msg <- sprintf("Mising variables: %s", missing_vars)
        stop(err_msg)
    }
    if (length(varnames_in_tbl) == 0) {
        return(.tbl)
    }
    out <- .tbl %>% mutate_at(vars(one_of(varnames_in_tbl)), as.factor)
    return(out)
}


get_sales_efficiency <- function(df_base, date_var = 'sale_date', id_var = 'buyer_id') {
    # This is a parallel to get_sales_counts, but for fuel efficiency.
    # The function signature is the same, but it returns fuel_cons.
    mpg_to_L100km_coef <- (100 * 3.785411784 / 1.609344)  # 3.7 L/gal, 1.6 km/mi

    stopifnot(length(date_var) == 1, length(id_var) == 1,
              date_var %in% c('sale_date', 'sale_week', 'event_time', 'event_week'),
              # other IDs are possible, but haven't been written yet:
              id_var %in% c('buyer_id', 'buy_state', 'sell_state', 'auction_state'))

    group_vars <- c(id_var, date_var)
    if (date_var == 'event_week') {
        # Add sale_year because event_week is just a week integer, something like -10,
        # and I don't want to total over year.
        df_base <- df_base %>% add_sale_year()
        group_vars <- c(group_vars, 'sale_year')
    } else if (date_var == 'event_time') {
        # Add sale_date because (1) we need to differentiate years, same as event_week,
        # and (2) it's convenient to have sale_date included in the output variables,
        # without actually changing the level of aggregation. (Put differently,
        # event_time and sale_year identify days exactly as well as sale_date.)
        group_vars <- c(group_vars, 'sale_year', 'sale_date')
    }
    # Use ln here rather than log. ln isn't defined in dplyr, so is passed to postgres.
    # The downside of using log() in dplyr is that it calls the two-argument form of
    # log, which doesn't work for floating points in postgres.
    # See: https://github.com/hadley/dplyr/issues/2464
    # Use ln() instead, which gets passed through.
    # https://www.postgresql.org/docs/current/static/functions-math.html#FUNCTIONS-MATH-FUNC-TABLE
    sales_gpm <- df_base %>%
        select_(.dots = c(group_vars, 'vin_pattern')) %>%
        inner.join(vin_decoder, by = 'vin_pattern') %>%
        group_by_(.dots = group_vars) %>%
        summarize(sale_count = n(),
                  fuel_cons = mean(mpg_to_L100km_coef / combined),
                  fuel_cons_log = mean(ln(mpg_to_L100km_coef / combined))) %>%
        ungroup() %>%
        mutate(sale_count_log = ln(sale_count)) %>%
        collapse()

    if (id_var == 'buyer_id') {  # Merge back in buy_state.
        # We've previously ensured that each buyer_id has at most one state.

        # dplyr bug (#2290) means this doesn't work:
        # sales_counts <- df_base %>% group_by(sale_date, buyer_id) %>%
        #     summarize(sale_count = n(),
        #               sale_tot = sum(sales_pr),
        #               buy_state = first(buy_state),
        #               alaskan_buyer = first(alaskan_buyer),
        #               post_dividend = first(post_dividend)) %>%
        #     collect()
        # Instead, do the aggregation, then join buy_state back in.
        #
        # (the collapse() here tells dplyr to think hard about the sql query (but not
        # actually go and process in the database), preventing it from getting confused in
        # the merge any trying to rename sale_date to sale_date.y.)
        buyer_info <- df_base %>% ungroup() %>%
            distinct(buyer_id, buy_state) %>% collapse()
        sales_gpm <- inner.join(sales_gpm, buyer_info, by = 'buyer_id') %>%
            collapse()
    } else if (id_var == 'seller_id') {
        stop("Not implemented yet.")
    }
    return(sales_gpm)
}


get_sales_counts <- function(df_base, date_var = 'sale_date', id_var = 'buyer_id',
    summarize_vars = NULL) {
    # Aggregate sale counts and sales_pr at some level of date and ID.
    stopifnot(length(date_var) == 1, length(id_var) == 1,
              date_var %in% c('sale_date', 'sale_week', 'event_time', 'event_week'),
              # other IDs are possible, but haven't been written yet:
              id_var %in% c('buyer_id', 'buy_state', 'sell_state', 'auction_state'))

    group_vars <- c(id_var, date_var)
    if (date_var == 'event_week') {
        # Add sale_year because event_week is just a week integer, something like -10,
        # and I don't want to total over year.
        df_base <- df_base %>% add_sale_year()
        group_vars <- c(group_vars, 'sale_year')
    } else if (date_var == 'event_time') {
        # Add sale_date because (1) we need to differentiate years, same as event_week,
        # and (2) it's convenient to have sale_date included in the output variables,
        # without actually changing the level of aggregation. (Put differently,
        # event_time and sale_year identify days exactly as well as sale_date.)
        group_vars <- c(group_vars, 'sale_year', 'sale_date')
    }
    summarize_vars_formulae <- c(
        'sales_pr_mean_log' = 'mean(ln(sales_pr))',
        'sales_pr_mean' = 'mean(sales_pr)',
        'msrp_mean' = 'mean(msrp)',
        'msrp_mean_log' = 'mean(ln(msrp))')
    if (is.null(summarize_vars)) {
        summarize_vars <- names(summarize_vars_formulae)
    }
    # Always do these two, no mater what summarize_vars is.
    summarize_calls <- c('sale_count' = 'n()', 'sale_tot' = 'sum(sales_pr)',
        summarize_vars_formulae[summarize_vars])
    summarize_calls <- summarize_calls[! is.na(summarize_calls)]
    # Use ln here rather than log. ln isn't defined in dplyr, so is passed to postgres.
    # The downside of using log() in dplyr is that it calls the two-argument form of
    # log, which doesn't work for floating points in postgres.
    # See: https://github.com/hadley/dplyr/issues/2464
    # Use ln() instead, which gets passed through.
    # https://www.postgresql.org/docs/current/static/functions-math.html#FUNCTIONS-MATH-FUNC-TABLE
    sales_counts <- df_base %>% group_by_(.dots = group_vars) %>%
        summarize_(.dots = as.list(summarize_calls)) %>%
        # summarize(sale_count = n(), sale_tot = sum(sales_pr),
        #           sales_pr_mean_log = mean(ln(sales_pr)),
        #           sales_pr_mean = mean(sales_pr),
        #           msrp_mean = mean(msrp),
        #           msrp_mean_log = mean(ln(msrp))) %>%
        ungroup() %>%
        mutate(sale_count_log = ln(sale_count), sale_tot_log = ln(sale_tot)) %>%
        collapse()

    if (id_var == 'buyer_id') {  # Merge back in buy_state.
        # We've previously ensured that each buyer_id has at most one state.

        # dplyr bug (#2290) means this doesn't work:
        # sales_counts <- df_base %>% group_by(sale_date, buyer_id) %>%
        #     summarize(sale_count = n(),
        #               sale_tot = sum(sales_pr),
        #               buy_state = first(buy_state),
        #               alaskan_buyer = first(alaskan_buyer),
        #               post_dividend = first(post_dividend)) %>%
        #     collect()
        # Instead, do the aggregation, then join buy_state back in.
        #
        # (the collapse() here tells dplyr to think hard about the sql query (but not
        # actually go and process in the database), preventing it from getting confused in
        # the merge any trying to rename sale_date to sale_date.y.)
        buyer_info <- df_base %>% ungroup() %>%
            distinct(buyer_id, buy_state) %>% collapse()
        sales_counts <- inner.join(sales_counts, buyer_info, by = 'buyer_id') %>%
            collapse()
    } else if (id_var == 'seller_id') {
        stop("Not implemented yet.")
    }
    return(sales_counts)
}


aggregate_sales_dd_unmemoized <- function(years, agg_var, days_before = 70,
        days_after = days_before, aggregate_fn = get_sales_counts) {
    # Create the dataset for the difference-in-differences analysis.
    # This is split out in a separate function before creating the ancitipation variables
    # so it can be cached and we quickly test different values of the anticipation
    # window.
    # This is memoized in r_defaults.r into aggregate_sales_dd

    stopifnot(is.numeric(years), length(years) >= 1,
              is.character(agg_var), length(agg_var) == 1)
    aggregate_fn <- match.fun(aggregate_fn)
    control_states <- find_match_states_crude()
    df <- auctions %>%
    # TODO: could make this faster by selecting vars depending on aggregate_fn
        select(sale_date, buy_state, sales_pr, vin_pattern, msrp) %>%
        filter_event_window(years = years, days_before = days_before,
                            days_after = days_after) %>%
        filter(buy_state %in% c('AK', control_states)) %>%
        add_event_time() %>%
        aggregate_fn(date_var = agg_var, id_var = 'buy_state') %>%
        tag_alaskan_buyer() %>%  # Make TRUE/FALSE alaskan_buyer variable
        collect(n = Inf)
    return(df)
}


run_dd <- function(years = 2002:2014,
    aggregation_level = 'daily',
    # How many days/weeks relative to the event day do we think dealers are anticipating?
    # NB: if aggregation_level is daily, anticipation_window is in days. If
    # aggregation_level is weekly, anticipation_window is in weeks. Zero is the day/week
    # when the dividend is sent (so -1 is the last period before the dividend).
    anticipation_window = NULL,
    outcomes = c('sale_count', 'sale_tot', 'sales_pr_mean'),
    # controls, including DD vars, excluding fixed effects
    controls = c('alaskan_buyer', 'anticipation', 'alaskan_buyer_anticipation', 'post',
                 'alaskan_buyer_post'),
    # Which fixed effects should we have?
    fixed_effects = c('sale_year', 'buy_state'),
    # what clusters should we have?
    # (remember that cluster covariance relies on asymptotics!)
    clusters = 0,
    days_before_limit = 70,
    days_after_limit = days_before_limit
    ) {

    stopifnot(aggregation_level %in% c('daily', 'weekly'),
              length(outcomes) >= 1,
            #   length(controls) == 1,
            #   length(fixed_effects) == 1,
            #   length(clusters) == 1,
              length(years) >= 1
              )
    if (is.null(anticipation_window)) {
        if (aggregation_level == 'daily') {
            anticipation_window <- c(-14, -1)
        } else if (aggregation_level == 'weekly') {
            anticipation_window <- c(-2, -1)
        }
    }
    if (length(anticipation_window) != 2 ||
        anticipation_window[1] > anticipation_window[2] ||
        (-days_before_limit) > anticipation_window[1] ||
        days_after_limit < anticipation_window[2] ) {
        err_msg <- sprintf('Bad anticipation_window: %s', vec2string(anticipation_window))
        stop(err_msg)
    }

    pull_dd_data <- function() {
        # This is a sub-function for organizational ease, but it relies heavily on
        # variables defined in the parent function. Be careful before moving it.

        # look up varname (will error if aggregation_level is not in there)
        agg_var <- c(daily = 'event_time', weekly = 'event_week')[[aggregation_level]]
        agg_fn <- outcome_to_agg_fn(outcomes)
        # grouping_vars <- c('sale_year', 'buy_state', agg_var)

        # Set up mutate_ calls.
        # Create two variables, anticipation_period and post_period.  anticipation_period
        # will be true for rows where agg_var (event_time or event_week) is between
        # within anticipation_window, and post_period is true when it's greater.
        mutate_calls_time <- list(
            anticipation = lazyeval::interp(~ between(agg_var, low, high),
                agg_var = as.name(agg_var), low = anticipation_window[1],
                high = anticipation_window[2]),
            # NB: this is post-anticipation, not post-dividend
            post = lazyeval::interp(~ agg_var > high,
                agg_var = as.name(agg_var), high = anticipation_window[2])
            )

        aggregate_sales_dd(years, agg_var, aggregate_fn = agg_fn,
            days_before = days_before_limit, days_after = days_after_limit) %>%
        force_panel_balance(c('sale_year', agg_var, 'buy_state'), fill_na = TRUE) %>%
            # Make these variables even if they're not used (it's fast)
            mutate_(.dots = mutate_calls_time) %>% # Make anticipation and post
            mutate(alaskan_buyer_anticipation = alaskan_buyer & anticipation,
                   alaskan_buyer_post = alaskan_buyer & post) %>%
            collect(n = Inf) %>%
            return()
    }

    # Avoid collinearity
    if ('buy_state' %in% fixed_effects) {
        controls <- setdiff(controls, 'alaskan_buyer')
    }
    df <- pull_dd_data()  # finally collect the data

    # Make the felm multi-part formula, something like
    # sale_tot | sale_count ~ anticipation + alaskan_buyer_anticipation + post +
    #     alaskan_buyer_post | buy_state + sale_year | 0 | buy_state
    # Using paste like this reduces some flexibility for interactions, but I can just
    # create those variables manually.
    reg_formula <- paste(paste(outcomes, collapse = ' | '), '~',
                         paste(controls, collapse = ' + '), '|',
                         paste(fixed_effects, collapse = ' + '),
                         '| 0 |',  # no IV vars
                         paste(clusters, collapse = ' + ')) %>% as.formula()
    reg_results <- tryCatch({
        my_felm(reg_formula, df)
    }, warning = function(warning_condition) {
        err_msg <- sprintf("felm issue with anticipation_window [%s, %s]",
                           anticipation_window[1], anticipation_window[2])
        warning(warning_condition)
        stop(err_msg)
    }, error = function(error_condition) {
        stop(error_condition)
    }, finally = {

    })

    return(reg_results)
}

outcome_to_agg_fn <- function(outcomes) {
    stopifnot(is.character(outcomes), outcomes >= 1)
    # TODO: a better way to do this would be have one function that handles all vars.
    if (! all(outcomes %in% names(OUTCOME_VARS))) {
        err_msg <- paste('Please update the list of outcome variables in OUTCOME_VARS,',
            'get_sales_counts_vars or get_sales_efficiency_vars to include',
            vec2string(setdiff(outcomes, names(OUTCOME_VARS))))
        stop(err_msg)
    }

    if (all(outcomes %in% GET_SALES_COUNTS_VARS)) {
        agg_fn <- get_sales_counts
    } else if (all(outcomes %in% GET_SALES_EFFICIENCY_VARS)) {
        agg_fn <- get_sales_efficiency
    } else {
        err_msg <- sprintf("Sorry, can't handle a mix of variables: %s",
                           vec2string(outcomes))
        stop(err_msg)
    }
    return(agg_fn)
}

vec2string <- function(x, quoted = TRUE, add_and = TRUE) {
    # Like the toString function, but better
    stopifnot(length(x) > 0, is.logical(quoted), is.logical(add_and))
    if (quoted) {
        collapse_pattern <- "', '"
        sprintf_pattern <- "'%s'"
    } else {
        collapse_pattern <- ", "
        sprintf_pattern <- "%s"
    }
    end <- length(x)
    if (end == 1) {
        return(sprintf(sprintf_pattern, x))
    }
    first_elements <- x[-end]
    last_element <- x[[end]]
    if (add_and && end > 2) {
        last_element <- paste(' and', sprintf(sprintf_pattern, last_element))
    } else {
        last_element <- paste(',', sprintf(sprintf_pattern, last_element))
    }

    first_elements <- sprintf(sprintf_pattern, paste(first_elements,
                                                     collapse = collapse_pattern))
    out <- paste0(first_elements, last_element)

    return(out)
}


plot_effects_individual_period <- function(
        outcome,
        aggregation_level = 'daily',
        years = 2002:2014,
        controls = "alaskan_buyer",
        fixed_effects = 'sale_year',
        clusters = 0,
        days_before_limit = 70,
        days_after_limit = days_before_limit) {

    stopifnot(length(outcome) == 1)  # makes parsing output *much* easier
    x_vars <- c(controls, fixed_effects)
    agg_var <- c('daily' = 'event_time', 'weekly' = 'event_week')[[aggregation_level]]
    aggregation_level_noun <- c('daily' = 'day', 'weekly' = 'week')[[aggregation_level]]
    agg_fn <- outcome_to_agg_fn(outcome)
    aggregated_sales <- aggregate_sales_dd(years, agg_var, aggregate_fn = agg_fn,
        days_before = days_before_limit, days_after = days_after_limit) %>%
        force_panel_balance(c('sale_year', agg_var, 'buy_state'), fill_na = TRUE)

    if ('sale_dow' %in% x_vars) {
        aggregated_sales <- add_sale_dow(aggregated_sales)
    } else if (any(grepl('sale_dow', x_vars, fixed = TRUE))) {
        stop("Something weird with sale_dow here.")
    }

    if ('alaskan_buyer' %in% x_vars) {
        aggregated_sales <- aggregated_sales %>% tag_alaskan_buyer(as_factor = FALSE)
        if ('buy_state' %in% x_vars) {
            stop("Can't have alaskan_buyer and buy_state because of collinearity")
        }
    }

    reg_formula <- paste(outcome, '~',
                         paste(controls, collapse = ' + '), '|',
                         paste(fixed_effects, collapse = ' + '),
                         '| 0 |',  # no IV vars
                         paste(clusters, collapse = ' + ')) %>% as.formula()
    get_results_one_period <- function(agg_var_value) {
        aggregated_sales_one_period <- aggregated_sales %>%
            filter_(.dots = lazyeval::interp(~var == val, var = as.name(agg_var),
                                             val = agg_var_value))

        reg_results <- tryCatch({
            reg_results <- my_felm(reg_formula, data = aggregated_sales_one_period)
        }, warning = function(warning_condition) {
            warning(sprintf('Problem with %s %ss', agg_var_value, aggregation_level_noun))
            stop(warning_condition)
        }, error = function(error_condition) {
            warning(sprintf('Problem with %s %ss', agg_var_value, aggregation_level_noun))
            stop(error_condition)
        }, finally = {

        })
        # reg_results <- my_felm(reg_formula, data = aggregated_sales_one_period)
        # TODO: use broom::tidy here instead.
        df <- data_frame(event_period = agg_var_value,
            coef = reg_results$coefficients['alaskan_buyerTRUE', ],
            se   = reg_results$rse[['alaskan_buyerTRUE']],
            pval = reg_results$rpval[['alaskan_buyerTRUE']])

        return(df)
    }

    # Now also grab the std dev of the sample we're looking at.
    sd_varname <- paste0(outcome, '_sd')  # either sale_tot_sd or sale_count_sd
    mean_varname <- paste0(outcome, '_mean')
    data_sd <- get_state_by_time_variation(aggregation_level = aggregation_level,
        vars_to_summarize = outcome, summary_fn = 'sd')[[sd_varname]]
    data_mean <- get_state_by_time_variation(aggregation_level = aggregation_level,
        vars_to_summarize = outcome, summary_fn = 'mean')[[mean_varname]]

    # Pull the unique values of agg_var into a vector, sort it, and pass the values to
    # get_results_one_period. Then collect the output dataframe into one, then use the
    # standard deviation we just calculated to get effect sizes.
    event_period_list <- aggregated_sales %>%
        distinct_(agg_var) %>%
        extract2(1) %>% sort()
    # The last week for MSRP is ridiculous, and makes it hard to see what's going on with the
    # rest of the weeks.
    if (outcome %in% c('msrp_mean', 'msrp_mean_log', 'sales_pr_mean', 'sales_pr_mean_log') &&
        days_after_limit == 70 && aggregation_level == 'weekly') {
        event_period_list <- event_period_list[-length(event_period_list)]
    }
    to_plot <- purrr::map_df(event_period_list, get_results_one_period) %>%
        calculate_effect_sizes(data_sd = data_sd, data_mean = data_mean)
    lab_x <- sprintf('Event %s', aggregation_level_noun)
    lab_y <- OUTCOME_VARS[[outcome]]
    lab_y_effect <- sprintf("Effect size (std. dev. %s)", tolower(lab_y))
    lab_y_mean  <- sprintf("Effect size (fraction of mean %s)", tolower(lab_y))
    coef_plot <- ggplot(to_plot, aes(x = event_period, y = coef)) +
        geom_point() +
        geom_errorbar(aes(ymin = conf95_lower, ymax = conf95_upper)) +
        labs(x = lab_x, y = lab_y) +
        PLOT_THEME
    coef_effect_plot <- ggplot(to_plot, aes(x = event_period, y = coef_effect)) +
        geom_point() +
        geom_errorbar(aes(ymin = conf95_lower_effect, ymax = conf95_upper_effect)) +
        labs(x = lab_x, y = lab_y_effect) +
        PLOT_THEME
    coef_effect_mean_plot <- ggplot(to_plot, aes(x = event_period, y = coef_effect_mean,
            ymin = conf95_lower_effect_mean, ymax = conf95_upper_effect_mean)) +
        geom_point() +
        geom_errorbar() +
        labs(x = lab_x, y = lab_y_mean) +
        PLOT_THEME
    filename_part <- sprintf('effects_individual_period_%s_%s_',
                             outcome, aggregation_level)
    suffix <- '_notitle.pdf'
    save_plot(coef_plot, paste0(filename_part, 'coef', suffix))

    if (! endsWith(outcome, 'log')) {
        # Don't make effect plots for logged outcomes because that doesn't make much
        # sense.(Don't worry about ggplot making the plots above; it doesn't do any work
        # until you actually display the plot.)
        save_plot(coef_effect_plot,      paste0(filename_part, 'coef_effect', suffix))
        save_plot(coef_effect_mean_plot, paste0(filename_part, 'coef_effect_mean', suffix))
    }
    invisible(to_plot)  # then return the data
}


calculate_effect_sizes <- function(df, data_sd = NULL, data_mean = NULL) {
    stopifnot(length(data_sd) <= 1, length(data_mean) <= 1,
              length(data_sd) + length(data_mean) >= 1)
    out <- df %>%
        mutate(conf95_lower = coef - (1.96 * se),
               conf95_upper = coef + (1.96 * se),
               # pmax because I want rowwise max, not max of all.
               conf95_max_mag = pmax(abs(conf95_lower), abs(conf95_upper)),
               is_signif = factor(pval < 0.05, levels = c(FALSE, TRUE), ordered = TRUE,
                                  labels = c('p > 0.05', 'p < 0.05')))
    if (! is.null(data_sd)) {
        out <- out %>%
            mutate(coef_effect = coef / data_sd,
                   conf95_lower_effect = conf95_lower / data_sd,
                   conf95_upper_effect = conf95_upper / data_sd,
                   conf95_max_mag_effect = conf95_max_mag / data_sd)
    }
    if (! is.null(data_mean)) {
        out <- out %>%
            mutate(coef_effect_mean = coef / data_mean,
                   conf95_lower_effect_mean = conf95_lower / data_mean,
                   conf95_upper_effect_mean = conf95_upper / data_mean,
                   conf95_max_mag_effect_mean = conf95_max_mag / data_mean)
    }
    return(out)
}


get_state_by_time_variation_unmemoized <- function(aggregation_level = 'daily',
        winsorize_pct = NULL, states_included = NULL, all_year = TRUE,
        vars_to_summarize = c('sale_tot', 'sale_count', 'sales_pr_mean'),
        summary_fn = 'sd') {

    # Parameters:
    # aggregation_level: the aggregation level of the counts and dollar amounts.
    #   daily/weekly, defaults to daily.
    # winsorize_pct: the amount by which the aggregated variables (counts or $) should be
    #   winsorized before calculating the std dev. Defaults to no winsorization.
    # states_included: the states included in the standard deviation calculation.
    #   Defaults to Alaska and the control states from find_match_states_crude().
    #   Note that the outcomes are demeaned by state before calculating the std dev.
    # all_year: should we look at variation throughout the year, or only in the event
    #   period we're studying (something like 70 days before to 70 days after)
    #   Defaults to using the whole year.
    # vars_to_summarize: which variables are we getting the standard deviation for?
    # summary_fn: what summary to create (mean or sd) (must be character because that
    #   makes it vastly easier to translate to SQL)

    if (aggregation_level == 'daily') {
        time_var <- 'sale_date'
    } else if (aggregation_level == 'weekly') {
        time_var <- 'sale_week'
    } else {
        stop("Bad aggregation_level, should be daily or weekly.")
    }

    # The names aren't the most clear, but agg_fn takes the original auction data
    # and aggregates to daily/weekly totals or averages, then summary_fn takes those
    # totals and finds their mean / sd
    stopifnot(is.character(summary_fn), length(summary_fn) == 1,
              is.function(match.fun(summary_fn)))
    if (summary_fn == 'sd') {
        should_demean <- TRUE
    } else if (summary_fn == 'mean') {
        should_demean <- FALSE
    } else {
        stop(sprintf('Not sure how to work with this function: %s', summary_fn))
    }

    agg_fn <- outcome_to_agg_fn(vars_to_summarize)
    # Only one of sales_pr or vin_pattern will be relevant in agg_fn, but that's fine.
    df <- auctions %>% select(sale_date, buy_state, sales_pr, vin_pattern, msrp)

    if (is.null(states_included)) {
        # What states get included in the std dev calculations?
        control_states <- find_match_states_crude()
        states_included <- c('AK', control_states)
    }
    if (length(states_included) <= 1) {
        # Necessary because if states_included has length 1, we encounter dplyr bug #511.
        df <- filter(df, buy_state == states_included)
    } else {
        df <- filter(df, buy_state %in% states_included)
    }

    if (! all_year) {
        df <- filter_event_window(df)
    }

    if (time_var == 'sale_week') {
        # NB: This is not exactly the same as event weeks
        # (date_part is a postges function)
        df <- mutate(df, sale_week = date_part('week', sale_date))
    }
    df <- df %>% agg_fn(date_var = time_var, id_var = 'buy_state') %>%
        # Some state-days don't have trades; fill these with zeros.
        force_panel_balance(c(time_var, 'buy_state'), fill_na = TRUE)
    if (should_demean) {
        # Demean so we're not getting huge standard deviations by looking across states
        # with different means.
        df <- df %>% group_by(buy_state) %>%
            demean_(vars_to_summarize) %>%
            ungroup()
    }

    if (! is.null(winsorize_pct)) {
        df <- winsorize(df, vars_to_summarize, winsorize_pct)
    }

    # summarize like:
    # summarize(sale_tot_sd = sd(sale_tot), sale_count_sd = sd(sale_count),
    #    sales_pr_mean_sd = sd(sales_pr_mean))
    one_summary <- function(x) {
        lazyeval::interp(~fn(x), x = as.name(x), fn = as.name(summary_fn))
    }
    summarize_calls <- lapply(vars_to_summarize, one_summary) %>%
        # make the *_sd or *_mean variable names
        setNames(paste0(vars_to_summarize, '_', summary_fn))
    df %>% summarize_(.dots = summarize_calls) %>%
        collect() %>% return()
}


demean_ <- function(.tbl, vars) {
    # Demean the variables.  If .tbl is grouped, it will be a within-group demeaning.
    stopifnot(is.character(vars), length(vars) >= 0)
    mutate_once <- function(var) {lazyeval::interp(~ v - mean(v), v = as.name(var))}
    mutate_calls <- lapply(vars, mutate_once) %>% setNames(vars)
    return(mutate_(.tbl, .dots = mutate_calls))
}


demean <- function(.tbl, ...) {
    varnames <- dots_to_names(...)
    demean_(.tbl, varnames)
}


run_dd_pick_max_effect <- function(outcome,
        aggregation_level = 'weekly', days_before_limit = 70,
        days_after_limit = days_before_limit) {

    # This function is very similar to the previous one,
    # plot_effects_by_anticipation_variable_start_and_end, but instead of searching for
    # an ancitipation window before the dividend day and then having a post-dividend
    # coefficient, we're just going to estimate the one period and find the maximizing
    # one.
    if (aggregation_level == 'daily') {
        loop_start <- -days_before_limit + 1
        loop_end <- days_after_limit - 1
        min_window_length <- 7
    } else if (aggregation_level == 'weekly'){
        loop_start <- ((-days_before_limit) %/% 7) + 1
        loop_end <- (days_after_limit %/% 7) - 1
        min_window_length <- 1
    } else {
        stop("aggregation_level must be 'daily' or 'weekly'")
    }
    stopifnot(outcome %in% names(OUTCOME_VARS),
        days_before_limit > 2)
    max_window_length <- abs(loop_end) + abs(loop_start) - 2
    windows <- purrr::cross2(
        # Form the cross of all possible window starts
        seq.int(loop_start, loop_end, by = 1),
        # crossed by all possible window ends:
        seq.int(loop_start, loop_end, by = 1),
        .filter = function(start, end) {
            # and then filter combos we don't want
            (end - start < min_window_length) || (end - start > max_window_length)
        }
    )
    stopifnot(length(windows) > 0)
    get_results_one_window <- function(start_end_list) {
        start <- start_end_list[[1]]
        end <- start_end_list[[2]]
        reg_results <- run_dd(outcomes = outcome, aggregation_level = aggregation_level,
            anticipation_window = c(start, end), days_before_limit = days_before_limit,
            fixed_effects = c('sale_year', 'buy_state'),
            controls = c('anticipation', 'alaskan_buyer_anticipation', 'post',
                         'alaskan_buyer_post'))

        # rse is apparently the robust standard error, though it's not well documented.
        # e.g. identical(sqrt(diag(reg_results$robustvcv)), reg_results$rse)
        # TODO: use broom::tidy here instead.
        df <- data_frame(start = start, end = end,
            coef = reg_results$coefficients['alaskan_buyer_anticipationTRUE', ],
            se   = reg_results$rse[['alaskan_buyer_anticipationTRUE']],
            pval = reg_results$rpval[['alaskan_buyer_anticipationTRUE']])
        return(df)
    }

    # Now also grab the std dev and mean of the sample we're looking at.
    sd_varname <- paste0(outcome, '_sd')
    mean_varname <- paste0(outcome, '_mean')
    data_sd <- get_state_by_time_variation(aggregation_level = aggregation_level,
        vars_to_summarize = outcome, summary_fn = 'sd')[[sd_varname]]
    data_mean <- get_state_by_time_variation(aggregation_level = aggregation_level,
        vars_to_summarize = outcome, summary_fn = 'mean')[[mean_varname]]
    stopifnot(! is.null(data_sd), ! is.null(data_mean))

    to_plot <- purrr::map_df(windows, get_results_one_window)
    sale_tot_divisor <- 1000
    if (outcome == 'sale_tot') {
        to_plot <- to_plot %>% mutate(coef = coef / sale_tot_divisor,
                                      se   = se   / sale_tot_divisor)
        data_sd <- data_sd / sale_tot_divisor
        data_mean <- data_mean / sale_tot_divisor
    }
    to_plot <- calculate_effect_sizes(to_plot, data_sd, data_mean)
    # Define a bunch of labels.
    lab_x <- 'Window end (%s)'
    lab_y <- 'Window start (%s)'
    if (aggregation_level == 'daily') {
        lab_x <- sprintf(lab_x, 'days')
        lab_y <- sprintf(lab_y, 'days')
    } else if (aggregation_level == 'weekly') {
        lab_x <- sprintf(lab_x, 'weeks')
        lab_y <- sprintf(lab_y, 'weeks')
    } else {
        stop("Error!")
    }

    # if (outcome == 'sale_tot') {
    #     lab_color_coef = 'Estimated additional Alaskan anticipation cars sold'
    #     lab_color_se = 'SE on additional Alaskan anticipation cars sold'
    # } else if (outcome == 'sale_count') {
    #     lab_color_coef = 'Estimated additional Alaskan anticipation sale volume'
    #     lab_color_se = 'SE on additional Alaskan anticipation sale volume'
    # } else if (outcome == 'sales_pr_mean') {
    #     lab_color_coef = 'Estimated additional Alaskan anticipation average sale price'
    #     lab_color_se = 'SE on additional Alaskan anticipation average sale price'
    # } else {
    #     stop("Error!")
    # }
    sign_as_factor <- function(x) {
        # edge case: make zero positive:
        sign_x <- if_else(sign(x) != 0, sign(x), 1)
        factor(sign_x, levels = c(1, -1), labels = c('Positive', 'Negative'))
    }

    # common_plot <- ggplot(to_plot, aes(x = end, y = start)) +
    #     scale_fill_distiller(palette = 'YlGnBu', direction = 1) +
    #     labs(x = lab_x, y = lab_y, fill = '') +
    #     guides(alpha = 'none') +
    #     scale_alpha_discrete(range = c(0.5,1)) +
    #     PLOT_THEME

    common_plot <- ggplot(to_plot, aes(x = end, y = start, alpha = is_signif)) +
        scale_fill_distiller(palette = 'YlGnBu', direction = 1) +
        labs(x = lab_x, y = lab_y, fill = '', color = 'Sign', size = '',
             alpha = 'Significance') +
        # guides(alpha = 'none') +
        scale_alpha_discrete(range = c(0.25, 1)) +
        scale_size_area() +
        guides(size  = guide_legend(order = 1),
               color = guide_legend(order = 2),
               alpha = guide_legend(order = 3)) +
        PLOT_THEME
    # Available colors:
    # Diverging:
    # BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
    # Qualitative:
    # Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
    # Sequential:
    # Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples,
    # RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd

    # coef_plot <- ggplot(to_plot, aes(x = end, y = start, fill = coef)) +
    #     geom_tile() +
    #     scale_fill_distiller(palette = 'RdBu') +
    #     labs(x = lab_x, y = lab_y, fill = lab_color_coef) +
    #     PLOT_THEME
    # se_plot <- ggplot(to_plot, aes(x = end, y = start, fill = se)) +
    #     geom_tile() +
    #     scale_fill_distiller(palette = 'RdBu') +
    #     labs(x = lab_x, y = lab_y, fill = lab_color_se) +
    #     PLOT_THEME
    # TODO: the geom_point plot looks terrible for daily data. Maybe use
    # `geom_tile(aes(fill = coef, alpha = is_signif))` for daily and geom_point for weekly

    coef_plot <- common_plot +
        geom_point(aes(size = abs(coef), color = sign_as_factor(coef),
                   alpha = is_signif)) +
        labs(size = 'Coefficient\nmagnitude')
    # If the plot has only positive values, don't show a legend (but keep the color)
    coef_plot <- remove_color_if_pos(coef_plot, to_plot$coef)

    coef_effect_plot <- common_plot +
        geom_point(aes(size = abs(coef_effect), color = sign_as_factor(coef_effect))) +
        labs(size = 'Coefficient magnitude\n(frac. of std. dev.)')
    # If the plot has only positive values, don't show a legend (but keep the color)
    coef_effect_plot <- remove_color_if_pos(coef_effect_plot, to_plot$coef_effect)

    coef_effect_mean_plot <- common_plot +
        # geom_tile(aes(fill = coef, alpha = is_signif))
        geom_point(aes(size = abs(coef_effect_mean),
                       color = sign_as_factor(coef_effect_mean))) +
        labs(size = 'Coefficient magnitude\n(frac. of mean)')
    # If the plot has only positive values, don't show a legend (but keep the color)
    coef_effect_mean_plot <- remove_color_if_pos(coef_effect_mean_plot,
                                                 to_plot$coef_effect_mean)

    # These plots are fine, but no longer necessary:
    # se_plot <- common_plot + geom_point(aes(size = se)) + labs(size = 'Standard\nError')
    # conf95_lower_plot <- common_plot +
    #     # geom_tile(aes(fill = conf95_lower, alpha = is_signif))
    #     geom_point(aes(size = abs(conf95_lower), color = sign_as_factor(conf95_lower),
    #                    alpha = is_signif)) +
    #     labs(size = '95% CI\nlower bound', color = 'Lower bound sign')
    # # If the plot has only positive values, don't show a legend (but keep the color)
    # if (all(sign(to_plot$conf95_lower) == 1)){
    #     conf95_lower_plot <- conf95_lower_plot + guides(color = 'none')
    # }
    # conf95_upper_plot <- common_plot +
    #     # geom_tile(aes(fill = conf95_upper, alpha = is_signif))
    #     geom_point(aes(size = abs(conf95_upper), color = sign_as_factor(conf95_upper),
    #                    alpha = is_signif)) +
    #     labs(size = '95% CI\nupper bound',  color = 'Upper bound sign')
    # # If the plot has only positive values, don't show a legend (but keep the color)
    # if (all(sign(to_plot$conf95_upper) == 1)){
    #     conf95_upper_plot <- conf95_upper_plot + guides(color = 'none')
    # }


    # In these three conf95 plots, we color by the sign of the coefficient, since we still
    # care whether the effect is positive or negative.
    # Note that the symmetry of CIs is helpful here -- iff coef is negative, the maximum
    # magnitude CI bound will be the lower bound.
    conf95_max_mag_plot <- common_plot +
        geom_point(aes(size = conf95_max_mag, color = sign_as_factor(coef))) +
        labs(size = 'Max 95%-CI\nmagnitude')
    # If the plot has only positive values, don't show a legend (but keep the color)
    conf95_max_mag_plot <- remove_color_if_pos(conf95_max_mag_plot, to_plot$coef)

    # Need a slightly taller margin for these plots with three-line legend labels
    special_margin <- theme(plot.margin = margin(t = 0.15, r = 0.2, b = 0.1,
                                                 l = 0.1, unit = "cm"))
    conf95_max_mag_effect_plot <- common_plot +
        geom_point(aes(size = conf95_max_mag_effect, color = sign_as_factor(coef))) +
        labs(size = 'Max 95%-CI\nmagnitude\n(frac. of std. dev.)') + special_margin
    conf95_max_mag_effect_plot <- remove_color_if_pos(conf95_max_mag_effect_plot,
                                                      to_plot$coef)
    conf95_max_mag_effect_mean_plot <- common_plot +
        geom_point(aes(size = conf95_max_mag_effect_mean, color = sign_as_factor(coef))) +
        labs(size = 'Max 95%-CI\nmagnitude\n(frac. of mean)') + special_margin
    conf95_max_mag_effect_mean_plot <- remove_color_if_pos(
        conf95_max_mag_effect_mean_plot, to_plot$coef)

    filename_base <- sprintf('variable_window_dd_%s_%s_area_', outcome, aggregation_level)

    save_plot(coef_plot,           paste0(filename_base, 'coef.pdf'))
    save_plot(conf95_max_mag_plot, paste0(filename_base, 'conf95_max.pdf'))
    # save_plot(se_plot,             paste0(filename_base, 'se.pdf'))
    # save_plot(conf95_lower_plot,   paste0(filename_base, 'conf95_lower.pdf'))
    # save_plot(conf95_upper_plot,   paste0(filename_base, 'conf95_upper.pdf'))

    if (! endsWith(outcome, 'log')) {
        # Don't make effect plots for logged outcomes because that doesn't make much
        # sense.(Don't worry about ggplot making the plots above; it doesn't do any work
        # until you actually display the plot.)
        save_plot(coef_effect_plot, paste0(filename_base, 'coef_effect.pdf'))
        save_plot(coef_effect_mean_plot, paste0(filename_base, 'coef_effect_mean.pdf'))
        save_plot(conf95_max_mag_effect_plot,
                  paste0(filename_base, 'conf95_max_effect.pdf'))
        save_plot(conf95_max_mag_effect_mean_plot,
                  paste0(filename_base, 'conf95_max_effect_mean.pdf'))
    }

    invisible(to_plot)
}


render_title <- function(title, default = '') {
    stopifnot(length(title) <= 1, length(default) == 1)
    if (is.null(title) || isTRUE(title)){
        out <- default
    } else if (is.character(title) && title != '') {
        out <- title
    } else if (identical(title, FALSE) || tile == '') {
        out <- ''
    } else {
        stop(sprintf("Can't process title: '%s'", title))
    }
    return(out)
}


remove_color_if_pos <- function(plt, x) {
    # Remove the color legend if x is always positive.
    # (Not the same as removing color because the graph will still be colorful)
    # There's a better way to do this by rendering the plot and figuring out the color
    # variable, but it's not worth it.
    if (all(sign(x) == 1)) {
        plt <- plt + guides(color = 'none')
    }
    return(plt)
}


plot_effects_by_anticipation <- function(outcome,
        aggregation_level = 'daily', days_before_limit = 70, title = NULL) {

    if (aggregation_level == 'daily') {
        loop_start <- (-days_before_limit) + 1
        min_window_length <- 7
    } else if (aggregation_level == 'weekly'){
        loop_start <- ((-days_before_limit) %/% 7) + 1
        min_window_length <- 1
    } else {
        stop("aggregation_level must be 'daily' or 'weekly'")
    }
    stopifnot(outcome %in% names(OUTCOME_VARS),
              days_before_limit > 2, loop_start < min_window_length)


    get_results_one_window <- function(start) {
        reg_results <- run_dd(outcomes = outcome, aggregation_level = aggregation_level,
            anticipation_window = c(start, -1), days_before_limit = days_before_limit)

        # rse is apparently the robust standard error, though it's not well documented.
        # e.g. identical(sqrt(diag(reg_results$robustvcv)), reg_results$rse)
        # TODO: use broom::tidy here instead.
        df <- data_frame(start = start,
            coef = reg_results$coefficients['alaskan_buyer_anticipationTRUE', ],
            se   = reg_results$rse[['alaskan_buyer_anticipationTRUE']],
            pval = reg_results$rpval[['alaskan_buyer_anticipationTRUE']])
        return(df)

    }
    windows <- seq.int(loop_start, -min_window_length, by = 1)
    to_plot <- purrr::map_df(windows, get_results_one_window)
    # For weekly, force ggplot to label all weeks, rather than having ridiculous
    # half-weeks.
    if (aggregation_level == 'weekly') {
        to_plot <- to_plot %>% mutate(start = factor(start))
    }

    # Now also grab the std dev and mean of the sample we're looking at.
    sd_varname <- paste0(outcome, '_sd')
    mean_varname <- paste0(outcome, '_mean')
    data_sd <- get_state_by_time_variation(aggregation_level = aggregation_level,
        vars_to_summarize = outcome, summary_fn = 'sd')[[sd_varname]]
    data_mean <- get_state_by_time_variation(aggregation_level = aggregation_level,
        vars_to_summarize = outcome, summary_fn = 'mean')[[mean_varname]]
    stopifnot(! is.null(data_sd), ! is.null(data_mean))

    sale_tot_divisor <- 1000
    if (outcome == 'sale_tot') {
        to_plot <- to_plot %>% mutate(coef = coef / sale_tot_divisor,
                                      se   = se   / sale_tot_divisor)
        data_sd <- data_sd / sale_tot_divisor
        data_mean <- data_mean / sale_tot_divisor
    }
    to_plot <- calculate_effect_sizes(to_plot, data_sd = data_sd, data_mean = data_mean)

    # NB: If you uncomment this block, you have to rejigger the sale_tot_divisor.
    # control_states <- find_match_states_crude()
    # individual_state_std_dev <- lapply_bind_rows(c('AK', control_states),
    #     get_state_by_time_variation,
    #     aggregation_level = aggregation_level, winsorize_pct = NULL,
    #     rbind_src_id = 'state', parallel_cores = 1) %>%
    #     select_(.dots = c('state', sd_varname)) %>%
    #     # use a common name regardless of outcome so I don't have to fuss with dplyr and
    #     # ggplot standard evaluation later
    #     setNames(c('state', 'outcome_sd'))
    # std_devs <- data_frame(state = 'Pooled', outcome_sd = data_sd) %>%
    #     bind_rows(individual_state_std_dev) %>%
    #     # Explicitly set the factor and its ordered levels for a better plot legend
    #     mutate(state = factor(state, levels = c('Pooled', 'AK', control_states),
    #                           labels = c('Pooled', 'AK', control_states), ordered = TRUE))
    # if (outcome == 'sale_tot') {
    #     # data_sd <- data_sd / sale_tot_divisor
    #     std_devs <- std_devs %>% mutate(outcome_sd = outcome_sd / sale_tot_divisor)
    # #    data_sd_ak_only <- data_sd_ak_only / sale_tot_divisor
    # }

    # Define a bunch of labels.
    aggregation_level_noun <- c('daily' = 'day', 'weekly' = 'week')[[aggregation_level]]
    lab_x <- sprintf('Window start (event %ss)', aggregation_level_noun)

    outcome_str <- tolower(OUTCOME_VARS[[outcome]]) %>% gsub('msrp', 'MSRP', .)
    lab_y <- outcome_str
    lab_y_effect <- sprintf("Effect size (std. dev. %s)", outcome_str)
    lab_y_effect_mean <- sprintf("Effect size (fraction of mean %s)", outcome_str)
    coef_plot <- ggplot(to_plot,
        aes(x = start, y = coef, ymin = conf95_lower, ymax = conf95_upper)) +
        geom_point() +
        geom_errorbar() +
        labs(x = lab_x, y = lab_y) +
        scale_color_manual(values = PALETTE_8_COLOR_START_WITH_BLACK) +
        PLOT_THEME
    coef_effect_plot <- ggplot(to_plot,
        aes(x = start, y = coef_effect, ymin = conf95_lower_effect,
            ymax = conf95_upper_effect)) +
        geom_point() +
        geom_errorbar() +
        labs(x = lab_x, y = lab_y_effect) +
        scale_color_manual(values = PALETTE_8_COLOR_START_WITH_BLACK) +
        PLOT_THEME
    coef_effect_mean_plot <- ggplot(to_plot,
        aes(x = start, y = coef_effect_mean,ymin = conf95_lower_effect_mean,
            ymax = conf95_upper_effect_mean)) +
        geom_point() +
        geom_errorbar() +
        labs(x = lab_x, y = lab_y_effect_mean) +
        scale_color_manual(values = PALETTE_8_COLOR_START_WITH_BLACK) +
        PLOT_THEME

    title <- render_title(title,
        default = 'Anticipation window treatment coefficient for varying window starts')
    title_pattern <- if_else(title == '', '_notitle', '')
    coef_effect_plot <- coef_effect_plot + labs(title = title)
    coef_effect_mean_plot <- coef_effect_mean_plot + labs(title = title)
    # Then make the versions with lines for the standard deviations
    # First, the one with a single, pooled std dev
    # coef_plot_with_pooled_sd <- coef_plot + geom_hline(yintercept = data_sd)

    # Then, to make sure it's not one state swamping the std dev calculation, do each
    # separately.
    # TODO: consider bringing this back, but it will require a bit of adjustment with
    # dividing by different standard deviations:
    # coef_plot_with_states_sd <- coef_plot +
        # geom_hline(data = std_devs, aes(yintercept = outcome_sd, color = state))

    # Make filenames like anticipation_window_sale_count_weekly_notitle.pdf
    filename_part <- sprintf('anticipation_window_%s_%s%s', outcome, aggregation_level,
                             title_pattern)
    save_plot(coef_plot, paste0(filename_part, '_coef.pdf'))

    if (! endsWith(outcome, 'log')) {
        # Don't make effect plots for logged outcomes because that doesn't make much
        # sense.(Don't worry about ggplot making the plots above; it doesn't do any work
        # until you actually display the plot.)
        save_plot(coef_effect_plot,      paste0(filename_part, '_coef_effect.pdf'))
        save_plot(coef_effect_mean_plot, paste0(filename_part, '_coef_effect_mean.pdf'))
    }
    # save_plot(coef_plot_with_pooled_sd, paste0(filename_part, '_pooled_sd.pdf'))
    # save_plot(coef_plot_with_states_sd, paste0(filename_part, '_states_sd.pdf'))
    invisible(to_plot)  # then return the data
}
