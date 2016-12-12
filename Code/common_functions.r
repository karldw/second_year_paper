

.pg_assert_existence <- function(con, table_name, col_name=NULL) {
    if (! DBI::dbExistsTable(con, table_name)) {
        err_msg <- sprintf("Table name '%s' is not in the database", table_name)
        stop(err_msg)
    }
    if (! is.null(col_name)) {
        known_cols <- DBI::dbListFields(con, table_name)
        if(! col_name %in% known_cols) {
            err_msg <- sprintf("Column '%s' not found in table '%s'.",
                               col_name, table_name)
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
    # don't do anything too dumb.
    # Note, postgres is smart enough that you don't need to index a column that's already
    # unique, but if you want to ALTER TABLE to make a primary key, you have to start
    # with a unique index.
    stopifnot(length(table_name) == 1 && length(indexed_col) == 1)

    .pg_assert_existence(con, table_name, indexed_col)

    index_name <- paste0(indexed_col, '_index')

    if (unique_index) {
        unique_cmd <- 'UNIQUE'
    } else {
        unique_cmd <- ''
    }
    drop_cmd <- sprintf("DROP INDEX IF EXISTS %s", index_name)
    DBI::dbSendStatement(con, drop_cmd)
    # fillfactor to 100 because I'm never adding rows to this table
    add_cmd <- sprintf("CREATE %s INDEX %s on %s (%s) WITH (fillfactor = 100)",
                       unique_cmd, index_name, table_name, indexed_col)
    res <- DBI::dbSendStatement(con, add_cmd)
    stopifnot(DBI::dbHasCompleted(res))
    return(index_name)
}


pg_add_primary_key <- function(con, table_name, key_col) {
    stopifnot(length(table_name) == 1 && length(key_col) == 1)

    existing_index <- pg_add_index(con, table_name, key_col, unique_index=TRUE)

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
    stopifnot(! is.na(nrow_df))
    return(nrow_df)
}
