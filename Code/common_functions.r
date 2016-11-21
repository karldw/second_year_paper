


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
    sql_cmd <- sprintf("CREATE %s INDEX %s on %s (%s)",
                       unique_cmd, index_name, table_name, indexed_col)
    res <- DBI::dbSendStatement(con, sql_cmd)
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
