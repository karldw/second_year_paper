
pg_vacuum <- function(con, table_name='all', analyze=TRUE) {
    stopifnot(length(table_name) == 1)
    if (analyze) {
        sql_cmd <- "VACUUM ANALYZE"
    } else {
        sql_cmd <- "VACUUM"
    }
    if (table_name != 'all') {
        # default w/o table name is all tables in database
        known_tables <- DBI::dbListTables(con)
        stopifnot(table_name %in% known_tables)
        sql_cmd <- paste(sql_cmd, table_name)
    }

    res <- DBI::dbSendStatement(con, sql_cmd)
    stopifnot(DBI::dbHasCompleted(res))
}


pg_add_index <- function(con, table_name, indexed_col) {
    # This function is here so I don't have to remember the SQL index syntax and so I
    # don't do anything too dumb.
    stopifnot(length(table_name) == 1 && length(indexed_col) == 1)

    known_tables <- DBI::dbListTables(con)
    if (! table_name %in% known_tables) {
        err_msg <- sprintf("Table name '%s' is not in the database", table_name)
        stop(err_msg)
    }

    known_cols <- DBI::dbListFields(con, table_name)
    if(! indexed_col %in% known_cols) {
        err_msg <- sprintf("Column '%s' not found in table '%s'.",
                           indexed_col, table_name)
        stop(err_msg)
    }
    index_name <- paste0(indexed_col, '_index')

    sql_cmd <- sprintf("CREATE INDEX %s on %s (%s)", index_name, table_name, indexed_col)
    res <- DBI::dbSendStatement(con, sql_cmd)
    stopifnot(DBI::dbHasCompleted(res))
    return(index_name)
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
