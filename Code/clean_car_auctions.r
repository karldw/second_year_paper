# Name: clean_car_auctions.r
# This program is mostly a translation of the cleaning functions in
# Dropbox/KarlJim/CarPriceData/Code/define_programs.do


install_lazy(c('RPostgreSQL', 'dplyr', 'magrittr'), verbose = FALSE)

library(RPostgreSQL)
#library(dplyr)
library(magrittr)
POSTGRES_DB <- 'second_year_paper'
POSTGRES_ORIG_TABLE <- 'all_years_all_sales'
POSTGRES_CLEAN_TABLE <- 'auctions_cleaned'

filter_bad_odo <- function(con) {
    # Define some regexes:
    # (remember that all escapes have to be doubled to get R to pass the single-escaped
    # string to postgres)

    # Definitely over 100,000 miles:
    regex_over1 <- "[1-9]00\\s*K"
    regex_over2 <- "[1-9]00,?000"
    # Very likely over 100,000 miles:
    regex_over3 <- "OVE?R.*[1-3]00"
    regex_over4 <- "[1-3]00.*OVE?R"
    regex_over5 <- "MI?LE?S\\s*OVE?R"  # "MILES OVER", vowels optional
    regex_over6 <- "O/[1-3]00"
    # Look for money numbers (escape $ to avoid postgres treating it as an anchor)
    # exception applies to 3,4,5 and 6 above
    regex_over3456_exclude <- "\\$\\s*[0-9\\.,]*00"

    # Combine:
    regex_over12 <- paste(regex_over1, regex_over2, sep="|")
    regex_over3456 <- paste(regex_over3, regex_over4, regex_over5, regex_over6, sep="|")

    # The following odometers also match strings like "5 DIGIT ODO", which seems okay.
    # Match "ODO", without any letters beforehand and no letters except "M" after.
    # (Avoid matching "ODOR")
    regex_odo <- "[^A-Z]ODO[^A-LN-Z]"
    regex_odo_exclude <- "DIG(?:ITAL)?\\s*ODO"  # (?:______) is a non-capturing group

    # Look for noted discrepancies
    regex_discrepancy <- "DI?SCR"

    delete_conditions <- paste(
        "miles <= 100",
        sprintf("((comments ~ '%s') AND (miles < 100000))", regex_over12),
        sprintf("((comments ~ '%s') AND (miles < 100000) AND (NOT (comments ~ '%s')))",
                regex_over3456, regex_over3456_exclude),
        sprintf("((comments ~ '%s') AND (NOT (comments ~ '%s')))",
                regex_odo, regex_odo_exclude),
        sep=' OR ')

    delete_command <- paste("DELETE FROM", POSTGRES_CLEAN_TABLE, "WHERE",
                            delete_conditions)
    rows_deleted <- DBI::dbExecute(con, delete_command)
    return(rows_deleted)
}


filter_damaged <- function(con) {
    regex_salvage <- "SAL[^A-UW-Z]"  # "SAL", then no letter except "V"

    # just "FLD" matches other things:
    regex_flood_fire <- "FLOOD|FIRE|FLD\\s*DMG"
    regex_fire_exclude <- "FIREST|MISS?FIRE"  # FIRESTONE (tires) or MISFIRE
    regex_lemon_junk <- "LEMON|[^A-Z]LMN[^A-Z]|JU?NK"  # Lemon law or junked

    # Look for things flagged as damaged, destroyed, bad components or needing a tow
    # ("DEST" mostly matches against cars with a certificate of destruction)
    regex_damage <- "DMG|DAM|BAD|TOW|DEST"

    # Vehicles flagged as inoperable (look only at the start of the string because in
    # other places, inop refers to specific components, eg, the odometer)
    regex_inop <- "^\\s*INOP"

    regex_combo_damage <- paste(regex_salvage, regex_lemon_junk, regex_damage,
                                regex_inop, sep='|')

    delete_conditions <- paste(
        "(salvg_flg = 'Y')",
        sprintf("((comments ~ '%s') AND (NOT (comments ~ '%s')))",
                regex_flood_fire, regex_fire_exclude),
        sprintf("(comments ~ '%s')", regex_combo_damage),
        sep=' OR ')

    delete_command <- paste("DELETE FROM", POSTGRES_CLEAN_TABLE, "WHERE",
                            delete_conditions)
    rows_deleted <- DBI::dbExecute(con, delete_command)
    return(rows_deleted)
}


filter_price <- function(con) {


    
}
copy_orig_table <- function(con, verbose){
    if (DBI::dbExistsTable(con, POSTGRES_CLEAN_TABLE)) {
        if (verbose) {
            message(paste('Deleting existing table:', POSTGRES_CLEAN_TABLE))
        }
        DBI::dbRemoveTable(con, POSTGRES_CLEAN_TABLE)
    }
    # http://stackoverflow.com/a/6613805
    create_table_command <- paste("SELECT *,",
                                  "concat_ws(' ', anncmts, remarks) as comments",
                                  "INTO", POSTGRES_CLEAN_TABLE,
                                  'FROM', POSTGRES_ORIG_TABLE)
    res <- DBI::dbSendStatement(con, create_table_command)
    stopifnot(DBI::dbHasCompleted(res))
}


clean_data <- function(con, verbose) {
    rows_deleted_bad_odo <- filter_bad_odo(con)  #  601321
    rows_deleted_damaged <- filter_damaged(con)  # 2665602
    deleted_counts <- c('bad odometer' = rows_deleted_bad_odo,
                        'damaged' = rows_deleted_damaged)
    return(deleted_counts)
}


main <- function(verbose = TRUE) {
    con <- DBI::dbConnect("PostgreSQL", dbname = POSTGRES_DB)
    copy_orig_table(con, verbose)
    deleted_counts <- clean_data(con, verbose)
    DBI::dbDisconnect(con)
    return(deleted_counts)
}
#main()
