# Name: clean_car_auctions.r
# This program is mostly a translation of the cleaning functions in
# Dropbox/KarlJim/CarPriceData/Code/define_programs.do

source('r_defaults.r')
options(warnPartialMatchArgs = FALSE)
install_lazy(c('RPostgreSQL', 'dplyr', 'magrittr'), verbose = FALSE)

library(RPostgreSQL)
#library(dplyr)
library(magrittr)
POSTGRES_DB <- 'second_year_paper'
POSTGRES_ORIG_TABLE <- 'all_years_all_sales'
POSTGRES_CLEAN_TABLE <- 'auctions_cleaned'
POSTGRES_VIN_DECODER_TABLE <- 'vin_decoder'
POSTGRES_ZIPCODE_TABLE <- 'zipcode'

message_if_verbose <- function(msg, verbose) {
    # Just for ease of reading code later
    if (verbose) {
        message(msg)
    }
    invisible()
}

copy_orig_table <- function(con, verbose){
    if (dbExistsTable(con, POSTGRES_CLEAN_TABLE)) {
        if (verbose) {
            message(paste('Deleting existing table:', POSTGRES_CLEAN_TABLE))
        }
        dbRemoveTable(con, POSTGRES_CLEAN_TABLE)
    }
    message_if_verbose(sprintf("Starting by copying %s to %s", POSTGRES_ORIG_TABLE,
                       POSTGRES_CLEAN_TABLE), verbose)

    # CREATE TABLE auctions_cleaned AS
    #
    #     SELECT auction_zip, sell_zip, buy_zip, sale_date, seller_id, seller_type,
    #     slrdlr_type, buyer_id, auction_code, vin, model_yr, make, model, miles,
    #     sale_type, salvg_flg, cond, anncmts, remarks, sales_pr, mmr, bid_ct,
    #     veh_type, comments, vin_pattern, buy_state, sell_state,
    #     state AS auction_state,
    #     concat_ws(' ', anncmts, remarks) AS comments,
    #     concat(substring(vin FROM 1 FOR 8),
    #            substring(vin FROM 10 FOR 2)) AS vin_pattern
    #     FROM
    #         (
    #             SELECT * FROM
    #                 (
    #                 SELECT * FROM
    #                     (
    #                         SELECT * FROM
    #                             all_years_all_sales
    #                         LEFT JOIN
    #                             (SELECT zip, state as buy_state FROM zipcode)
    #                         ON (buy_zip = zip)
    #                     )
    #                 LEFT JOIN
    #                     (SELECT zip, state as sell_state FROM zipcode)
    #                 ON (sell_zip = zip)
    #                 )
    #             LEFT JOIN
    #                 (SELECT zip, state AS auction_state FROM zipcode)
    #             ON (auction_zip = zip)
    #         )

    # create_table_command <- paste(
    #     "SELECT *, concat_ws(' ', anncmts, remarks) AS comments,",
    #     "concat(substring(vin FROM 1 FOR 8), substring(vin FROM 10 FOR 2)) AS vin_pattern",
    #     "INTO", POSTGRES_CLEAN_TABLE, 'FROM', POSTGRES_ORIG_TABLE)

    # get the original field names, then tack an extra comma on the end.

    orig_col_names <- dbListFields(con, POSTGRES_ORIG_TABLE) %>%
        paste(collapse=', ') %>% paste0(',')


    create_table_command <- paste(
        "CREATE TABLE",
        POSTGRES_CLEAN_TABLE,
        "AS",
            "SELECT",
                "sale_date, sell_zip, buy_zip, auction_zip, vin, model_yr, miles,",
                "sales_pr, mmr, bid_ct, veh_type,",
                "buy_state, sell_state, auction_state,",
                "NULLIF(auction_code, '') AS auction_code,",
                "NULLIF(buyer_id, '')     AS buyer_id,",
                "NULLIF(cond, '')         AS cond,",
                "NULLIF(make, '')         AS make,",
                "NULLIF(model, '')        AS model,",
                "NULLIF(sale_type, '')    AS sale_type,",
                "NULLIF(salvg_flg, '')    AS salvg_flg,",
                "NULLIF(seller_id, '')    AS seller_id,",
                "NULLIF(seller_type, '')  AS seller_type,",
                "NULLIF(slrdlr_type, '')  AS slrdlr_type,",
                "concat_ws(' ', anncmts, remarks) AS comments,",
                "concat(substring(vin FROM 1 FOR 8),",
                       "substring(vin FROM 10 FOR 2)) AS vin_pattern,",
                "random() as rand",
            "FROM",
                "(",
                    "SELECT",
                        orig_col_names,
                        "buy_state, sell_state, auction_state",
                    "FROM",
                        "(",
                        "SELECT",
                            orig_col_names,
                            "buy_state, sell_state",
                        "FROM",
                            "(",
                                "SELECT",
                                    orig_col_names,
                                    "buy_state",
                                "FROM",
                                    POSTGRES_ORIG_TABLE,
                                "LEFT JOIN",
                                    "(SELECT zip, state as buy_state FROM",
                                    POSTGRES_ZIPCODE_TABLE,
                                    ") AS zipcode_buy_state",
                                "ON (buy_zip = zip)",
                            ") AS joined_buy",
                        "LEFT JOIN",
                            "(SELECT zip, state as sell_state FROM",
                            POSTGRES_ZIPCODE_TABLE,
                            ") AS zipcode_sell_state",
                        "ON (sell_zip = zip)",
                        ") AS joined_buy_sell",
                    "LEFT JOIN",
                        "(SELECT zip, state AS auction_state FROM",
                        POSTGRES_ZIPCODE_TABLE,
                        ") AS zipcode_auction_state",
                    "ON (auction_zip = zip)",
                ") AS joined_buy_sell_auction",
        sep='\n')

    set_seed_cmd <- "SELECT setseed(-0.783164798234)"
    suppressWarnings(dbExecute(con, set_seed_cmd))

    res <- dbSendStatement(con, create_table_command)
    stopifnot(dbHasCompleted(res))
}


execute_deletion <- function(con, delete_conditions) {
    delete_command <- paste("DELETE FROM", POSTGRES_CLEAN_TABLE, "WHERE",
                            delete_conditions)
    rows_deleted <- dbExecute(con, delete_command)
    return(rows_deleted)
}


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

    rows_deleted <- execute_deletion(con, delete_conditions)
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

    rows_deleted <- execute_deletion(con, delete_conditions)
    return(rows_deleted)
}


filter_price <- function(con, min_price = 100, max_price = 80000, msrp_factor = 1.5) {

    # The goal here is to drop rows outside the interval [100, min(80000, 1.5*msrp)]
    # The numbers, of course, can be changed.
    # To filter on msrp, we have to pull in the vin_decoder data, and to do that, we need
    # to match on the appropriate VIN substring
    # LEAST will ignore NULLs, so if MSRP is missing, just use 80000

    # Here's what the code would look like if msrp was already a column in the table:
    # delete_conditions <- sprintf("sales_pr NOT BETWEEN %s AND (LEAST(%s, %s * msrp))",
    #                              sales_pr_min, sales_pr_max, msrp_factor)
    # rows_deleted <- execute_deletion(con, delete_conditions)

    # The VIN match data has VIN positions 1-8, 10 and 11 (position 9 is a check digit)

    # I think this should work, but it will delete rows that don't match the VIN decoder.
    # Is that what I want?  (Alternative: do a left join)
    # delete_command <- paste0(
    #     "DELETE FROM ", POSTGRES_CLEAN_TABLE, " AS auctions LEFT JOIN ",
    #     POSTGRES_VIN_DECODER_TABLE, " AS vin_decoder WHERE (",
    #     "(substring(auctions.vin FROM 1 FOR 8) || substring(auctions.vin FROM 10 FOR 2))",
    #     " = vin_decoder.vin_pattern", ") AND ",
    #     "(sales_pr NOT BETWEEN ", min_price, " AND LEAST(", max_price, ", ",  msrp_factor,
    #                                                      " * vin_decoder.msrp))"
    # )
    delete_command <- paste(
        "WITH vin_msrp AS (",
            "select vin, msrp from",
            POSTGRES_CLEAN_TABLE, "AS x LEFT JOIN",
            POSTGRES_VIN_DECODER_TABLE, "AS y ON",
            "x.vin_pattern = y.vin_pattern",
        ")",
        "DELETE FROM", POSTGRES_CLEAN_TABLE, "AS auctions using vin_msrp",
        "where auctions.vin = vin_msrp.vin AND",
        # All the action is here:
        sprintf("sales_pr NOT BETWEEN %s AND LEAST(%s, %s * msrp)",
                min_price, max_price, msrp_factor)
        )
    rows_deleted <- dbExecute(con, delete_command)
    return(rows_deleted)
}


filter_canadian <- function(con) {
    delete_conditions <- sprintf("comments ~ '%s'", "CAND|CANAD|CNAD")
    rows_deleted <- execute_deletion(con, delete_conditions)
    return(rows_deleted)
}


filter_weird_vehicles <- function(con) {
    # In order, these are: trailers, boats, air compressors (?), golf carts, vehicles
    # with incomplete bodies, ATVs and RVs.
    delete_conditions <- "veh_type IN ('A', 'B', 'C', 'G', 'I', 'P', 'R')"
    rows_deleted <- execute_deletion(con, delete_conditions)
    return(rows_deleted)
}


filter_multistate <- function(con) {
    # not yet tested...
    # buy_state
    delete_cmd_buyers <- paste(
        "DELETE FROM",
        POSTGRES_CLEAN_TABLE,
        "AS auctions",
        "USING (",
            "SELECT buyer_id",
            "FROM (SELECT buyer_id, buy_state,",
                  "COUNT(*) OVER (PARTITION BY buyer_id) AS state_count",
            "FROM (SELECT buyer_id, buy_state",
            "FROM (SELECT buyer_id, buy_state",
            "FROM auctions_cleaned",
            "WHERE ((NOT((buyer_id) IS NULL)) AND (NOT((buy_state) IS NULL)))) gcbljb",
            "GROUP BY buyer_id, buy_state) cwlvmtvtlb) mwglxvbdut",
            "WHERE (state_count > 1.0)",
        ") AS bad_buyer_ids",
        "WHERE bad_buyer_ids.buyer_id = auctions.buyer_id"
        )
    # sell_state:
    delete_cmd_sellers <- paste(
        "DELETE FROM",
        POSTGRES_CLEAN_TABLE,
        "AS auctions",
        "USING (",
            "SELECT seller_id",
            "FROM (SELECT seller_id, sell_state,",
                  "COUNT(*) OVER (PARTITION BY seller_id) AS state_count",
            "FROM (SELECT seller_id, sell_state",
            "FROM (SELECT seller_id, sell_state",
            "FROM auctions_cleaned",
            "WHERE ((NOT((seller_id) IS NULL)) AND (NOT((sell_state) IS NULL)))) ksxnpl",
            "GROUP BY seller_id, sell_state) mmqkuetnjn) tlhxdtjxpr",
            "WHERE (state_count > 1.0)",
        ") AS bad_seller_ids",
        "WHERE bad_seller_ids.seller_id = auctions.seller_id")

    buyer_rows_deleted  <- dbExecute(con, delete_cmd_buyers)   # ~39 buyers
    seller_rows_deleted <- dbExecute(con, delete_cmd_sellers)  # ~12 sellers
    return(buyer_rows_deleted + seller_rows_deleted)
}


clean_data <- function(con, verbose) {
    message_if_verbose("Dropping weird vehicles", verbose)
    rows_deleted_weird_vehicles <- filter_weird_vehicles(con)  # 20299

    message_if_verbose("Dropping bad odometer", verbose)
    rows_deleted_bad_odo <- filter_bad_odo(con)  # 586941

    message_if_verbose("Dropping damaged vehicles", verbose)
    rows_deleted_damaged <- filter_damaged(con)  # 2665224

    message_if_verbose("Dropping inappropriate prices", verbose)
    rows_deleted_price <- filter_price(con)  # 59193

    message_if_verbose("Dropping Canadian cars", verbose)
    rows_deleted_canadian <- filter_canadian(con)  # 132118

    message_if_verbose("Dropping obs with multiple buy/sell states", verbose)
    rows_deleted_multistate <- filter_multistate(con)

    deleted_counts <- c('weird vehicles' = rows_deleted_weird_vehicles,
                        'bad odometer'   = rows_deleted_bad_odo,
                        'damaged'        = rows_deleted_damaged,
                        'price'          = rows_deleted_price,
                        'canadian'       = rows_deleted_canadian,
                        'multiple states'= rows_deleted_multistate)
    return(deleted_counts)
}


index_and_clean <- function(con, verbose) {
    message_if_verbose("Adding SQL indexes (ignore the messages about 'does not exist, skipping')", verbose)
    message_if_verbose("  - buy_state_index", verbose)
    pg_add_index(con, POSTGRES_CLEAN_TABLE, 'buy_state')      # buy_state_index
    message_if_verbose("  - sell_state_index", verbose)
    pg_add_index(con, POSTGRES_CLEAN_TABLE, 'sell_state')     # sell_state_index
    message_if_verbose("  - auction_state_index", verbose)
    pg_add_index(con, POSTGRES_CLEAN_TABLE, 'auction_state')  # auction_state_index
    message_if_verbose("  - buyer_id_index", verbose)
    pg_add_index(con, POSTGRES_CLEAN_TABLE, 'buyer_id')       # buyer_id_index
    message_if_verbose("  - sale_date_index", verbose)
    pg_add_index(con, POSTGRES_CLEAN_TABLE, 'sale_date')      # sale_date_index

    # You'd think we could make the random index unique, but there are collisions
    message_if_verbose("  - rand_index", verbose)
    pg_add_index(con, POSTGRES_CLEAN_TABLE, 'rand')           # rand_index
    # Not all of the zips are valid, but that's probably okay.
    # pg_add_foreign_key(con, POSTGRES_CLEAN_TABLE, 'buy_zip', POSTGRES_ZIPCODE_TABLE, 'zip')
    # pg_add_foreign_key(con, POSTGRES_CLEAN_TABLE, 'sell_zip', POSTGRES_ZIPCODE_TABLE, 'zip')
    # pg_add_foreign_key(con, POSTGRES_CLEAN_TABLE, 'auction_zip', POSTGRES_ZIPCODE_TABLE, 'zip')
    # pg_add_foreign_key(con, POSTGRES_CLEAN_TABLE, 'vin_pattern',
    #                    POSTGRES_VIN_DECODER_TABLE, 'vin_pattern')
    message_if_verbose("  - vacuuming and analyzing", verbose)
    pg_vacuum(con, POSTGRES_CLEAN_TABLE)
}


# replace_blanks_with_null <- function(con, verbose) {
#     message_if_verbose('Making blank strings NULL', verbose)
#     .replace_one_blank <- function(column_name, con) {
#         sql_cmd <- sprintf("UPDATE %s SET %s = NULL where %s = ''",
#                            POSTGRES_CLEAN_TABLE, column_name, column_name)
#         res <- dbSendStatement(con, sql_cmd)
#         stopifnot(dbHasCompleted(res))
#     }
#     vars_to_check <- c('buyer_id', 'seller_id', 'seller_type', 'slrdlr_type',
#                        'auction_code', 'make', 'model', 'sale_type', 'salvg_flg', 'cond')
#     # these could definitely be run in parallel:
#     lapply(vars_to_check, .replace_one_blank, con=con)
#
#     invisible()
# }


delete_orig_table <- function(con, verbose) {
    message_if_verbose("Dropping orignal table", verbose)
    dbRemoveTable(con, POSTGRES_ORIG_TABLE)
}


# add_random_index <- function(con, verbose) {
#
#     set_seed_cmd <- "SET SEED 783164798234"
#     add_column_cmd <- sprintf('ALTER TABLE %s ADD COLUMN random_index float8 NOT NULL '
# }


main <- function(verbose = TRUE) {
    con <- dbConnect("PostgreSQL", dbname = POSTGRES_DB)

    copy_orig_table(con, verbose)
    deleted_counts <- clean_data(con, verbose)
    index_and_clean(con, verbose)
    # delete_orig_table(con, verbose)
    dbDisconnect(con)

    return(deleted_counts)
}

deleted_counts <- main()
print(deleted_counts)
