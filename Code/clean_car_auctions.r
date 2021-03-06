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
POSTGRES_CPI_TABLE <- 'cpi'


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

    # get the original field names, then tack an extra comma on the end.
    orig_col_names <- dbListFields(con, POSTGRES_ORIG_TABLE) %>%
        paste(collapse = ', ') %>% paste0(',')
    state_cols <- 'buy_state, sell_state, auction_state,'

    # This version, while easier to understand, uses too much temp disk space:
    # create_table_command <- paste(
    #     "CREATE UNLOGGED TABLE", POSTGRES_CLEAN_TABLE, "AS",
    #
    #         "WITH",
    #             # We'll merge against zipcode_state several times.
    #             # zip is the primary key in the zipcode table.
    #             "zipcode_state AS (SELECT zip, state FROM", POSTGRES_ZIPCODE_TABLE, "),",
    #             # First merge to create buy_state
    #             "joined_buy AS (SELECT", orig_col_names, "state AS buy_state",
    #                 "FROM", POSTGRES_ORIG_TABLE,
    #                 "LEFT JOIN zipcode_state ON zip = buy_zip),",
    #             # Then merge that result to create sell_state
    #             "joined_buy_sell AS (SELECT", orig_col_names,
    #                 "buy_state, state AS sell_state",
    #                 "FROM joined_buy",
    #                 "LEFT JOIN zipcode_state ON zip = sell_zip),",
    #             # Then merge that result to create auction_state
    #             "joined_buy_sell_auction AS (SELECT", orig_col_names,
    #                 "buy_state, sell_state, state AS auction_state",
    #                 "FROM joined_buy_sell",
    #                 "LEFT JOIN zipcode_state ON zip = auction_zip),",
    #             # Then merge that result with the CPI data to create sales_pr_real
    #             # (which will be renamed in the next step to sales_pr, but it's easier
    #             # here to keep the sales_pr variable in orig_col_names)
    #             "joined_buy_sell_auction_cpi AS (SELECT", orig_col_names,
    #                 "buy_state, sell_state, auction_state,",
    #                 "sales_pr / cpi_base_2016 as sales_pr_real,",
    #                 "concat(substring(vin FROM 1 FOR 8),",
    #                        "substring(vin FROM 10 FOR 2)) AS vin_pattern",
    #                 "FROM joined_buy_sell_auction AS a",
    #                 # year and month are, jointly, the primary key on the CPI table
    #                 "LEFT JOIN", POSTGRES_CPI_TABLE, "AS b",
    #                 "ON DATE_PART('YEAR', a.sale_date) = b.year AND",
    #                 "DATE_PART('MONTH', a.sale_date) = b.month),",
    #             "msrp_info AS (SELECT",
    #                 "msrp, vin_pattern, model_yr AS vin_decoder_model_yr FROM",
    #                 POSTGRES_VIN_DECODER_TABLE, "),",
    #             "joined_buy_sell_auction_cpi_msrp AS (SELECT", orig_col_names,
    #                 "buy_state, sell_state, auction_state,",
    #                 "sales_pr_real, x.vin_pattern, msrp, vin_decoder_model_yr",
    #                 "FROM joined_buy_sell_auction_cpi AS x",
    #                 "LEFT JOIN msrp_info AS y",
    #                 "ON x.vin_pattern = y.vin_pattern)",
    #     "SELECT",
    #         "sale_date, sell_zip, buy_zip, auction_zip, vin, miles,",
    #         # Finally switch around the sales_pr names, making sales_pr into
    #         # sales_pr_nominal and sales_pr_real into sales_pr (since this is the one
    #         # I'll want to use most of the time).
    #         "sales_pr AS sales_pr_nominal,",
    #         "sales_pr_real AS sales_pr,",
    #         "mmr, bid_ct, veh_type,",
    #         "buy_state, sell_state, auction_state, msrp,",
    #         # Trust vin_decoder_model_yr if available, and recode 0 in auction model_yr
    #         # as NULL.
    #         "COALESCE(vin_decoder_model_yr, NULLIF(model_yr, 0)) as model_yr,",
    #         "NULLIF(auction_code, '') AS auction_code,",
    #         "NULLIF(buyer_id, '')     AS buyer_id,",
    #         "NULLIF(cond, '')         AS cond,",
    #         "NULLIF(make, '')         AS make,",
    #         "NULLIF(model, '')        AS model,",
    #         "NULLIF(sale_type, '')    AS sale_type,",
    #         "NULLIF(salvg_flg, '')    AS salvg_flg,",
    #         "NULLIF(seller_id, '')    AS seller_id,",
    #         "NULLIF(seller_type, '')  AS seller_type,",
    #         "NULLIF(slrdlr_type, '')  AS slrdlr_type,",
    #         "concat_ws(' ', anncmts, remarks) AS comments,",
    #         "random() as rand",
    #     "FROM joined_buy_sell_auction_cpi_msrp",  # Pull from the WITH query above
    #     "WITH DATA",  # copy the data from the query, not just structure (the default)
    #     sep = '\n')

    # Make small temporary tables we'll use below (mostly for convenience, and to
    # document the primary keys in code).
    create_zipcode_state <- paste("CREATE TEMPORARY TABLE zipcode_state AS",
            "(SELECT zip, state FROM", POSTGRES_ZIPCODE_TABLE, ")")
    dbSendStatement(con, create_zipcode_state)
    pg_add_primary_key(con, 'zipcode_state', 'zip')

    create_msrp_info <- paste("CREATE TEMPORARY TABLE msrp_info AS",
        "(SELECT msrp, vin_pattern, model_yr AS vin_decoder_model_yr FROM",
        POSTGRES_VIN_DECODER_TABLE, ")")
    dbSendStatement(con, create_msrp_info)
    pg_add_primary_key(con, 'msrp_info', 'vin_pattern')

    create_table_command <- paste(
        "CREATE UNLOGGED TABLE",
        POSTGRES_CLEAN_TABLE,
        "AS",
        "SELECT",
            "sale_date, sell_zip, buy_zip, auction_zip, vin, miles,",
            # Rename the original sales_pr to sales_pr_nominal, since most of the time
            # we want to use inflation-adjusted prices.
            "sales_pr AS sales_pr_nominal, sales_pr_real AS sales_pr, msrp,",
            # Trust vin_decoder_model_yr if available, and recode 0 in auction model_yr
            # as NULL.
            "COALESCE(vin_decoder_model_yr, NULLIF(model_yr, 0)) as model_yr,",
            state_cols,
            "mmr, bid_ct, veh_type, vin_pattern,",
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
            "random() as rand",
        "FROM",

            # Merge on vin_pattern to get msrp and vin_decoder_model_yr (5)
            "(SELECT", orig_col_names, state_cols,
                "sales_pr_real, msrp, vin_decoder_model_yr,",
                # NB: this will only keep vin_pattern where we have matches, with the
                # VIN decoder, but that's all that matters.
                "msrp_info.vin_pattern",
            "FROM",

                # Merge on year and month to create sales_pr_real (4)
                "(SELECT",
                    orig_col_names, state_cols,
                    '(sales_pr / cpi_base_2016) AS sales_pr_real,',
                    # Make the vin_pattern variable for the upcoming merge with msrp_info:
                    "concat(substring(vin FROM 1 FOR 8),",
                           "substring(vin FROM 10 FOR 2)) AS vin_pattern",
                "FROM",

                    # Merge on auction_zip to create auction_state (3)
                    "(SELECT",
                        orig_col_names, 'buy_state, sell_state, state AS auction_state',
                    "FROM",

                        # Merge on sell_zip to create sell_state (2)
                        "(SELECT",
                            orig_col_names, "buy_state, state AS sell_state",
                        "FROM",

                            # Merge on buy_zip to create buy_state (1)
                            "(SELECT",
                                orig_col_names, "state AS buy_state",
                            "FROM",
                                POSTGRES_ORIG_TABLE,
                                "LEFT JOIN",
                                "zipcode_state",
                                "ON buy_zip = zip",
                            ") AS joined_buy",
                            # End of merge on buy_zip to create buy state (1)

                            "LEFT JOIN",
                            "zipcode_state",
                            "ON sell_zip = zip",
                        ") AS joined_buy_sell",
                        # End of merge on sell_zip to create sell_state (2)

                        "LEFT JOIN",
                        "zipcode_state",
                        "ON auction_zip = zip",
                    ") AS joined_buy_sell_auction",
                    # End of merge on auction_zip to create auction_state (3)

                    "LEFT JOIN",
                    POSTGRES_CPI_TABLE, "AS cpi",
                    "ON DATE_PART('year',  joined_buy_sell_auction.sale_date) = cpi.year",
                    "AND",
                    "DATE_PART('month', joined_buy_sell_auction.sale_date) = cpi.month",
                ") AS joined_buy_sell_auction_cpi",
                # End of merge on year and month to create sales_pr_real (4)

                "LEFT JOIN",
                "msrp_info",
                "ON msrp_info.vin_pattern = joined_buy_sell_auction_cpi.vin_pattern",
            ") AS joined_buy_sell_auction_cpi_msrp",
            # End of merge on vin_pattern to get msrp and vin_decoder_model_yr (5)
        sep = '\n')
    # message("\n")
    # exsql <- paste("EXPLAIN ANALYZE", create_table_command)
    # expl_raw <- RPostgreSQL::dbGetQuery(con, exsql)
    # expl <- paste(expl_raw[[1]], collapse = "\n")
    # message("<PLAN>\n", expl)
    # stop()
    set_seed_cmd <- "SELECT setseed(-0.783164798234)"
    suppressWarnings(dbExecute(con, set_seed_cmd))

    res <- dbSendStatement(con, create_table_command)
    # stopifnot(dbHasCompleted(res))
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
    regex_over12 <- paste(regex_over1, regex_over2, sep = "|")
    regex_over3456 <- paste(regex_over3, regex_over4, regex_over5, regex_over6, sep = "|")

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
        sep = ' OR ')

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
                                regex_inop, sep = '|')

    delete_conditions <- paste(
        "(salvg_flg = 'Y')",
        sprintf("((comments ~ '%s') AND (NOT (comments ~ '%s')))",
                regex_flood_fire, regex_fire_exclude),
        sprintf("(comments ~ '%s')", regex_combo_damage),
        sep = ' OR ')

    rows_deleted <- execute_deletion(con, delete_conditions)
    return(rows_deleted)
}


filter_price <- function(con, min_price = 100, max_price = 80000, msrp_factor = 1.5) {
    # This big commented-out section is here from a previous, more complicated situation.

    # The goal here is to drop rows outside the interval [100, min(80000, 1.5*msrp)]
    # The numbers, of course, can be changed.
    # To filter on msrp, we have to pull in the vin_decoder data, and to do that, we need
    # to match on the appropriate VIN substring
    # LEAST will ignore NULLs, so if MSRP is missing, just use 80000

    # Here's what the code would look like if msrp was already a column in the table:
    # delete_cond <- sprintf("sales_pr_nominal NOT BETWEEN %s AND (LEAST(%s, %s * msrp))",
    #                              sales_pr_min, sales_pr_max, msrp_factor)
    # rows_deleted <- execute_deletion(con, delete_cond)

    # The VIN match data has VIN positions 1-8, 10 and 11 (position 9 is a check digit)

    # I think this should work, but it will delete rows that don't match the VIN decoder.
    # Is that what I want?  (Alternative: do a left join)
    # delete_command <- paste0(
    # "DELETE FROM ", POSTGRES_CLEAN_TABLE, " AS auctions LEFT JOIN ",
    # POSTGRES_VIN_DECODER_TABLE, " AS vin_decoder WHERE (",
    # "(substring(auctions.vin FROM 1 FOR 8) || substring(auctions.vin FROM 10 FOR 2))",
    # " = vin_decoder.vin_pattern", ") AND ",
    # "(sales_pr_nominal NOT BETWEEN ", min_price, " AND LEAST(", max_price, ", ",
    #     msrp_factor, " * vin_decoder.msrp))"
    # )
    # delete_command <- paste(
    #     "WITH vin_msrp AS (",
    #         "select vin, msrp from",
    #         POSTGRES_CLEAN_TABLE, "AS x LEFT JOIN",
    #         POSTGRES_VIN_DECODER_TABLE, "AS y ON",
    #         "x.vin_pattern = y.vin_pattern",
    #     ")",
    #     "DELETE FROM", POSTGRES_CLEAN_TABLE, "AS auctions using vin_msrp",
    #     "where auctions.vin = vin_msrp.vin AND",
    #     # All the action is here:
    #     sprintf("sales_pr_nominal NOT BETWEEN %s AND LEAST(%s, %s * msrp)",
    #             min_price, max_price, msrp_factor)
    #     )
    # rows_deleted <- dbExecute(con, delete_command)
    # TODO: delete the code above, when I'm sure I don't need it.

    # The goal here is to drop rows outside the interval [100, min(80000, 1.5*msrp)]
    # The numbers, of course, can be changed.
    # LEAST will ignore NULLs, so if MSRP is missing, we just use 80000
    delete_cond <- sprintf("sales_pr_nominal NOT BETWEEN %s AND (LEAST(%s, %s * msrp))",
                           min_price, max_price, msrp_factor)
    rows_deleted <- execute_deletion(con, delete_cond)

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


filter_resale <- function(con, resale_min_acceptable_days) {
    stopifnot(length(resale_min_acceptable_days) == 1,
              is.numeric(resale_min_acceptable_days),
              resale_min_acceptable_days > 0,
              as.integer(resale_min_acceptable_days) == resale_min_acceptable_days)

    delete_cmd_resale <- paste(
        # Note that because of the lead,
        # the last sale will have a NULL sale_date_diff. Filter out NULLs, since we want
        # the final sales.  Also filter out sales gaps more than
        # resale_min_acceptable_days, since those gaps are okay.
        "WITH resale_vins AS (",
            # distinct_vehicle_sales clause:
            "SELECT sale_date, vin FROM (",
                "SELECT sale_date, vin,",
                # Calculate lead of sale_date within VINs, call it sale_date_diff
                "CASE WHEN (vin = LEAD(vin, 1, NULL) OVER (ORDER BY vin, sale_date)) THEN",
                "(LEAD(sale_date, 1, NULL) OVER (ORDER BY vin, sale_date) - sale_date)",
                "ELSE (NULL) END AS sale_date_diff",
                "FROM (",
                    # distinct_vehicle_sales clause:
                    "SELECT sale_date, vin FROM", POSTGRES_CLEAN_TABLE,
                        "GROUP BY vin, sale_date",
                        "ORDER BY vin, sale_date",
                    ") AS distinct_vehicle_sales",  # close the distinct_vehicle_sales clause
            ") AS calculated_date_diffs",  # close the calculated_date_diffs clause
            "WHERE (NOT (sale_date_diff IS NULL)) AND",
                "(sale_date_diff > ", resale_min_acceptable_days, ")",
        ")",  # close the resale_vins clause
        # Now that I have the resale_vins table, actually do the deletion:
        "DELETE FROM", POSTGRES_CLEAN_TABLE, "AS auctions using resale_vins",
        "WHERE resale_vins.vin = auctions.vin AND",
            "resale_vins.sale_date = auctions.sale_date"
        )
    resale_rows_deleted  <- dbExecute(con, delete_cmd_resale)
    return(resale_rows_deleted)
}


clean_data <- function(con, verbose) {
    # Delete resale first because I don't want to take the second-to-last sale if one of
    # the following filters deletes the last one.
    resale_min_acceptable_days <- 365
    message_if_verbose(sprintf("Dropping obs resold within %s days",
                               resale_min_acceptable_days), verbose)
    rows_deleted_resale <- filter_resale(con, resale_min_acceptable_days)

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
    rows_deleted_multistate <- filter_multistate(con)  # 85764

    deleted_counts <- c('resold'         = rows_deleted_resale,
                        'weird vehicles' = rows_deleted_weird_vehicles,
                        'bad odometer'   = rows_deleted_bad_odo,
                        'damaged'        = rows_deleted_damaged,
                        'price'          = rows_deleted_price,
                        'canadian'       = rows_deleted_canadian,
                        'multiple states'= rows_deleted_multistate)
    return(deleted_counts)
}


index_and_clean <- function(con, verbose) {
    message_if_verbose("Adding SQL indexes", verbose)
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
    message_if_verbose("  - vin_index", verbose)
    pg_add_index(con, POSTGRES_CLEAN_TABLE, 'vin')      # sale_date_index
    # You'd think we could make the random index unique, but there are collisions
    message_if_verbose("  - rand_index", verbose)
    pg_add_index(con, POSTGRES_CLEAN_TABLE, 'rand')           # rand_index
    # Not all of the zips are valid, but that's probably okay.
    # (that is, they look like a five-digit zip, but don't match POSTGRES_ZIPCODE_TABLE),
    # so you can't pg_add_foreign_key on them.
    # Same is true of the VIN decoder.

    # pg_add_foreign_key(con, POSTGRES_CLEAN_TABLE, 'buy_zip',
    #                    POSTGRES_ZIPCODE_TABLE, 'zip')
    # pg_add_foreign_key(con, POSTGRES_CLEAN_TABLE, 'sell_zip',
    #                    POSTGRES_ZIPCODE_TABLE, 'zip')
    # pg_add_foreign_key(con, POSTGRES_CLEAN_TABLE, 'auction_zip',
    #                    POSTGRES_ZIPCODE_TABLE, 'zip')
    # pg_add_foreign_key(con, POSTGRES_CLEAN_TABLE, 'vin_pattern',
    #                    POSTGRES_VIN_DECODER_TABLE, 'vin_pattern')
    message_if_verbose("  - vacuuming and analyzing", verbose)
    pg_vacuum(con, POSTGRES_CLEAN_TABLE)
}


delete_orig_table <- function(con, verbose) {
    # Only run this if you really need the disk space.
    message_if_verbose("Dropping orignal table", verbose)
    dbRemoveTable(con, POSTGRES_ORIG_TABLE)
}


report_counts <- function(deleted_counts) {
    for (i in seq_along(deleted_counts)) {
        count_name <- gsub('\\W', '_', names(deleted_counts)[[i]], perl = TRUE)
        count_filename <- paste0('clean_car_auctions_', count_name, '.tex')
        count_value <- as.integer(deleted_counts[[i]])
        # write the snippets out to ../Text/Generated_snippets/
        make_snippet(count_value, count_filename, lazy = FALSE)
    }
}


main <- function(verbose = TRUE) {
    pg_user <- Sys.info()[["user"]] %>% tolower()
    con <- dbConnect("PostgreSQL", dbname = POSTGRES_DB,
                     user = pg_user, password = pg_user)

    copy_orig_table(con, verbose)
    deleted_counts <- clean_data(con, verbose)
    # delete_orig_table(con, verbose)  # Only run this if you really need the disk space.
    report_counts(deleted_counts)
    index_and_clean(con, verbose)
    dbDisconnect(con)

    return(deleted_counts)
}

deleted_counts <- main()
print(deleted_counts)
