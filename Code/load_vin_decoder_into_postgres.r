source('r_defaults.r')

install_lazy(c('readr', 'RPostgreSQL', 'dplyr'), verbose = FALSE)
library(RPostgreSQL)
library(readr)
library(dplyr)
options(warn = 2)
set.seed(198872394)

POSTGRES_DB <- 'second_year_paper'
POSTGRES_TABLE <- 'vin_decoder'
try(
CSV_DIR <- file.path(dropbox_home()[1],
                     'KarlJim/CarPriceData/VINdecoder/DataOne_US_LDV_Data')
, silent = TRUE
)
if (! (exists('CSV_DIR') & dir.exists(CSV_DIR))) {
    CSV_DIR <- '~/Work/second_year_paper/Data/VINdecoder/DataOne_US_LDV_Data'
}
stopifnot(dir.exists(CSV_DIR))


shuffle <- function(.data) {
    dplyr::sample_frac(.data, 1, replace = FALSE)
}


.load_vin_reference <- function() {
    csv_filename <- file.path(CSV_DIR, 'VIN_REFERENCE.csv')

    load_cols <- cols_only(
        # vin_id = col_integer(),
        vehicle_id = col_integer(),
        vin_pattern = col_character(),
        year = col_integer(),
        # make = col_character(),
        # model = col_character(),
        # trim = col_character(),
        # style = col_character(),
        # mfr_model_num = col_character(),
        # mfr_package_code = col_character(),
        # doors = col_integer(),
        # drive_type = col_character(),
        vehicle_type = col_character(),
        # rear_axle = col_character(),
        # body_type = col_character(),
        # body_subtype = col_character(),
        # bed_length = col_character(),
        engine_id = col_integer(),
        # engine_name = col_character(),
        # engine_size = col_double(),
        # engine_block = col_character(),
        # engine_cylinders = col_integer(),
        # engine_valves = col_integer(),
        # engine_induction = col_character(),
        # engine_aspiration = col_character(),
        # engine_cam_type = col_character(),
        fuel_type = col_character()
        # trans_id = col_integer(),
        # trans_name = col_character(),
        # trans_type = col_character(),
        # trans_speeds = col_integer(),
        # wheelbase = col_character(),
        # gross_vehicle_weight_range =  = col_character(),
        # restraint_type = col_character(),
        # brake_system = col_character(),
        # country_of_mfr = col_character(),
        # plant =  = col_character()
    )
    # Harmonize the fuel types with LKP_VEH_MPG.csv, where the categories are Diesel,
    # Ethanol, Gasoline, Hydrogen, Natural Gas and Propane.
    fuel_types <- c(
        "B" = "Diesel",  # Biodiesel
        "D" = "Diesel",
        "F" = "Ethanol",  # flex fuel
        "G" = "Gasoline",
        "H" = "Hydrogen",
        "I" = "Gasoline",  # Plug-in hybrid
        "N" = "Natural Gas",
        "P" = "Propane",
        "Y" = "Gasoline"  # Hybrid
    )
    fuel_type_translation <- data_frame(fuel_code = names(fuel_types),
                                        fuel_type = unname(fuel_types))
    df <- readr::read_csv(csv_filename, col_types = load_cols, progress = FALSE) %>%
        dplyr::rename(model_yr = year, fuel_code = fuel_type) %>%
        dplyr::filter(! fuel_code %in% c("L", "", NA)) %>%
        full.join(fuel_type_translation, by = 'fuel_code') %>%
        ensure_id_vars(vehicle_id, vin_pattern)
    stopifnot(! anyNA(df))
    # drop fuel_code here because I want to check for NAs in it.
    df %>% select(-fuel_code) %>% return()
}


.load_veh_price <- function() {
    csv_filename <- file.path(CSV_DIR, 'VEH_PRICE.csv')
    load_cols <- cols_only(
        # veh_price_id = col_integer(),
        vehicle_id = col_integer(),
        msrp = col_integer()
        # invoice = col_integer(),
        # dest = col_integer(),
        # gas_guzzler_tax = col_integer()
    )
    df <- readr::read_csv(csv_filename, col_types=load_cols, progress=FALSE) %>%
        dplyr::mutate(msrp = dplyr::if_else(msrp == 0, NA_integer_, msrp)) %>%
        ensure_id_vars(vehicle_id)
    return(df)
}


.load_lkp_veh_mpg <- function() {
    # Load MPG info

    csv_filename <- file.path(CSV_DIR, 'LKP_VEH_MPG.csv')
    load_cols <- cols_only(
        # veh_mpg_id = col_integer(),
        vehicle_id = col_integer(),
        engine_id = col_integer(),
        transmission_id = col_integer(),
        fuel_type = col_character(),
        fuel_grade = col_character(),
        # city_old = col_integer(),
        # highway_old = col_integer(),
        # combined_old = col_integer(),
        city = col_integer(),
        highway = col_integer(),
        combined = col_integer()
    )

    df <- readr::read_csv(csv_filename, col_types=load_cols, progress=FALSE) %>%
        # shuffle the rows so the filtering is random
        shuffle() %>%
        dplyr::group_by(vehicle_id, engine_id, transmission_id, fuel_type) %>%
        # within groups defined by the variables above, if there are duplicates,
        # keep only the non-premium fuel grade
        # First tag groups with duplicates and provide an index to select one row from
        # each duplicated group, making sure not to select "Premium" as that one row
        # because I think non-premium versions of the car are more important.
        dplyr::mutate(temp_group_size = n()) %>%
        dplyr::filter(! (temp_group_size > 1 & fuel_grade == "Premium")) %>%
        dplyr::select(-temp_group_size, -fuel_grade) %>%
        dplyr::rename(trans_id = transmission_id) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(fuel_type = dplyr::if_else(
            fuel_type == "Gasoline (Mid Grade Unleaded Required)", 'Gasoline', fuel_type),
            fuel_type = dplyr::if_else(fuel_type == "CNG", "Natural Gas", fuel_type)) %>%
        # ensure_id_vars(vehicle_id, engine_id, trans_id, fuel_type)
        ensure_id_vars(vehicle_id, engine_id, trans_id, fuel_type)
    return(df)
}


.load_def_transmission <- function() {
    # Load transmission info (required for MPG merge)
    csv_filename <- file.path(CSV_DIR, 'DEF_TRANSMISSION.csv')
    load_cols <- cols_only(
        trans_id = col_integer(),
        # trans_name = col_character(),
        # trans_brand = col_character(),
        trans_type = col_character()
        # trans_detail_type = col_character(),
        # trans_gears = col_integer()
    )
    mpg_df <- .load_lkp_veh_mpg()
    df <- readr::read_csv(csv_filename, col_types = load_cols, progress = FALSE) %>%
        ensure_id_vars(trans_id) %>%
        right.join(mpg_df, by = 'trans_id') %>%
        dplyr::group_by(vehicle_id, engine_id, fuel_type) %>%
        # within groups defined by the variables above, if there are duplicates,
        # keep only the automatic transmission
        dplyr::mutate(temp_group_size = n()) %>%
        dplyr::filter(! (temp_group_size > 1 & trans_type == "A")) %>%
        dplyr::select(vehicle_id, engine_id, fuel_type, city, highway, combined) %>%
        # Collapse by these variables, casewise.  Equivalent Stata would be:
        # collapse (mean) city highway combined, by(vehicle_id engine_id fuel_type) cw
        dplyr::group_by(vehicle_id, engine_id, fuel_type) %>%
        na.omit() %>%
        dplyr::summarize_all(mean) %>%
        ensure_id_vars(vehicle_id, engine_id, fuel_type)
    return(df)
}


merge_files <- function() {
    vin_reference <- .load_vin_reference()
    veh_price <- .load_veh_price() %>% ensure_id_vars(vehicle_id)
    veh_trans_mpg <- .load_def_transmission() %>%
        ensure_id_vars(vehicle_id, engine_id, fuel_type)
    not_na <- Negate(is.na)
    df <- inner.join(vin_reference, veh_price, by = 'vehicle_id') %>%
        full.join(veh_trans_mpg, by=c('vehicle_id', 'engine_id', 'fuel_type')) %>%
        dplyr::filter(not_na(city), not_na(highway), not_na(combined), not_na(msrp),
                      not_na(vehicle_type), not_na(fuel_type), not_na(model_yr),
                      not_na(vin_pattern)) %>%
        dplyr::group_by(vin_pattern) %>%
        # But it turns out vin_pattern doesn't uniquelly identify rows,
        # so we'll need to collapse down.
        # allow for random ordering to avoid bias if there's something weird about the
        # row ordering and they don't all have the same model_yr/fuel_type.
        shuffle() %>%
        dplyr::summarize(model_yr = first(model_yr),
                         fuel_type = first(fuel_type),
                         city = first(city),
                         highway = first(highway),
                         combined  = first(combined),
                         vehicle_type  = first(vehicle_type),
                         # max because we're using it to filter auction prices later
                         msrp = max(msrp))
    return(df)
}


main <- function(verbose = TRUE) {
    vin_decoder <- merge_files()
    # vin_decoder <- haven::read_dta(file.path())
    pg_user <- Sys.info()[["user"]] %>% tolower()
    con <- dbConnect("PostgreSQL", dbname = POSTGRES_DB,
                     user = pg_user, password = pg_user)
    if (DBI::dbExistsTable(con, POSTGRES_TABLE)) {
        if (verbose) {
            message(sprintf('Deleting existing table: %s', POSTGRES_TABLE))
        }
        DBI::dbRemoveTable(con, POSTGRES_TABLE)
    }

    # returns TRUE if successful
    successful_write <- DBI::dbWriteTable(con, POSTGRES_TABLE, vin_decoder,
                                          row.names = FALSE)
    stopifnot(successful_write)
    pg_add_primary_key(con, POSTGRES_TABLE, 'vin_pattern')
    DBI::dbDisconnect(con)
}


# Run things:
main()
