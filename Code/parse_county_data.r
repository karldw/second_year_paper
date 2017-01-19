

source('r_defaults.r')
suppressPackageStartupMessages(library(dplyr))
library(readr)
library(reshape2)

try(
SOURCE_DIR <- file.path(box_home()[1],
                     'second_year_paper_data/County_populations')
, silent = TRUE
)
if (! (exists('SOURCE_DIR') && dir.exists(SOURCE_DIR))) {
    SOURCE_DIR <- '~/Everything_else/Box_local/second_year_paper_data/County_populations'
}
stopifnot(dir.exists(SOURCE_DIR))

load_one_decade <- function(decade) {
    stopifnot(length(decade) == 1)
    if (decade == 2000) {
        csv_basename <- 'CO-EST00INT-TOT.csv'
    } else if (decade == 2010) {
        csv_basename <- 'CO-EST2015-alldata.csv'
    } else {
        stop('Decade must be 2000 or 2010.')
    }
    csv_filename <- file.path(SOURCE_DIR, csv_basename)
    stopifnot(file.exists(csv_filename))

    # Unnamed columns don't get read
    # (see the pdf documentation in the csv directory for the full set of variables)
    if (decade == 2000) {
        select_cols <- cols_only(
            SUMLEV = col_integer(),
            STATE = col_integer(),
            COUNTY = col_integer(),
            STNAME = col_character(),
            CTYNAME = col_character(),
            POPESTIMATE2000 = col_integer(),
            POPESTIMATE2001 = col_integer(),
            POPESTIMATE2002 = col_integer(),
            POPESTIMATE2003 = col_integer(),
            POPESTIMATE2004 = col_integer(),
            POPESTIMATE2005 = col_integer(),
            POPESTIMATE2006 = col_integer(),
            POPESTIMATE2007 = col_integer(),
            POPESTIMATE2008 = col_integer(),
            POPESTIMATE2009 = col_integer()
            # take 2010 from both because a few states don't have 2010 data in the newer.
            # POPESTIMATE2010 = col_integer()
        )
    } else if (decade == 2010) {
        select_cols <- cols_only(
            SUMLEV = col_integer(),
            STATE = col_integer(),
            COUNTY = col_integer(),
            STNAME = col_character(),
            CTYNAME = col_character(),
            POPESTIMATE2010 = col_integer(),
            POPESTIMATE2011 = col_integer(),
            POPESTIMATE2012 = col_integer(),
            POPESTIMATE2013 = col_integer(),
            POPESTIMATE2014 = col_integer(),
            POPESTIMATE2015 = col_integer()
        )
    }
    df <- read_csv(csv_filename, col_types=select_cols) %>%
        setNames(tolower(names(.))) %>%
        filter(sumlev == 50) %>%  # counties, not states and counties
        select(-sumlev) %>%
        # add leading zeros with with sprintf
        # see http://stackoverflow.com/a/5816779
        mutate(state  = sprintf("%02d", state),
               county = sprintf("%03d", county)) %>%
        melt(id.vars=c('state', 'county', 'stname', 'ctyname'),
             variable.name='year', value.name='population') %>%
        as.tbl() %>%
        mutate(year = as.integer(gsub('.*(\\d{4}).*', '\\1', year, perl=TRUE))) %>%
        ensure_id_vars(stname, ctyname, year) %>%
        ensure_id_vars(state, county, year)
    expected_state_by_year <- length(unique(df$state)) * length(unique(df$year))
    actual_state_by_year <- distinct(df, state, year) %>% nrow()
    stopifnot(expected_state_by_year == actual_state_by_year)

    return(df)
}

main <- function() {
    combined_df <- bind_rows(load_one_decade(2000), load_one_decade(2010)) %>%
        distinct(state, county, year, .keep_all=TRUE) %>%
        ensure_id_vars(stname, ctyname, year)
    out_dir <- '../Data'
    stopifnot(dir.exists(out_dir))
    saveRDS(combined_df, file.path(out_dir, 'us_county_by_year_population.rda'))
}


main()
