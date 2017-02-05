
# generate_text_snippets.r
# This file runs queries and generates some of the numbers to be \include{}-ed in the
# LaTeX text.
# If the files already exist, it doesn't regenerate them, so if you make changes, clear
# them out!

source('r_defaults.r')
install_lazy(c('dplyr', 'RPostgreSQL'), verbose = FALSE)
suppressPackageStartupMessages(library(dplyr))

POSTGRES_DB <- 'second_year_paper'
POSTGRES_CLEAN_TABLE <- 'auctions_cleaned'
if (! exists('con')) {
    pg_user <- Sys.info()[["user"]] %>% tolower()
    con <- src_postgres(POSTGRES_DB, user = pg_user, password = pg_user)
}
# orig_data <- tbl(con, POSTGRES_ORIG_TABLE)
auctions <-  tbl(con, POSTGRES_CLEAN_TABLE)
# states <- tbl(con, 'states')

# Total auction count
auctions %>% count() %>%
    make_snippet('auctions_cleaned_total_obs_count.tex', sig_figs = 20)

# Alaskan buy count:
auctions %>% filter(buy_state == 'AK') %>% count() %>%
    make_snippet('auctions_cleaned_alaska_obs_count.tex', sig_figs = 20)
