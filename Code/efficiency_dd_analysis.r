source('r_defaults.r')
# library(RPostgreSQL)
install_lazy(c('dplyr', 'ggplot2', 'magrittr', 'lfe', 'memoise', 'lubridate'), verbose = FALSE)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
library(magrittr)
suppressPackageStartupMessages(library(lfe))
library(memoise)

POSTGRES_DB <- 'second_year_paper'
POSTGRES_CLEAN_TABLE <- 'auctions_cleaned'
POSTGRES_VIN_DECODER_TABLE <- 'vin_decoder'
if (! exists('con')) {
    con <- src_postgres(POSTGRES_DB)
}
auctions <-  tbl(con, POSTGRES_CLEAN_TABLE)
states <- tbl(con, 'states')
vin_decoder <- tbl(con, POSTGRES_VIN_DECODER_TABLE)

if (! exists('auctions_with_mpg')) {
    # auctions_with_mpg <- 
}
