
library(memoise)
library(rvest)
library(dplyr)
library(magrittr)
library(assertr)
library(lubridate)
if (! memoise::is.memoised(read_html)) {
    read_html <- memoise::memoise(xml2::read_html)
}

download_summary_table <- function(){
    url <- 'https://pfd.alaska.gov/Division-Info/Summary-of-Applications-and-Payments'

    expected_names <- c("Dividend\n            Year",
                        "State \n            Population",
                        "Dividend Applications",
                        "Dividend Applications" ,
                        "Dividend\n            Amount",
                        "% Change from\n            Prior Year",
                        "Total \n            Disbursed Amount",
                        "Total \n            Disbursed Amount" )
    new_names <- c('year', 'state_pop', 'apps_received', 'apps_paid',
                   'amount', 'pct_change', 'total_disbursed', 'total_disbursed2')

    as_numeric_smart <- function(x){
        x_cleaned <- gsub(pattern = '[,\\$%]', replacement = '',
                          x = x, perl = TRUE)
        return(as.numeric(x_cleaned))
    }

    df <- read_html(url) %>%  # download url
        html_table %>%  # magic convert to table
        extract2(1) %>%  # take the first table from the list (ensure2 == `[[`)
        verify(all(names(.) == expected_names)) %>%  # check columns before rename
        set_colnames(new_names) %>%
        as.tbl %>%
        select(-pct_change, -total_disbursed2) %>%
        slice(., c(-1, -nrow(.)))  %>%  # first and last rows aren't data
        mutate_all(as_numeric_smart)
    return(df)
}
