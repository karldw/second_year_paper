
source('r_defaults.r')
install_lazy(c('memoise', 'rvest', 'dplyr', 'assertr', 'magrittr', 'lubridate', 'readr', 'ensurer', 'ggplot2'), verbose = FALSE)
library(memoise)
suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(dplyr))
library(magrittr)
library(assertr)
library(ensurer)
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(readr))
library(ggplot2)
DATA_DIR <- '../Data'
SAVED_DATA <- file.path(DATA_DIR, 'permanent_fund_payments.rda')
SAVED_DATA_ADJUSTED <- file.path(DATA_DIR, 'permanent_fund_payments_adjusted.rda')
stopifnot(dir.exists(DATA_DIR))
if (! memoise::is.memoised(read_html)) {
    read_html <- memoise::memoise(xml2::read_html)
}


# PLOT_THEME <- theme(panel.background = element_rect(fill = NA),
#                     panel.border = element_rect(fill = NA, color = 'black'),
#                     panel.grid.major = element_blank(),
#                     panel.grid.minor = element_blank(),
#                     axis.ticks = element_line(color = 'gray5'),
#                     axis.text = element_text(color = 'black', size = 10),
#                     axis.title = element_text(color = 'black', size = 12),
#                     legend.key = element_blank())


download_summary_table <- function(){
    url <- 'https://pfd.alaska.gov/Division-Info/Summary-of-Applications-and-Payments'

    expected_names <- c("DividendYear",
                        "StatePopulation",
                        "Dividend Applications",
                        "Dividend Applications" ,
                        "DividendAmount",
                        "% Change fromPrior Year",
                        "TotalDisbursed Amount",
                        "TotalDisbursed Amount" )
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
        mutate_all(as_numeric_smart) %>%
        readr::write_rds(SAVED_DATA, compress = 'gz')
    return(df)
}


load_summary_table <- function(must_download = FALSE) {
    if (must_download || (! file.exists(SAVED_DATA))) {
        df <- download_summary_table()
    } else {
        df <- readr::read_rds(SAVED_DATA)
    }
    return(df)
}


load_cpi <- function() {
    # Downloaded from FRED, series CPIAUCSL
    # https://fred.stlouisfed.org/series/CPIAUCSL#
    filename <- file.path(DATA_DIR, 'CPI/CPIAUCSL_1947_01_01-2016-09-01.csv')
    stopifnot(file.exists(filename))

    col_spec <- cols(
        DATE = col_date(format = ""),
        CPIAUCSL = col_double()
    )

    df <- read_csv(filename, col_types = col_spec) %>%
        setNames(tolower(names(.))) %>%
        mutate(year = year(date)) %>%
        group_by(year) %>%
        summarize(cpi_year_mean = mean(cpiaucsl))

    cpi_adj_2016 <- filter(df, year == 2016) %$% cpi_year_mean
    mutate(df, cpi_base_2016 = cpi_year_mean / cpi_adj_2016) %>%
        select(-cpi_year_mean) %>%
        return()
}


adjust_pfd_payments <- function() {
    # In 2008 there was a bonus paid to every APF recipient
    # bill: http://www.legis.state.ak.us/basis/get_bill_text.asp?hsid=SB4002A&session=25
    bonus_2008 <- 1200
    pfd_payments <- load_summary_table() %>%
        mutate(n_ = total_disbursed / amount,
               total_disbursed = if_else(year == 2008, total_disbursed + bonus_2008 * n_,
                                        total_disbursed),
               amount = if_else(year == 2008, amount + bonus_2008, amount))

    cpi <- load_cpi()

    merged <- left.join(pfd_payments, cpi, by = 'year') %>%
        ensure(! anyNA(.$cpi_base_2016)) %>%
        mutate(amount_2016dollars = amount / cpi_base_2016,
               total_disbursed_2016dollars = total_disbursed / cpi_base_2016) %>%
        select(-cpi_base_2016)
    return(merged)
}


plot_payments <- function() {
    payments <- adjust_pfd_payments()
    no_margin <- theme(plot.margin = margin(t = 0, r = 0.2, b = 0, l = 0.1, unit = "cm"))
    plt_individual <- ggplot(payments, aes(x = year, y = amount_2016dollars)) +
        geom_bar(stat = 'identity') +
        labs(x = '', y = '2016 dollars',
             title = 'Alaska Permanent Fund payments, per individual') +
        PLOT_THEME + no_margin
    plt_individual_notitle <- ggplot(payments, aes(x = year, y = amount_2016dollars)) +
        geom_bar(stat = 'identity') +
        labs(x = '', y = '2016 dollars', title = '') +
        PLOT_THEME + no_margin

    plt_aggregate <- ggplot(payments,
        aes(x = year, y = total_disbursed_2016dollars / 10^6)) +
        geom_bar(stat = 'identity') +
        labs(x = '', y = 'Millions of 2016 dollars',
             title = 'Alaska Permanent Fund payments, state total') +
        PLOT_THEME + no_margin

    save_plot(plt_individual, 'permanent_fund_payments_individual.pdf')
    save_plot(plt_individual_notitle, 'permanent_fund_payments_individual_notitle.pdf')
    save_plot(plt_aggregate, 'permanent_fund_payments_aggregate.pdf')
}

adjust_pfd_payments() %>% readr::write_rds(SAVED_DATA_ADJUSTED, compress = 'gz')
plot_payments()
