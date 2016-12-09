

install_lazy(c('memoise', 'rvest', 'dplyr', 'assertr', 'magrittr', 'lubridate', 'readr', 'ensurer', 'ggplot2'), verbose = FALSE)
library(memoise)
library(rvest)
library(dplyr)
library(magrittr)
library(assertr)
library(ensurer)
library(lubridate)
library(readr)
library(ggplot2)

if (! memoise::is.memoised(read_html)) {
    read_html <- memoise::memoise(xml2::read_html)
}


PLOT_THEME <- theme(panel.background = element_rect(fill = NA),
                    panel.border = element_rect(fill = NA, color = 'black'),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.ticks = element_line(color = 'gray5'),
                    axis.text = element_text(color = 'black', size = 10),
                    axis.title = element_text(color = 'black', size = 12),
                    legend.key = element_blank())


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


load_cpi <- function() {
    # Downloaded from FRED, series CPIAUCSL
    # https://fred.stlouisfed.org/series/CPIAUCSL#
    filename <- '../Data/CPI/CPIAUCSL_1947_01_01-2016-09-01.csv'
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
    df <- mutate(df, cpi_base_2016 = cpi_year_mean / cpi_adj_2016) %>% select(-cpi_year_mean)
    return(df)
}


adjust_pfd_payments <- function() {
    # In 2008 there was a bonus paid to every APF recipient
    # bill: http://www.legis.state.ak.us/basis/get_bill_text.asp?hsid=SB4002A&session=25
    bonus_2008 <- 1200
    pfd_payments <- download_summary_table() %>%
        mutate(n_ = total_disbursed / amount,
               total_disbursed = if_else(year == 2008, total_disbursed + bonus_2008 * n_,
                                        total_disbursed),
               amount = if_else(year == 2008, amount + bonus_2008, amount))

    cpi <- load_cpi()

    merged <- left_join(pfd_payments, cpi, by='year') %>%
        ensure(! anyNA(.$cpi_base_2016)) %>%
        mutate(amount_2016dollars = amount / cpi_base_2016,
               total_disbursed_2016dollars = total_disbursed / cpi_base_2016) %>%
        select(-cpi_base_2016)
    return(merged)
}


plot_payments <- function() {
    payments <- adjust_pfd_payments()
    plt_individual <- ggplot(payments, aes(x=year, y=amount_2016dollars)) +
        geom_bar(stat='identity') +
        labs(x='', y='2016 dollars', title='Alaska Permanent Fund payments, per individual') +
        PLOT_THEME
    plt_individual_notitle <- ggplot(payments, aes(x=year, y=amount_2016dollars)) +
        geom_bar(stat='identity') +
        labs(x='', y='2016 dollars', title='') +
        PLOT_THEME

    plt_aggregate <- ggplot(payments, aes(x=year, y=total_disbursed_2016dollars/10^6)) +
        geom_bar(stat='identity') +
        labs(x='', y='Millions of 2016 dollars', title='Alaska Permanent Fund payments, state total') +
        PLOT_THEME

    plot_dir <- '../Text/Plots'
    stopifnot(dir.exists(plot_dir))

    file.path(plot_dir, 'permanent_fund_payments_individual.pdf') %>%
    ggsave(plt_individual, width=6.3, height=3.54)

    file.path(plot_dir, 'permanent_fund_payments_individual_notitle.pdf') %>%
    ggsave(plt_individual_notitle, width=6.3, height=3.54)

    file.path(plot_dir, 'permanent_fund_payments_aggregate.pdf') %>%
    ggsave(plt_aggregate, width=6.3, height=3.54)
}

x <- adjust_pfd_payments()
plot_payments()
