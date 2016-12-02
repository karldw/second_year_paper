source('r_defaults.r')
library(dplyr)
library(ggplot2)

POSTGRES_DB <- 'second_year_paper'
auctions <- src_postgres(POSTGRES_DB)
# Previously (in clean_car_auctions.r), I've made and indexd a "rand" variable
# take a 5% sample (that's 5% in expectation; it might vary slightly)
auctions_sample <- filter(auctions, rand < 0.05) %>% collect()
