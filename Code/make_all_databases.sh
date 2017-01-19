#!/bin/bash
# make_all_databases.sh

set -euf -o pipefail

Rscript download_state_data.r
Rscript parse_county_data.r
Rscript load_manheim_into_postgres.r
Rscript load_states_into_postgres.r
Rscript load_vin_decoder_into_postgres.r
Rscript load_zipcodes_into_postgres.r
Rscript clean_car_auctions.r
