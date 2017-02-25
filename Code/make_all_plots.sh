#!/bin/bash
# make_all_plots.sh
# Run in linux, or on Windows via git bash.

set -euf -o pipefail

Rscript get_alaska_data.r
Rscript basic_dd_analysis.r
Rscript efficiency_dd_analysis.r
