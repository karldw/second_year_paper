#!/bin/bash

set -euf -o pipefail

# See directions in http://mirrors.ctan.org/macros/latex/contrib/gitinfo2/gitinfo2.pdf

cp -pi post-checkout  ../../.git/hooks/
cp -pi post-commit    ../../.git/hooks/
cp -pi post-merge     ../../.git/hooks/
