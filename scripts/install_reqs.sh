#!/bin/bash

set -e

## build ARGs
NCPUS=${NCPUS:--1}

# a function to install apt packages only if they are not installed
function apt_install() {
    if ! dpkg -s "$@" >/dev/null 2>&1; then
        if [ "$(find /var/lib/apt/lists/* | wc -l)" = "0" ]; then
            apt-get update
        fi
        apt-get install -y --no-install-recommends "$@"
    fi
}

apt_install \
    libgdal-dev \
    make \
    default-jdk

install2.r --error --skipinstalled -n "$NCPUS" \
    RColorBrewer \
    janitor \
    jsonlite \
    leaflet \
    randomcoloR \
    rJava \
    rpivotTable \
    scales \
    shiny \
    shinydashboard \
    shinyjs \
    shinythemes \
    shinyWidgets \
    tidyverse \
    xlsx

## a bridge to far? -- brings in another 60 packages
# install2.r --error --skipinstalled -n "$NCPUS" tidymodels

# Clean up
rm -rf /var/lib/apt/lists/*
rm -rf /tmp/downloaded_packages

## Strip binary installed lybraries from RSPM
## https://github.com/rocker-org/rocker-versioned2/issues/340
strip /usr/local/lib/R/site-library/*/libs/*.so

# Check the igraph version
echo -e "Check the rJava package...\n"

R -q -e "library(rJava)"

echo -e "\nInstall rJava package, done!"