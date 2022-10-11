# SmartLYT docker file
FROM rocker/shiny

# system libraries
RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    make \
    libcurl4-openssl-dev \
    libcairo2-dev \
    libssl-dev \
    zlib1g-dev \
    libxml2-dev \
    libudunits2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libicu-dev \
    libgdal-dev \
    gdal-bin \
    libgeos-dev \
    libproj-dev \
    pandoc \
    libpng-dev \ 
    && rm -rf /var/lib/apt/lists/*

RUN install2.r --error --skipinstalled \
    shiny \
    shinyjs \ 
    here \
    tidyverse \
    ggiraph \
    sf \
    terra \
    mapdeck