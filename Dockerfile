# Use the official R Shiny image as base
# This includes R, Shiny Server, and Ubuntu OS pre-configured
FROM rocker/shiny:4.4.3

# Set working directory to Shiny Server's default app location
# All Shiny apps are served from /srv/shiny-server/
WORKDIR /srv/shiny-server

# Install system dependencies (Ubuntu packages) required for R packages
# These C/C++ libraries are needed to compile certain R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

# Install R packages from RStudio Package Manager (faster binary installs)
# These are all the R packages our Shiny app depends on
# Using 4 CPU cores (Ncpus=4) to speed up installation
RUN R -e "options(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/__linux__/noble/latest')); \
    install.packages(c('shiny', 'shinydashboard', 'dplyr', 'ggplot2', 'tidyverse', 'haven', 'labelled', 'plotly', 'rstatix', 'kableExtra', 'lubridate', 'DT', 'forcats', 'writexl', 'rlang', 'rmarkdown', 'openxlsx', 'htmltools', 'purrr', 'tidyr', 'treemapify', 'glue', 'scales', 'httr', 'jsonlite', 'tools', 'knitr', 'devtools'), Ncpus = 4)"

# Copy the entire WFP application source code into container
# This includes DESCRIPTION, R/, inst/, etc. - the full R package structure
COPY . /srv/shiny-server/wfp-data-quality-app/

# Install our WFP app as an R package using devtools
# This makes functions and data available to the Shiny app
RUN R -e "devtools::install('/srv/shiny-server/wfp-data-quality-app', dependencies = TRUE)"

# Copy the main Shiny app files to the root directory
# Shiny Server will automatically serve app.R from /srv/shiny-server/
# This makes the app accessible at http://localhost:3838
COPY inst/app/app.R /srv/shiny-server/app.R
COPY inst/app/report.Rmd /srv/shiny-server/report.Rmd

# Copy custom Shiny Server configuration
# This overrides default settings (timeouts, logging, etc.)
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Create directories and set ownership for the 'shiny' user
# Shiny Server runs as 'shiny' user for security
# Logs will be written to /var/log/shiny-server/
RUN mkdir -p /var/log/shiny-server && \
    chown -R shiny:shiny /srv/shiny-server && \
    chown -R shiny:shiny /var/log/shiny-server

# Expose port 3838 for web access
EXPOSE 3838

# Start Shiny Server as main process (PID 1)
# This will serve the app at http://localhost:3838
# Reads config from /etc/shiny-server/shiny-server.conf
# Serves apps from /srv/shiny-server/
CMD ["/usr/bin/shiny-server"]