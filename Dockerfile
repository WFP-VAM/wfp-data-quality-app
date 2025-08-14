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
    cmake \
    && rm -rf /var/lib/apt/lists/*

# Install renv for reproducible package management
RUN R -e "install.packages('renv')"

# Copy renv configuration files first (for optimal Docker layer caching)
# This allows package restoration to be cached separately from source code changes
COPY renv.lock /srv/shiny-server/wfp-data-quality-app/
COPY renv/activate.R /srv/shiny-server/wfp-data-quality-app/renv/
COPY .Rprofile /srv/shiny-server/wfp-data-quality-app/
COPY DESCRIPTION /srv/shiny-server/wfp-data-quality-app/

# Set working directory for renv operations
WORKDIR /srv/shiny-server/wfp-data-quality-app

# Restore R packages from renv.lock (reproducible package versions)
# This installs exact versions specified in lockfile, replacing manual package list
RUN R -e "renv::restore()"

# Copy the application source code
COPY R/ /srv/shiny-server/wfp-data-quality-app/R/
COPY inst/ /srv/shiny-server/wfp-data-quality-app/inst/

# Install our WFP app as an R package using devtools
# Dependencies already installed via renv::restore()
RUN R -e "devtools::install('.', dependencies = FALSE)"

# Return to shiny server working directory
WORKDIR /srv/shiny-server

# Copy the main Shiny app files to the root directory from the already-copied location
# Shiny Server will automatically serve app.R from /srv/shiny-server/
# This makes the app accessible at http://localhost:3838
RUN cp /srv/shiny-server/wfp-data-quality-app/inst/app/app.R /srv/shiny-server/app.R
RUN cp /srv/shiny-server/wfp-data-quality-app/inst/app/report.Rmd /srv/shiny-server/report.Rmd

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