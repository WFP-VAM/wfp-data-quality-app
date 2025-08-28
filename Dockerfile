# Stage 1: Base with system dependencies and renv
FROM rocker/shiny:4.4.3 AS base

# Install system dependencies (Ubuntu packages) required for R packages
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

# Create app directory structure
RUN mkdir -p /srv/shiny-server/wfp/WfpDataQualityApp
WORKDIR /srv/shiny-server/wfp/WfpDataQualityApp

# Stage 2: Application dependencies
# Using a multi-stage build here ensures the R packages installation step is cached and only re-run if renv.lock changes
FROM base AS dependencies

# Copy only renv configuration files (dependency definition)
COPY renv.lock ./
COPY renv/activate.R ./renv/
COPY .Rprofile ./

# Configure renv settings
RUN R -e "renv::settings\$use.cache(FALSE)"

# Restore R packages - this layer is cached unless renv.lock changes
RUN R -e "renv::restore()"

# Stage 3: Runtime - copy application code and configure server
FROM base AS runtime

# Copy activation files and renv environment and from the dependencies stage
COPY --from=dependencies /srv/shiny-server/wfp/WfpDataQualityApp/.Rprofile ./
COPY --from=dependencies /srv/shiny-server/wfp/WfpDataQualityApp/renv ./renv

# Copy application code
COPY inst/app/ ./

# Return to shiny server working directory
WORKDIR /srv/shiny-server

# Copy custom Shiny Server configuration
# This overrides default settings (timeouts, logging, etc.)
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Create directories and set ownership for the 'shiny' user
# Shiny Server runs as 'shiny' user for security
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