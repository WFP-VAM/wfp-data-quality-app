# Data Quality Check

## Overview

This is a Shiny application package for high frequency check for food security surveys data quality
and visualization of the trend on the key food security indicators. This app serves for enumerator performance checking and flagging issue.
This is a support and joint effort from WACARO RAM Team.

## Features

- Upload data in SPSS format (local format) or directly from MoDa datasets  up to 500MB
- Analyze food security indicators:
  - Food Consumption Score (FCS)
  - Household Dietary Diversity Score (HDDS)
  - Reduced Coping Strategy Index (rCSI)
  - Household Hunger Scale (HHS)
  - FewsNet Matrix Table
  - Livelihood Coping Strategy Index (LCS)
  - Food Expenditure Share (FES)
  - Consolidated Approach for Reporting Indicators of Food Security (CARI)
- View results by administrative levels (Admin1, Admin2) and by enumerator
- Track survey progress with submission vs planned
- Generate comprehensive HTML reports with visualizations

## Installation

### Prerequisites

- **R Version**: R >= 4.4.3 (as specified in DESCRIPTION)
- **System Requirements**: Sufficient RAM to handle datasets up to 500MB
- **Internet Connection**: Required for MoDa API integration and package installation

### Step-by-Step Installation

1. **Install R and RStudio** (if not already installed):
   - Download R from [CRAN](https://cran.r-project.org/)
   - Download RStudio from [Posit](https://posit.co/downloads/)

2. **Install required system dependencies**:
   ```r
   # Install devtools if not already installed
   if (!requireNamespace("devtools", quietly = TRUE)) {
     install.packages("devtools")
   }
   ```

3. **Clone and install the package**:
   ```bash
   # Clone the repository locally
   git clone https://github.com/WFP-VAM/wfp-data-quality-app.git
   cd wfp-data-quality-app
   ```

4. **Launch RStudio from the project directory**:
   ```bash
   # Open RStudio from the current directory
   open wfp-data-quality-app.Rproj    # On macOS
   # OR
   rstudio wfp-data-quality-app.Rproj  # On Linux/Windows
   # OR simply double-click the .Rproj file in your file manager
   ```

5. **Install the package** (in RStudio):
   ```r
   # Install the package with dependencies
   devtools::install(".", dependencies = TRUE)
   ```

6. **Launch the application**:
   ```r
   # Load the library
   library(wfp.data.quality.app)
   
   # Launch the application
   run_app()
   ```

### Dependencies

The package automatically installs the following dependencies:
- **Core Shiny**: `shiny`, `shinydashboard`
- **Data Processing**: `dplyr`, `tidyverse`, `haven`, `labelled`
- **Visualization**: `ggplot2`, `plotly`, `treemapify`
- **Statistical Analysis**: `rstatix`
- **Data Export**: `writexl`, `openxlsx`, `DT`, `kableExtra`
- **API Integration**: `httr`, `jsonlite`
- **Other**: `lubridate`, `forcats`, `rlang`, `rmarkdown`, `htmltools`, `purrr`, `tidyr`, `glue`, `scales`, `devtools`


### Development Mode

For developers who want to modify the package:

```r
# Load the package in development mode (without installing)
devtools::load_all(".")

# Or use R CMD for traditional installation
R CMD INSTALL .
```

## Requirements

The application requires datasets with standard variable names from survey designer.

## Contact

For questions or support, please contact:
- Aliou Badara SAMAKE: samakealioubadara@gmail.com or alioubadara.samake@wfp.org

[![R-CMD-check](https://github.com/WFP-VAM/wfp-survey-data-quality-app/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/WFP-VAM/wfp-survey-data-quality-app/actions)
