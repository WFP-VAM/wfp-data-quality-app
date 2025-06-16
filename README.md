# Data Quality Check

## Overview

This is a Shiny application package for high frequency check for food security surveys data quality
and visualization of the trend on the key food security indicators. This app serves for enumerator performance checking and flagging issue.
This is a support and joint effort from WACARO RAM Team.

## Features

- Upload data in SPSS format (local format) or directly from MoDa datasets  up to 200MB
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

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("aliou-badara/myshinyapp")
```

## Usage

```r
library(myshinyapp)

# Launch the application
run_app()
```

## Requirements

The application requires datasets with standard variable names. For more information on required variables, please see the in-app documentation or refer to [this guide](https://wfp-vam.github.io/RBD_FS_CH_guide_EN/combined-questionnaire-syntaxes-for-all-5-indicators.html).

## Contact

For questions or support, please contact:
- Aliou Badara SAMAKE: samakealioubadara@gmail.com or alioubadara.samake@wfp.org

[![R-CMD-check](https://github.com/aliou-badara/myshinyapp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aliou-badara/myshinyapp/actions)
