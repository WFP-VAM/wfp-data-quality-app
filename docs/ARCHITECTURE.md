# WFP Survey Data Quality App - Architecture Overview

## Overview
This document provides a high-level architectural overview of the WFP Data Quality App, helping developers understand the modular codebase structure and locate key implementation areas.

## Application Structure

### Framework & Package Management
- **Package Management**: `renv` for reproducible dependency management
  - `renv.lock`: Contains exact versions of 200+ packages
  - `.Rprofile`: Automatically activates renv on project load
- **Main Application Entry**: `inst/app/app.R` (39 lines - library imports and app launch)
- **Package Entry Point**: `R/run_app.R` 
- **Report Template**: `inst/app/report.Rmd`

*Note: The `inst/` folder contains files that are installed with the package and accessible via `system.file()` at runtime.*

## Modular Architecture

### Core Application Files
```
inst/app/
├── app.R          # Main entry point (39 lines)
├── ui.R           # UI structure and module loading (50 lines)  
├── server.R       # Server logic (3,783 lines)
├── report.Rmd     # Report template
└── www/
    └── custom.css # Styling (70 lines)
```

### UI Module Structure
```
inst/app/modules/ui/
├── homeUI.R           # Home page with guidance (66 lines)
├── dataUploadUI.R     # Data upload & MoDa API (111 lines)
├── surveyProgressUI.R # Survey monitoring (105 lines)
├── fcsUI.R           # Food Consumption Score (173 lines)
├── hddsUI.R          # Household Dietary Diversity (78 lines)
├── rcsiUI.R          # Reduced Coping Strategy Index (172 lines)
├── hhsUI.R           # Household Hunger Scale (77 lines)
├── lcsUI.R           # Livelihood Coping Strategies (177 lines)
├── fesUI.R           # Food Expenditure Share (141 lines)
├── cariUI.R          # CARI indicators (88 lines)
├── matrixUI.R        # FewsNet Matrix (30 lines)
└── reportUI.R        # Report generation (19 lines)
```

## UI Architecture

### Dashboard Structure
- **Framework**: `shinydashboard` with header, sidebar, and body
- **Navigation**: 12 main tabs using modular UI functions
- **Styling**: CSS file located at `www/custom.css`
- **Namespace Implementation**: Each module uses `NS(id)` for ID isolation

### UI Loading Pattern
```r
# ui.R dynamic module loading
my_path <- c("modules/ui/")
source_files <- list.files(my_path, "*.R$")
map(paste0(my_path, source_files), source)

# Dashboard structure
dashboardPage(
  dashboardHeader(title = "WFP Data Quality Check"),
  dashboardSidebar(sidebarMenu(...)),
  dashboardBody(
    tags$head(includeCSS("www/custom.css")),
    tabItems(
      tabItem(tabName = "home", homeUI("home")),
      tabItem(tabName = "upload", dataUploadUI("upload")),
      # ... other modular tabs
    )
  )
)
```

### Namespace Architecture
Each UI module implements namespacing:
```r
# Pattern used across all UI modules
moduleUI <- function(id) {
  ns <- NS(id)
  tagList(
    # UI elements automatically namespaced
  )
}
```

## Server Architecture

### Core Server File (`server.R`)
- **Size**: 3,783 lines 
- **Structure**: All server-side reactive logic and data processing
- **Integration**: Works with namespaced UI modules

### Data Flow Core Functions

#### 1. Data Ingestion
```r
# SPSS file upload handling
spss_data <- reactive({ ... })

# MoDa API integration  
moda_data <- eventReactive({ ... })

# Unified data source
dataset <- reactive({ ... })
```

#### 2. Data Processing
```r
# Main filtered dataset
reqData <- reactive({ ... })

# Administrative filters with namespace support
# Filter reactives integrated throughout server logic
```

#### 3. Indicator Calculations
Each food security indicator has dedicated calculation functions in `server.R`:
- **FCS**: Food Consumption Score analysis (Lines 361-377)
- **HDDS**: Household Dietary Diversity Score (Lines 380-438)
- **rCSI**: Reduced Coping Strategy Index (Lines 441-452)
- **HHS**: Household Hunger Scale (Lines 455-495)
- **FES**: Food Expenditure Share (Lines 498-590)
- **LCS**: Livelihood Coping Strategies (Lines 597-673)
- **CARI**: Consolidated Approach for Reporting Indicators (Lines 676-764)

## Package Management

### renv Workflow
```r
renv::restore()              # Install exact package versions from renv.lock
devtools::install(".")       # Install app package
wfp.data.quality.app::run_app()  # Launch application
```

### Development Setup
```r
# Load the package in development mode
devtools::load_all(".")
```

## Data Integration Points

### SPSS Files
```r
# File upload handling
input$spss_file
haven::read_sav()        # SPSS file reading
# Expected: 80+ standardized variables
```

### MoDa API
```r
# API workflow  
httr::POST()            # Export job creation
httr::GET()             # Status polling
# Handles: Authentication, async exports, ZIP extraction
```

## Configuration

### File Upload Limits
- **Application Limit**: 500MB (`options(shiny.maxRequestSize = 500 * 1024^2)`)
- **Configuration Location**: `inst/app/modules/ui/homeUI.R:3`

### Styling
- **CSS Location**: `inst/app/www/custom.css`
- **WFP Branding**: Custom colors, fonts (Open Sans), and component styling
- **Responsive Design**: Bootstrap-based layout with custom overrides

## Key Components

### UI Modules
Each UI module handles a specific section of the application:
- **homeUI**: Application guidance and variable requirements
- **dataUploadUI**: File upload and MoDa API integration
- **surveyProgressUI**: Survey monitoring and progress tracking
- **Indicator modules** (fcsUI, hddsUI, etc.): Individual food security indicators
- **reportUI**: Report generation interface

### Server Logic
- **Reactive Programming**: Uses Shiny's reactive framework
- **Data Processing**: Centralized in `server.R`
- **Indicator Calculations**: Specific functions for each food security metric
- **Report Generation**: R Markdown integration

### Namespace System
- **ID Isolation**: Each module has its own namespace using `NS(id)`
- **Module Communication**: Server logic handles cross-module interactions
- **Reusable Components**: Modules can be used independently

This modular architecture separates concerns across UI modules, server logic, and styling, enabling focused development and maintenance of specific application areas.