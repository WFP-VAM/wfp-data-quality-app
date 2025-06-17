# WFP Survey Data Quality App - Architecture Overview

## Overview
This document provides a high-level architectural overview to help developers understand the codebase structure and locate key implementation areas.

## Application Structure

### Framework & File Organization
- **Main Application**: `inst/app/app.R` (~5000 lines)
- **Package Entry Point**: `R/run_app.R` 
- **Report Template**: `inst/app/report.Rmd`

*Note: The `inst/` folder contains files that are installed with the package and accessible via `system.file()` at runtime.*

### Code Structure in app.R
```
Lines 1-45:     Package imports and configuration
Lines 49-1200:  UI definition (shinydashboard)
Lines 1205-4989: Server logic and reactive functions
```

## UI Architecture (Lines 49-1204)

### Dashboard Structure
- **Framework**: `shinydashboard` with header, sidebar, and body
- **Navigation**: 12 main tabs in sidebar menu
- **Styling**: Custom CSS with WFP branding (lines 49-150)

### Key UI Sections
```r
# Dashboard structure
dashboardPage(
  dashboardHeader(...),    # Lines ~50
  dashboardSidebar(...),   # Lines ~50-70 (12 menu items)
  dashboardBody(...)       # Lines ~70-1200 (tab content)
)
```

### Tab Structure (Lines 150-1200)
Each tab follows this pattern:
- Home (lines ~150-200): App guidance
- Data Upload (lines ~200-300): File input + MoDa API
- Survey Progress (lines ~300-400): Monitoring dashboards
- Indicators (FCS, HDDS, rCSI, etc.): Analysis interfaces
- Report (lines ~1100-1200): Report generation

## Server Architecture (Lines 1205-4989)

### Data Flow Core Functions

#### 1. Data Ingestion (Lines 1215-1331)
```r
# SPSS file upload
spss_data <- reactive({ ... })           # Line 1215

# MoDa API integration  
moda_data <- eventReactive({ ... })      # Line 1232

# Unified data source
dataset <- reactive({ ... })             # Line 1331
```

#### 2. Data Processing (Lines 2006-2100)
```r
# Main filtered dataset
reqData <- reactive({ ... })             # Line 2006

# Administrative filters
# Note: Admin filter reactives are integrated throughout server logic
```

#### 3. Indicator Calculations (Lines 2000-4000)
Each food security indicator has dedicated calculation functions:
- **FCS**: Lines ~2000-2500
- **HDDS**: Lines ~2500-3000  
- **rCSI**: Lines ~3000-3500
- **CARI**: Lines ~3500-4000

### Key Helper Functions (Throughout server section)
```r
convert_to_monthly()     # Expenditure standardization
calc_monthly_expense()   # Monthly calculations
pct4()                  # 4-point scale percentages
```

## Data Integration Points

### SPSS Files (Lines ~1310-1350)
```r
# File upload handling
input$spss_file
haven::read_sav()        # SPSS file reading
# Expected: 80+ standardized variables
```

### MoDa API (Lines ~1350-1400)
```r
# API workflow
httr::POST()            # Export job creation
httr::GET()             # Status polling  
# Handles: Authentication, async exports, ZIP extraction
```

## Output Generation (Lines 3500-4984)

### Visualization Pattern
Each indicator follows this pattern:
```r
# Plot outputs
output$fcs_plot <- renderPlotly({ ... })
output$hdds_table <- renderDT({ ... })
output$rcsi_valuebox <- renderValueBox({ ... })
```

### Report Generation (Line 4920)
```r
# R Markdown integration
output$downloadReport <- downloadHandler({
  rmarkdown::render("report.Rmd", ...)
})
```

## Key Architecture Concepts

### Reactive Programming
- **Primary reactive**: `dataset()` (line 1331) - feeds all analyses
- **Filtered reactive**: `reqData()` (line 2006) - applies user filters
- **Indicator reactives**: Each indicator has calculation reactives

### Code Patterns to Understand
1. **Data source switching**: Lines 1331-1400 (SPSS vs MoDa)
2. **Filter cascading**: Throughout server logic (Admin1 → Admin2 → Admin4)
3. **Indicator calculation**: Lines 2006-4000 (consistent pattern per indicator)
4. **Output rendering**: Lines 2500-4989 (plots, tables, value boxes)

### Development Focus Areas
- **Data processing**: Lines 1215-2006
- **Indicator logic**: Lines 2006-4000  
- **UI/UX**: Lines 49-1204
- **Integration**: API code around lines 1232-1331

This structure enables developers to quickly locate specific functionality and understand the data flow from upload through analysis to visualization.