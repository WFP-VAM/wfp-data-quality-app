library(devtools)
library(dplyr)
library(DT)
library(forcats)
library(ggplot2)
library(glue)
library(haven)
library(htmltools)
library(httr)
library(jsonlite)
library(kableExtra)
library(labelled)
library(lubridate)
library(openxlsx)
library(plotly)
library(purrr)
library(rlang)
library(rmarkdown)
library(rstatix)
library(scales)
library(shiny)
library(shinydashboard)
library(tidyr)
library(tidyverse)
library(treemapify) # for treemap
library(writexl)
library(wfpthemes)

# custom files
source("ui/tabs/home.R")
source("ui/tabs/data_upload.R")
source("ui/tabs/survey_progress.R")
source("ui/tabs/fcs.R")
source("ui/tabs/hdds.R")
source("ui/tabs/rcsi.R")
source("ui/tabs/hhs.R")
source("ui/tabs/matrix.R")
source("ui/tabs/lcs.R")
source("ui/tabs/fes.R")
source("ui/tabs/cari.R")
source("ui/tabs/report.R")

# Set maximum file upload size to 200 MB
options(shiny.maxRequestSize = 200 * 1024^2) # 200 MB

############################
## UI
############################
ui <- dashboardPage(
    dashboardHeader(title = "WFP Data Quality Check"),
    dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
            menuItem("Survey Progress", tabName = "survey", icon = icon("chart-line")),
            menuItem("FCS", tabName = "fcs", icon = icon("utensils")),
            menuItem("HDDS", tabName = "hdds", icon = icon("leaf")),
            menuItem("rCSI", tabName = "rcsi", icon = icon("heartbeat")),
            menuItem("HHS", tabName = "hhs", icon = icon("home")),
            menuItem("MATRIX", tabName = "matrix", icon = icon("table")),
            menuItem("Livelihood Coping (LCS)", tabName = "lcs", icon = icon("users")),
            menuItem("FES", tabName = "fes", icon = icon("dollar")),
            menuItem("CARI", tabName = "cari", icon = icon("clipboard-check")),
            menuItem("Report", tabName = "report", icon = icon("file-alt"))
        )
    ),
    dashboardBody(
        tags$head(
            # includeCSS("https://uikit.wfp.org/cdn/1.21.0/css/styles.min.css"),
            includeCSS("www/custom.css")
        ),
        tabItems(

            # Tab 1: Home
            tabItem(
                tabName = "home",
                homeUI()
            ),
            # Tab 2: Data
            tabItem(
                tabName = "upload",
                dataUploadUI()
            ),
            # Tab 3: Survey Progress
            tabItem(
                tabName = "survey",
                surveyProgressUI()
            ),

            # Tab 4: FCS
            tabItem(
                tabName = "fcs",
                fcsUI()
            ),

            # Tab 5: HHDS
            tabItem(
                tabName = "hdds",
                hhdsUI()
            ),

            # Tab 6: rCSI
            tabItem(
                tabName = "rcsi",
                rcsiUI()
            ),

            # Tab 7: HHS
            tabItem(
                tabName = "hhs",
                hhsUI()
            ),

            # Tab : HHS
            tabItem(
                tabName = "matrix",
                fewsnetMatrixUI()

            ),

            #########################################################################
            # TAB 9: LIVELIHOOD COPING STRATEGIES (with sub-tabs)
            #########################################################################
            tabItem(
                tabName = "lcs",
                lcsUI()

            ),
            #########################################################################
            # TAB 10: FES (with sub-tabs)
            #########################################################################
            tabItem(
                tabName = "fes",
                fesUI()
            ),

            #########################################################################
            # TAB 11: CARI (with sub-tabs)
            #########################################################################
            tabItem(
                tabName = "cari",
                cariUI()
            ),

            #########################################################################
            # TAB 12: REPORT
            #########################################################################
            tabItem(
                tabName = "report",
                reportUI()

            )
        ) # end tabItems
    )
)
