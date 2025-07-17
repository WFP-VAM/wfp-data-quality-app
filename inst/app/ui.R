############################
## Load Packages
############################
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

# custom files
source("ui/home.R")
source("ui/data_upload.R")
source("ui/survey_progress.R")
source("ui/fcs.R")
source("ui/hdds.R")
source("ui/rcsi.R")
source("ui/hhs.R")
source("ui/matrix.R")
source("ui/lcs.R")

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
                fluidRow(
                    tabBox(
                        id = "fesTabs", width = 12,

                        # 1) Sub-tab: FES by Admin1
                        tabPanel(
                            "FES by Admin1",
                            fluidRow(
                                column(
                                    4,
                                    box(
                                        title = "FES Filters", status = "warning", solidHeader = TRUE, width = 12,
                                        # Let user pick recall period
                                        selectInput("fesRecall", "Recall Period:",
                                            choices = c("7 days" = 7, "1 month" = 30),
                                            selected = 30
                                        )
                                    )
                                ),
                                column(
                                    8,
                                    box(
                                        title = "Food Expenditure Share (FES) by Admin1", status = "primary", solidHeader = TRUE, width = 12,
                                        plotlyOutput("plotFESadm1")
                                    )
                                )
                            )
                        ),

                        # 2) Sub-tab: FES by Admin2 (force user to pick an Admin1)
                        tabPanel(
                            "FES by Admin2",
                            fluidRow(
                                column(
                                    4,
                                    box(
                                        title = "FES Filters", status = "warning", solidHeader = TRUE, width = 12,
                                        selectInput("admin1FilterFES2", "Select Admin1:",
                                            choices = character(0), selected = NULL
                                        )
                                    )
                                ),
                                column(
                                    8,
                                    box(
                                        title = "Food Expenditure Share (FES) by Admin2", status = "primary", solidHeader = TRUE, width = 12,
                                        plotlyOutput("plotFESadm2")
                                    )
                                )
                            )
                        ),

                        # 3) Sub-tab: FES by Admin1 & Enumerator
                        tabPanel(
                            "FES by Admin1/2 & Enumerator",
                            fluidRow(
                                column(
                                    4,
                                    box(
                                        title = "FES Filters", status = "warning", solidHeader = TRUE, width = 12,
                                        selectInput("admin1FilterEnumFES", "Select Admin1:",
                                            choices = character(0), selected = NULL
                                        ),
                                        selectInput("admin2FilterEnumFES", "Select Admin2:",
                                            choices = c("All"), selected = "All"
                                        )
                                    )
                                ),
                                column(
                                    8,
                                    box(
                                        title = "Food Expenditure Share (FES) by Admin1/2 & Enumerator", status = "primary", solidHeader = TRUE, width = 12,
                                        plotlyOutput("plotFESadm1Enum")
                                    )
                                )
                            )
                        ),
                        # 4) Sub-tab: Food Expenses by Admin1 (Boxplot and Outliers)
                        tabPanel(
                            "FES Food Expenses by Admin1",
                            fluidRow(
                                # Remove the Admin1 selection dropdown (no filtering needed at Admin1 level)
                                column(
                                    12,
                                    box(
                                        title = "Monthly Food Expenses by Admin1", status = "primary", solidHeader = TRUE, width = 12,
                                        plotlyOutput("plotFESBoxAdmin1")
                                    )
                                )
                            ),
                            fluidRow(
                                box(
                                    title = "Outlier Table: FES Food Expenses by Admin1", status = "primary", solidHeader = TRUE, width = 12,
                                    dataTableOutput("tableFESOutliersAdmin1"),
                                    br(),
                                    downloadButton("downloadOutlierTableAdmin1", "Export to Excel")
                                )
                            )
                        ),
                        # 5) Sub-tab: Food Expenses by Admin2 (Boxplot and Outliers)

                        tabPanel(
                            "FES Food Expenses by Admin2",
                            fluidRow(
                                box(
                                    title = "FES Filters", status = "warning", solidHeader = TRUE, width = 4,
                                    # This filter lets the user choose an Admin1.
                                    selectInput("fesBoxAdmin2Admin1",
                                        "Select Admin1:",
                                        choices = character(0), # will be updated in the server
                                        selected = NULL
                                    )
                                ),
                                box(
                                    title = "Boxplot of Monthly Food Expenses by Admin2", status = "primary", solidHeader = TRUE, width = 8,
                                    plotlyOutput("plotFESBoxAdmin2")
                                )
                            ),
                            fluidRow(
                                box(
                                    title = "Outliers in Monthly Food Expenses by Admin2", status = "info", solidHeader = TRUE, width = 12,
                                    dataTableOutput("tableFESOutliersAdmin2"),
                                    br(),
                                    downloadButton("downloadOutlierTable", "Download Outlier Table (Excel)")
                                )
                            )
                        ),

                        # 6) Sub-tab: Zero Montly Food Expenses
                        tabPanel(
                            "Zero Food Expenditure",
                            DT::dataTableOutput("zeroExpTable")
                        )
                    ) # end tabBox
                )
            ),

            #########################################################################
            # TAB 11: CARI (with sub-tabs)
            #########################################################################
            tabItem(
                tabName = "cari",
                fluidRow(
                    tabBox(
                        id = "cariTabs", width = 12,

                        # A) CARI Console
                        tabPanel(
                            "CARI Console",
                            fluidRow(
                                column(
                                    4,
                                    box(
                                        title = "Select FCS Threshold for CARI", status = "warning", solidHeader = TRUE, width = 12,
                                        selectInput("cariFcsThreshold", "FCS Threshold:",
                                            choices = c("21" = "FCSCat21", "28" = "FCSCat28"), selected = "FCSCat21"
                                        )
                                    )
                                ),
                                column(
                                    8,
                                    box(
                                        title = "CARI Console Table", status = "primary", width = 12,
                                        htmlOutput("cariCtable")
                                    )
                                )
                            )
                        ),

                        # B) CARI by Admin1
                        tabPanel(
                            "CARI by Admin1",
                            fluidRow(
                                box(
                                    title = "CARI by Admin1", width = 12, status = "primary",
                                    plotlyOutput("plotCARIadm1")
                                )
                            )
                        ),

                        # C) CARI by Admin2
                        tabPanel(
                            "CARI by Admin2",
                            fluidRow(
                                column(
                                    4,
                                    box(
                                        title = "Filters", width = 12, status = "warning",
                                        selectInput("admin1FilterCARI2", "Admin1:", choices = character(0), selected = NULL)
                                    )
                                ),
                                column(
                                    8,
                                    box(
                                        title = "CARI by Admin2", width = 12, status = "primary",
                                        plotlyOutput("plotCARIadm2")
                                    )
                                )
                            )
                        ),

                        # D) CARI by Admin1 & Enumerator
                        tabPanel(
                            "CARI by Admin1/2 & Enumerator",
                            fluidRow(
                                column(
                                    4,
                                    box(
                                        title = "Filters", width = 12, status = "warning",
                                        selectInput("admin1FilterEnumCARI", "Admin1:", choices = character(0), selected = NULL),
                                        selectInput("admin2FilterEnumCARI", "Admin2:", choices = c("All"), selected = "All")
                                    )
                                ),
                                column(
                                    8,
                                    box(
                                        title = "CARI by Admin1/2 & Enumerator", width = 12, status = "primary",
                                        plotlyOutput("plotCARIadm1Enum")
                                    )
                                )
                            )
                        )
                    ) # end tabBox
                )
            ),

            #########################################################################
            # TAB 12: REPORT
            #########################################################################
            tabItem(
                tabName = "report",
                fluidRow(
                    box(
                        title = "Report Instructions", width = 12, status = "info", solidHeader = TRUE,
                        "Please consult all pages of the application to generate the report.
       Once you have visited all tabs, click the button below to download an HTML report that mirrors the app."
                    )
                ),
                fluidRow(
                    box(
                        title = "Download Report", width = 12, status = "primary", solidHeader = TRUE,
                        uiOutput("reportDownloadUI")
                    )
                )
            )
        ) # end tabItems
    )
)
