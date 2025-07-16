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
source("tabs/home.R")
source("tabs/data_upload.R")
source("tabs/survey_progress.R")

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
            # Load the font
            tags$link(
                href = "https://fonts.googleapis.com/css2?family=Open+Sans:ital,wght@0,300..800;1,300..800&display=swap",
            ),
            includeCSS("www/custom.css")
        ),
        tabItems(

            ########################
            # TAB 1: HOME
            ########################
            tabItem(
                tabName = "home",
                homeUI()
            ),

            ########################
            # TAB 2: DATA UPLOAD
            ########################
            
            tabItem(
                tabName = "upload",
                dataUploadUI()
            ),

            ########################
            # TAB 3: SURVEY PROGRESS
            ########################
            tabItem(
                tabName = "survey",
                surveyProgressUI()

            ),

            #########################################################################
            # TAB 4: FCS (with sub-tabs)
            #########################################################################
            tabItem(
                tabName = "fcs",
                fluidRow(
                    tabBox(
                        id = "fcsTabs", width = 12,

                        ## Sub-tab: FCS by Admin1
                        tabPanel(
                            "FCS by Admin1",
                            fluidRow(
                                column(
                                    4,
                                    box(
                                        title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                        selectInput("fcsThresholdAdm1", "FCS Threshold:",
                                            choices = c("FCSCat21", "FCSCat28"),
                                            selected = "FCSCat21"
                                        )
                                        # No Admin1 filter here, because you're showing all on one chart
                                    )
                                ),
                                column(
                                    8,
                                    box(
                                        title = "FCS by Admin1", width = 12, status = "primary", solidHeader = TRUE,
                                        plotlyOutput("plotFCSadm1")
                                    )
                                )
                            )
                        ),

                        ## Sub-tab: FCS by Admin2
                        tabPanel(
                            "FCS by Admin2",
                            fluidRow(
                                column(
                                    4,
                                    box(
                                        title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                        # Force user to pick exactly one Admin1
                                        selectInput("admin1FilterFCS2", "Admin1:",
                                            choices = character(0), # we'll populate from server
                                            selected = NULL
                                        ),
                                        selectInput("fcsThresholdAdm2", "FCS Threshold:",
                                            choices = c("FCSCat21", "FCSCat28"),
                                            selected = "FCSCat21"
                                        )
                                    )
                                ),
                                column(
                                    8,
                                    box(
                                        title = "FCS by Admin2", width = 12, status = "primary", solidHeader = TRUE,
                                        plotlyOutput("plotFCSadm2")
                                    )
                                )
                            )
                        ),

                        ## Sub-tab: FCS by Admin1/2 & Enumerator
                        tabPanel(
                            "FCS by Admin1/2 & Enumerator",
                            fluidRow(
                                column(
                                    4,
                                    box(
                                        title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                        # Force user to pick exactly one Admin1
                                        selectInput("admin1FilterEnumFCS", "Admin1:",
                                            choices = character(0),
                                            selected = NULL
                                        ),
                                        # Admin2 filter with "All" option
                                        selectInput("admin2FilterEnumFCS", "Admin2:",
                                            choices = c("All"), # we'll populate from server
                                            selected = "All"
                                        ),
                                        # FCS threshold filter
                                        selectInput("fcsThresholdAdm1Enum", "FCS Threshold:",
                                            choices = c("FCSCat21", "FCSCat28"),
                                            selected = "FCSCat21"
                                        )
                                    )
                                ),
                                column(
                                    8,
                                    box(
                                        title = "FCS by Admin1/2 & Enumerator", width = 12, status = "primary", solidHeader = TRUE,
                                        plotlyOutput("plotFCSadm1Enum")
                                    )
                                )
                            )
                        ),

                        ## Sub-tab: Boxplot Median FCS by Enumerator
                        tabPanel(
                            "FCS Boxplot (Enumerator)",
                            fluidRow(
                                box(
                                    title = "Boxplot: Median FCS by Enumerator", status = "primary", solidHeader = TRUE, width = 12,
                                    plotlyOutput("plotFCSEnumNameBox")
                                )
                            ),
                            fluidRow(
                                box(
                                    title = "Enumerators with Outlier Median FCS", width = 12, status = "info",
                                    tableOutput("tableFCSOutlier")
                                )
                            )
                        ),

                        #######################################################################
                        ## ADDITIONAL SUB-TABS FOR TABLES
                        #######################################################################

                        ## Sub-tab: Households with Low Cereal Consumption
                        tabPanel(
                            "Low Cereal Consumption",
                            fluidRow(
                                box(
                                    title = "Households with Low Cereal Consumption", width = 12, status = "primary",
                                    dataTableOutput("tableCereal"),
                                    br(),
                                    downloadButton("downloadCereal", "Dowload Low Cereal Table (Excel)")
                                )
                            )
                        ),

                        ## Sub-tab: Cereal, Vegetable, and Dairy
                        tabPanel(
                            "Cereal, Vegetable, Dairy",
                            fluidRow(
                                box(
                                    title = "Cereal, Vegetable, and Dairy", width = 12, status = "primary",
                                    dataTableOutput("tableCerealVegDairy"),
                                    br(),
                                    downloadButton("downloadCerealVegDairy", "Dowload Cereal/Veg/Dairy Table (Excel)")
                                )
                            )
                        ),

                        ## Sub-tab: Low Vegetable Consumption
                        tabPanel(
                            "Low Vegetable Consumption",
                            fluidRow(
                                box(
                                    title = "Low Vegetable Consumption", width = 12, status = "primary",
                                    dataTableOutput("tableVeg"),
                                    br(),
                                    downloadButton("downloadVeg", "Dowload Low Veg Table (Excel)")
                                )
                            )
                        ),

                        ## Sub-tab: Meat, Oil, and Sugar Consumption
                        tabPanel(
                            "Meat, Oil, and Sugar",
                            fluidRow(
                                box(
                                    title = "Meat, Oil, and Sugar Consumption", width = 12, status = "primary", solidHeader = TRUE,
                                    dataTableOutput("tableMeatOilSugar"),
                                    br(),
                                    downloadButton("downloadMeatOilSugar", "Download Meat/Oil/Sugar Table (Excel)")
                                )
                            )
                        )
                    ) # end tabBox
                )
            ),

            #########################################################################
            # TAB 5: HDDS (with sub-tabs)
            #########################################################################
            tabItem(
                tabName = "hdds",
                fluidRow(
                    tabBox(
                        id = "hddsTabs", width = 12,

                        ## Sub-tab: HDDS by Admin1
                        tabPanel(
                            "HDDS by Admin1",
                            fluidRow(
                                box(
                                    title = "HDDS (CH phases) by Admin1", width = 12, status = "primary",
                                    plotlyOutput("plotHDDSadm1")
                                )
                            )
                        ),

                        ## Sub-tab: HDDS by Admin2
                        tabPanel(
                            "HDDS by Admin2",
                            fluidRow(
                                column(
                                    4,
                                    box(
                                        title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                        # Force user to pick exactly one Admin1
                                        selectInput("admin1FilterHDDS2", "Admin1:",
                                            choices = character(0), # we'll populate from server
                                            selected = NULL
                                        ),
                                    )
                                ),
                                column(
                                    8,
                                    box(
                                        title = "HDDS (CH phases) by Admin2", width = 12, status = "primary", solidHeader = TRUE,
                                        plotlyOutput("plotHDDSadm2")
                                    )
                                )
                            )
                        ),

                        ## Sub-tab: HDDS by Admin1/2 & Enumerator
                        tabPanel(
                            "HDDS by Admin1/2 & Enumerator",
                            fluidRow(
                                column(
                                    4,
                                    box(
                                        title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                        # Force user to pick exactly one Admin1
                                        selectInput("admin1FilterEnumHDDS", "Admin1:",
                                            choices = character(0),
                                            selected = NULL
                                        ),
                                        # Admin2 filter with "All" option
                                        selectInput("admin2FilterEnumHDDS", "Admin2:",
                                            choices = c("All"), # we'll populate from server
                                            selected = "All"
                                        ),
                                    )
                                ),
                                column(
                                    8,
                                    box(
                                        title = " HDDS (CH phases) by Admin1/2 & Enumerator", width = 12, status = "primary", solidHeader = TRUE,
                                        plotlyOutput("plotHDDSadm1Enum")
                                    )
                                )
                            )
                        ),
                    ) # end tabBox
                )
            ),

            #########################################################################
            # TAB 6: rCSI (with sub-tabs)
            #########################################################################
            tabItem(
                tabName = "rcsi",
                fluidRow(
                    tabBox(
                        id = "rcsiTabs", width = 12,

                        ## Sub-tab: rCSI (CH phases) by Admin1
                        tabPanel(
                            "rCSI by Admin1",
                            fluidRow(
                                box(
                                    title = "rCSI (CH phases) by Admin1", width = 12, status = "primary",
                                    plotlyOutput("plotrCSIadm1")
                                )
                            )
                        ),

                        ## Sub-tab: rCSI (CH phases) by Admin2
                        tabPanel(
                            "rCSI by Admin2",
                            fluidRow(
                                column(
                                    4,
                                    box(
                                        title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                        # Force user to pick exactly one Admin1
                                        selectInput("admin1FilterrCSI2", "Admin1:",
                                            choices = character(0), # we'll populate from server
                                            selected = NULL
                                        ),
                                    )
                                ),
                                column(
                                    8,
                                    box(
                                        title = "rCSI (CH phases) by Admin2", width = 12, status = "primary", solidHeader = TRUE,
                                        plotlyOutput("plotrCSIadm2")
                                    )
                                )
                            )
                        ),

                        ## Sub-tab: rCSI by Admin1/2 & Enumerator
                        tabPanel(
                            "rCSI by Admin1/2 & Enumerator",
                            fluidRow(
                                column(
                                    4,
                                    box(
                                        title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                        # Force user to pick exactly one Admin1
                                        selectInput("admin1FilterEnumrCSI", "Admin1:",
                                            choices = character(0),
                                            selected = NULL
                                        ),
                                        # Admin2 filter with "All" option
                                        selectInput("admin2FilterEnumrCSI", "Admin2:",
                                            choices = c("All"), # we'll populate from server
                                            selected = "All"
                                        ),
                                    )
                                ),
                                column(
                                    8,
                                    box(
                                        title = "rCSI (CH phases) by Admin1/2 & Enumerator", width = 12, status = "primary", solidHeader = TRUE,
                                        plotlyOutput("plotrCSIadm1Enum")
                                    )
                                )
                            )
                        ),




                        ## Sub-tab: rCSI Boxplot & Outliers Admin1
                        tabPanel(
                            "rCSI Boxplot by Admin1",
                            fluidRow(
                                column(
                                    12,
                                    box(
                                        title = "rCSI by Admin1", status = "primary", solidHeader = TRUE, width = 12,
                                        plotlyOutput("plotRCSIBoxAdmin1")
                                    )
                                )
                            ),
                            fluidRow(
                                column(
                                    12,
                                    box(
                                        title = "Outliers in rCSI by Admin1", status = "primary", solidHeader = TRUE, width = 12,
                                        DT::dataTableOutput("tableRCSIOutliersAdmin1"),
                                        br(),
                                        downloadButton("downloadRCSIOutliersAdmin1", "Download Outlier Table (Excel)")
                                    )
                                )
                            )
                        ),

                        # Sub-tab: rCSI Boxplot & Outliers Admin2
                        tabPanel(
                            "rCSI Boxplot by Admin2",
                            fluidRow(
                                column(
                                    4,
                                    box(
                                        title = "rCSI Filters", status = "warning", solidHeader = TRUE, width = 12,
                                        selectInput("rcsiBoxAdmin2Admin1",
                                            "Select Admin1:",
                                            choices = character(0),
                                            selected = NULL
                                        )
                                    )
                                ),
                                column(
                                    8,
                                    box(
                                        title = "rCSI by Admin2", status = "primary", solidHeader = TRUE, width = 12,
                                        plotlyOutput("plotRCSIBoxAdmin2")
                                    )
                                )
                            ),
                            fluidRow(
                                column(
                                    12,
                                    box(
                                        title = "Outliers in rCSI by Admin2", status = "info", solidHeader = TRUE, width = 12,
                                        DT::dataTableOutput("tableRCSIOutliersAdmin2"),
                                        br(),
                                        downloadButton("downloadRCSIOutliersAdmin2", "Download Outlier Table (Excel)")
                                    )
                                )
                            )
                        ),

                        # Sub-Tab for Triangulation within the rCSI tab
                        tabPanel(
                            "Triangulation",
                            fluidRow(
                                box(
                                    title = "Triangulation Filters", status = "warning", solidHeader = TRUE, width = 4,
                                    selectInput("triangAdmin1", "Select Admin1:", choices = character(0)),
                                    # New Admin2 filter input; will be updated from server
                                    selectInput("triangAdmin2", "Select Admin2:", choices = c("All"), selected = "All"),
                                    # New FCSCat selector input
                                    selectInput("triangFCSCat", "Select FCS Category:",
                                        choices = c("FCSCat21", "FCSCat28"), selected = "FCSCat21"
                                    )
                                )
                            ),
                            fluidRow(
                                box(
                                    title = "Triangulation of rCSI by FCS (Poor or Borderline) by Enumerator", status = "primary", solidHeader = TRUE, width = 12,
                                    plotlyOutput("plotTriangulation")
                                )
                            ),
                            fluidRow(
                                box(
                                    title = "Triangulation Details", status = "info", solidHeader = TRUE, width = 12,
                                    dataTableOutput("tableTriangulation"),
                                    br(),
                                    downloadButton("downloadTriangulation", "Download Triangulaion (Excel)")
                                )
                            )
                        )
                    ) # end tabBox
                )
            ),

            #########################################################################
            # TAB 7: HHS (with sub-tabs)
            #########################################################################
            tabItem(
                tabName = "hhs",
                fluidRow(
                    tabBox(
                        id = "hhsTabs", width = 12,

                        ## Sub-tab: HHS by Admin1
                        tabPanel(
                            "HHS by Admin1",
                            fluidRow(
                                box(
                                    title = "HHS (CH phases) by Admin1", width = 12, status = "primary",
                                    plotlyOutput("plotHHSadm1")
                                )
                            )
                        ),

                        ## Sub-tab: HHS by Admin2
                        tabPanel(
                            "HHS by Admin2",
                            fluidRow(
                                column(
                                    4,
                                    box(
                                        title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                        # Force user to pick exactly one Admin1
                                        selectInput("admin1FilterHHS2", "Admin1:",
                                            choices = character(0), # we'll populate from server
                                            selected = NULL
                                        ),
                                    )
                                ),
                                column(
                                    8,
                                    box(
                                        title = "HHS (CH phases) by Admin2", width = 12, status = "primary", solidHeader = TRUE,
                                        plotlyOutput("plotHHSadm2")
                                    )
                                )
                            )
                        ),

                        ## Sub-tab: HHS by Admin1 & Enumerator
                        tabPanel(
                            "HHS by Admin1/2 & Enumerator",
                            fluidRow(
                                column(
                                    4,
                                    box(
                                        title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                        # Force user to pick exactly one Admin1
                                        selectInput("admin1FilterEnumHHS", "Admin1:",
                                            choices = character(0),
                                            selected = NULL
                                        ),
                                        # Admin2 filter with "All" option
                                        selectInput("admin2FilterEnumHHS", "Admin2:",
                                            choices = c("All"), # we'll populate from server
                                            selected = "All"
                                        ),
                                    )
                                ),
                                column(
                                    8,
                                    box(
                                        title = "HHS (CH phases) by Admin1/2 & Enumerator", width = 12, status = "primary", solidHeader = TRUE,
                                        plotlyOutput("plotHHSadm1Enum")
                                    )
                                )
                            )
                        ),
                    ) # end tabBox
                )
            ),

            #########################################################################
            # TAB 8: MATRIX Table
            #########################################################################

            tabItem(
                tabName = "matrix",
                fluidRow(
                    tabBox(
                        id = "matrixTabs", width = 12,
                        tabPanel(
                            "Matrix",
                            sidebarLayout(
                                sidebarPanel(
                                    selectInput("matAdmin1", "Admin1:", choices = NULL),
                                    selectInput("matAdmin2", "Admin2:", choices = "All"),
                                    selectInput("matFCSCat", "FCS Category:",
                                        choices = c("FCSCat21", "FCSCat28"),
                                        selected = "FCSCat21"
                                    )
                                ),
                                mainPanel(
                                    DTOutput("matrixTable"),
                                    br(),
                                    h4("Detail — illogical cells numbers (3,4,5,8,9,10)"),
                                    DTOutput("matrixDetails")
                                )
                            )
                        ),
                    ) # end tabBox
                )
            ),


            #########################################################################
            # TAB 9: LIVELIHOOD COPING STRATEGIES (with sub-tabs)
            #########################################################################
            tabItem(
                tabName = "lcs",
                fluidRow(
                    tabBox(
                        id = "lcsTabs", width = 12,

                        ## Sub-tab: "Dynamic LCS" to select strategies
                        tabPanel(
                            "Dynamic LCS",
                            fluidRow(
                                column(
                                    4,
                                    box(
                                        title = "Select Strategies", status = "warning", solidHeader = TRUE, width = 12,

                                        # 1) Stress strategies
                                        selectInput("stressVars", "Pick EXACTLY 4 Stress Strategies:",
                                            multiple = TRUE,
                                            # No default 'selected' – starts empty
                                            choices = c(
                                                "Lcs_stress_Saving", "Lcs_stress_DomAsset", "Lcs_stress_ConsActive",
                                                "Lcs_stress_SellFoodRation", "Lcs_stress_SellNFIRation", "Lcs_stress_EatOut",
                                                "Lcs_stress_BorrowCash", "Lcs_stress_Pawn", "Lcs_stress_LessSchool",
                                                "Lcs_stress_Utilities", "Lcs_stress_Edu", "Lcs_stress_BorrowFood",
                                                "Lcs_stress_MoreLabour", "Lcs_stress_HHSeparation", "Lcs_stress_Housing",
                                                "Lcs_stress_LessSchool", "LcsR_stress_Animals", "LcsR_stress_BorrowCash",
                                                "LcsR_stress_Pawn", "LcsR_stress_DomAsset", "LcsR_stress_EatOut",
                                                "LcsR_stress_LessSchool", "LcsR_stress_Saving", "LcsR_stress_HHSeparation",
                                                "LcsR_stress_ConsActive", "LcsR_stress_SellFoodRation", "LcsR_stress_DomMigration",
                                                "LcsR_stress_Housing", "LcsR_stress_SellNFIRation", "LcsR_stress_Edu"
                                            )
                                        ),

                                        # 2) Crisis strategies
                                        selectInput("crisisVars", "Pick EXACTLY 3 Crisis Strategies:",
                                            multiple = TRUE,
                                            choices = c(
                                                "Lcs_crisis_ProdAssets", "Lcs_crisis_Barter", "Lcs_crisis_Health",
                                                "Lcs_crisis_Housing", "Lcs_crisis_HHSeparation", "Lcs_crisis_OutSchool",
                                                "Lcs_crisis_Migration", "Lcs_crisis_DomMigration", "Lcs_crisis_ChildWork",
                                                "Lcs_crisis_Edu_Health", "Lcs_crisis_Barter", "Lcs_crisis_ConsActive",
                                                "Lcs_crisis_Edu", "Lcs_crisis_Health", "Lcs_crisis_Marriage", "Lcs_crisis_Utilities",
                                                "LcsR_crisis_AgriCare", "LcsR_crisis_ImmCrops",
                                                "LcsR_crisis_Seed", "LcsR_crisis_Animals", "LcsR_crisis_Health",
                                                "LcsR_crisis_Edu", "LcsR_crisis_ProdAssets", "LcsR_crisis_Housing",
                                                "LcsR_crisis_HHSeparation", "LcsR_crisis_Barter", "LcsR_crisis_Migration",
                                                "LcsR_crisis_ChildWork", "LcsR_crisis_Marriage", "LcsR_crisis_ConsActive",
                                                "LcsR_crisis_OutSchool", "LcsR_crisis_DomMigration"
                                            )
                                        ),

                                        # 3) Emergency strategies
                                        selectInput("emergencyVars", "Pick EXACTLY 3 Emergency Strategies:",
                                            multiple = TRUE,
                                            choices = c(
                                                "Lcs_em_ChildMigration", "Lcs_em_IllegalAct", "Lcs_em_Begged",
                                                "Lcs_em_Marriage", "Lcs_em_ResAsset", "Lcs_em_Migration", "Lcs_em_ChildWork",
                                                "Lcs_em_OutSchool", "LcsR_em_FemAnimal", "LcsR_em_WildFood", "LcsR_em_Seed",
                                                "LcsR_em_OutSchool", "LcsR_em_Migration", "LcsR_em_ChildWork", "LcsR_em_Marriage",
                                                "LcsR_em_ResAsset", "LcsR_em_Begged", "LcsR_em_IllegalAct", "LcsR_em_ChildMigration"
                                            )
                                        )
                                    )
                                ),
                                column(
                                    8,
                                    box(
                                        title = "Dynamic LCS Results", status = "primary", solidHeader = TRUE, width = 12,
                                        # We'll display a plot (or any other output) here
                                        plotlyOutput("plotDynamicLCS"),
                                        # Also display an info/warning text if there's a problem
                                        textOutput("dynamicLCSWarning")
                                    )
                                )
                            )
                        ),

                        ## Sub-tab: LCS by Admin1
                        tabPanel(
                            "LCS by Admin1",
                            fluidRow(
                                box(
                                    title = "LCS by Admin1", width = 12, status = "primary",
                                    plotlyOutput("plotLHCSadm1")
                                )
                            )
                        ),

                        ## Sub-tab: LCS by Admin2
                        tabPanel(
                            "LCS by Admin2",
                            fluidRow(
                                column(
                                    4,
                                    box(
                                        title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                        # Force user to pick exactly one Admin1
                                        selectInput("admin1FilterLCS2", "Admin1:",
                                            choices = character(0), # we'll populate from server
                                            selected = NULL
                                        ),
                                    )
                                ),
                                column(
                                    8,
                                    box(
                                        title = "LCS by Admin2", width = 12, status = "primary", solidHeader = TRUE,
                                        plotlyOutput("plotLHCSadm2")
                                    )
                                )
                            )
                        ),

                        ## Sub-tab: LCS by Admin1 & Enumerator
                        tabPanel(
                            "LCS by Admin1/2 & Enumerator",
                            fluidRow(
                                column(
                                    4,
                                    box(
                                        title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                        # Force user to pick exactly one Admin1
                                        selectInput("admin1FilterEnumLCS", "Admin1:",
                                            choices = character(0),
                                            selected = NULL
                                        ),
                                        # Admin2 filter with "All" option
                                        selectInput("admin2FilterEnumLCS", "Admin2:",
                                            choices = c("All"), # we'll populate from server
                                            selected = "All"
                                        ),
                                    )
                                ),
                                column(
                                    8,
                                    box(
                                        title = "LCS by Admin1/2 & Enumerator", width = 12, status = "primary", solidHeader = TRUE,
                                        plotlyOutput("plotLHCSadm1Enum")
                                    )
                                )
                            )
                        ),

                        # Sub-tab: LCS by Strategy
                        tabPanel(
                            "LCS by Strategy",
                            fluidRow(
                                column(
                                    4,
                                    box(
                                        title = "Filters", status = "warning", solidHeader = TRUE, width = 12,
                                        selectInput("lcsStrategyAdmin1", "Select Admin1:",
                                            choices = character(0), selected = NULL
                                        ),
                                        selectInput("lcsStrategyAdmin2", "Select Admin2:",
                                            choices = c("All"), selected = "All"
                                        ),
                                        selectInput("lcsStrategyEnu", "Select Enumerator:",
                                            choices = c("All"), selected = "All"
                                        )
                                    )
                                ),
                                column(
                                    8,
                                    box(
                                        title = "Breakdown of Livelihood Coping Strategies (LCS)", status = "primary", solidHeader = TRUE, width = 12,
                                        plotlyOutput("plotLCSStrategy")
                                    )
                                )
                            )
                        ),
                    ) # end tabBox
                )
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
