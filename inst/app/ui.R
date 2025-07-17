

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
source("ui/fes.R")
source("ui/cari.R")
source("ui/report.R")

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
