############################
## Data Quality Check App
#This is a data quality check tool for food security assessment in near real time.
#The app was developed by WFP WACARO RAM Team with support from all COs.
#The app is structure in 3 main points: User Interface (UI), Server Code and Report Template.
############################



############################
## Load Packages
############################
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(haven)
library(labelled)
library(plotly)
library(rstatix)
library(kableExtra)
library(lubridate)
library(DT)
library(forcats)
library(treemapify) # for treemap
library(writexl)
library(rlang)
library(rmarkdown)
library(openxlsx)
library(htmltools)
library(purrr)
library(tidyr)
library(glue)
library(forcats)
library(scales)
library(devtools)
library(httr)
library(jsonlite)

############################
## Increase upload limit
############################
options(shiny.maxRequestSize = 200 * 1024^2)  # 200 MB

############################
## UI
############################
ui <- dashboardPage(
  dashboardHeader(title = "Data Quality Check"),

  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Home",      tabName = "home", icon = icon("home")),
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
    # 1) Insert the CSS for cells 1..45 → FEWS‑NET colours
      tags$head(
        # Load the font
        tags$link(
          href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600;700&display=swap",
          rel  = "stylesheet"
        ),
        # Your custom CSS overrides
        tags$style(HTML("
        /* Fonts */
          body, .sidebar, .main-header .navbar {
            font-family: 'Poppins', sans-serif;
            color: #404040;         /* WFP Body Text */
            background: #ffffff;    /* WFP White */
          }

          /* Header gradient (Primary → Accent) */
          .skin-blue .main-header .logo {
            background: linear-gradient(135deg, #2A93FC, #1F6EBC) !important;
            color: #FFF !important;
            font-weight: 600;
            font-size: 1.4em;
          }
          .skin-blue .main-header .navbar {
            background: linear-gradient(135deg, #1F6EBC, #2A93FC) !important;
          }

          /* Sidebar background & active item */
          .skin-blue .main-sidebar {
            background: #003399 !important;  /* WFP Dark Blue */
          }
          .skin-blue .sidebar-menu > li.active > a {
            background: #2A93FC !important;  /* WFP Primary Blue */
            color: #FFF !important;
          }
          .skin-blue .sidebar-menu > li > a {
            color: #000000 !important;       /* very light grey for links */
          }
          .skin-blue .sidebar-menu > li > a:hover {
            background: #57AAFD !important;  /* a lighter WFP blue */
            color: #FFF !important;
          }

          /* Cards and boxes */
          .box, .info-box, .small-box {
            border-radius: 12px !important;
            box-shadow: 0 4px 12px rgba(0,0,0,0.05) !important;
            border: none !important;
          }
          .box-header .box-title {
            font-weight: 600 !important;
            font-size: 1.1em !important;
          }
        .matrix-table th, .matrix-table td { padding: 0; border: 1px solid #444; }
      .matrix-table th { background: #eee; }
      "))
      ),

    tabItems(

      ########################
      # TAB 1: HOME
      ########################
      tabItem(
        tabName = "home",
        fluidRow(
          box(
            title = "Guidance",
            status = "primary", solidHeader = TRUE, width = 12,
            HTML(
              "<ol>\
            <li><strong>Data Upload Tab</strong>: upload your data in <strong>SPSS (.sav)</strong> format. Max size 200 MB.<br>\
              <ul>\
                <li><strong>Upload .sav file</strong>: Select <strong>Upload .sav file</strong> under <strong>Choose data source</strong> dropdown list to browse your file.</li>\
                <li><strong>MoDa API</strong>: Select <strong>Load from MoDa API (.sav)</strong> from the dropdown list then enter your <strong>Form ID</strong> and your <strong>Token</strong> from MoDa.</li>\
              </ul>\
              Once loaded, you’ll see a preview of your data.\
            </li>\
            <li>Ensure your dataset contains <strong>all variables in the table below</strong>, plus the <strong>standard variable names of the key indicators</strong>: \
              <strong><a href='https://docs.wfp.org/api/documents/WFP-0000134782/download/' target='_blank'>FCS</a></strong>, \
              <strong><a href='https://docs.wfp.org/api/documents/WFP-0000136453/download/' target='_blank'>HDDS</a></strong>, \
              <strong><a href='https://docs.wfp.org/api/documents/WFP-0000109062/download/' target='_blank'>HHS</a></strong>, \
              <strong><a href='https://docs.wfp.org/api/documents/WFP-0000109756/download/' target='_blank'>rCSI</a></strong>, \
              <strong><a href='https://docs.wfp.org/api/documents/WFP-0000134094/download/' target='_blank'>LCS-FS</a></strong> \
              <strong><a href='https://docs.wfp.org/api/documents/WFP-0000147820/download//' target='_blank'>(List of strategies)</a></strong> and \
              <strong><a href='https://docs.wfp.org/api/documents/WFP-0000134243/download/' target='_blank'>FES</a></strong>.\
              <strong>This is mandatory for the app to work.</strong>\
            </li>\
            <li>If a variable (e.g. supervisor’s name) is missing, create an empty column named <code>EnuSupervisorName</code>.</li>\
            <li>If you encounter issues, please contact \
              <strong><a href='mailto:rbd.ram@wfp.org'>WACARO RAM</a></strong> or \
              <strong><a href='mailto:alioubadara.samake@wfp.org'>Aliou Badara SAMAKE</a></strong>.\
            </li>\
          </ol>"
            )
          )
        ),
        fluidRow(
          box(
            title = "Required Variables",
            status = "info", solidHeader = TRUE, width = 12,
            HTML(
              "<table class='table table-striped'>\
            <thead>\
              <tr>\
                <th>Variable name</th><th>Label</th>\
              </tr>\
            </thead>\
            <tbody>\
              <tr><td>RESPConsent</td><td>Consent form respondent to do the survey (1 = Yes, 0 = No)</td></tr>\
              <tr><td>ADMIN1Name</td><td>Admin1 area (region/wilaya)</td></tr>\
              <tr><td>ADMIN2Name</td><td>Admin2 area (department/cerlce/district)</td></tr>\
              <tr><td>ADMIN4Name</td><td>Community/village/disaggregated area</td></tr>\
              <tr><td>EnuSupervisorName</td><td>Name of supervisor</td></tr>\
              <tr><td>EnuName</td><td>Name of Enumerator</td></tr>\
              <tr><td>HHSizeCalc</td><td>Household size (Total number of household members)</td></tr>\
            </tbody>\
          </table>"
            )
          )
        )
      ),



      ########################
      # TAB 2: DATA UPLOAD
      ########################
      tabItem(tabName = "upload",
              fluidRow(
                box(width = 12,
                    selectInput("data_source", "Choose data source:",
                                choices = c(
                                  "Upload .sav file"         = "spss",
                                  "Load from MoDa API (.sav)" = "moda"
                                )
                    )
                )
              ),

              # SPSS file upload
              conditionalPanel(
                condition = "input.data_source == 'spss'",
                fluidRow(
                  box(
                    title = "Upload SPSS (.sav) Data", width = 6, status = "primary", solidHeader = TRUE,
                    fileInput("file", "Choose a .sav file", accept = ".sav"),
                    helpText("Allowed size up to 200 MB.")
                  ),
                  box(
                    title = "File Info", width = 6, status = "info", solidHeader = TRUE,
                    verbatimTextOutput("fileUploadedMessage")
                  )
                )
              ),

              # MoDa API inputs
              conditionalPanel(
                condition = "input.data_source == 'moda'",
                fluidRow(
                  box(
                    title = "MoDa API settings", width = 6, status = "warning", solidHeader = TRUE,
                    textInput("moda_formid", "Form ID",    placeholder = "e.g. 152205"),
                    textInput("moda_token",  "Bearer token"),
                    actionButton("moda_load", "Load from MoDa")
                  ),
                  box(
                    title = "MoDa status", width = 6, status = "info", solidHeader = TRUE,
                    verbatimTextOutput("moda_status")
                  )
                )
              ),

              # Preview & summary (same for both)
              fluidRow(
                box(
                  title = "Preview of Data", width = 12, status = "primary", solidHeader = TRUE,
                  DT::dataTableOutput("dataPreview")
                )
              ),
              fluidRow(
                box(
                  title = "Data Summary", status = "info", solidHeader = TRUE, width = 12,
                  fluidRow(
                    valueBoxOutput("obsBox", width = 4),
                    valueBoxOutput("varBox", width = 4),
                    valueBoxOutput("dupBox", width = 4)
                  )
                )
              )
      ),

      ########################
      # TAB 3: SURVEY PROGRESS
      ########################
      tabItem(tabName = "survey",
              fluidRow(
                tabBox(id = "surveyTabs", width = 12,

                       ## Sub-tab A) Submissions Over Time
                       tabPanel("Submissions Over Time",
                                fluidRow(
                                  box(
                                    title="Submissions Over Time", status="primary", solidHeader=TRUE, width=12,
                                    plotlyOutput("plotSubmission")
                                  )
                                )
                       ),

                       ## Sub-tab B) Count by Admin1
                       tabPanel("Submission by Admin1",
                                fluidRow(
                                  box(
                                    title="Submission by Admin1", status="primary", solidHeader=TRUE, width=12,
                                    plotlyOutput("plotAdm1")
                                  )
                                )
                       ),

                       ## Sub-tab C) Count by Admin2 (single Admin1 filter)
                       tabPanel("Submission by Admin2",
                                fluidRow(
                                  box(
                                    title="Select Admin1Name", status="warning", solidHeader=TRUE, width=4,
                                    selectInput("admin1Filter", "Admin1Name:",
                                                choices = character(0), # populated in server
                                                selected = NULL
                                    )
                                  ),
                                  box(
                                    title="Submission by Admin2", status="primary", solidHeader=TRUE, width=8,
                                    plotlyOutput("plotAdm2Filter")
                                  )
                                )
                       ),

                       ## Sub-tab D) Enumerator by Admin1Name (Histogram)
                       tabPanel("Submission by Enumerator by Admin level",
                                # first row: two filters side by side
                                fluidRow(
                                  box(
                                    title="Select Admin1Name", status="warning", solidHeader=TRUE, width=6,
                                    selectInput("admin1FilterEnum", "Admin1Name:",
                                                choices = character(0),
                                                selected = NULL)
                                  ),
                                  box(
                                    title="Select Admin2Name", status="warning", solidHeader=TRUE, width=6,
                                    selectInput("admin2FilterEnum", "Admin2Name:",
                                                choices = character(0),
                                                selected = NULL)
                                  )
                                ),
                                # second row: the histogram
                                fluidRow(
                                  box(
                                    title="Surveys by Enumerator (Histogram)", status="primary", solidHeader=TRUE, width=12,
                                    plotlyOutput("plotAdm1Enum")
                                  )
                                )
                       ),

                       ## Sub-tab E) Enumerator by Admin2Name (Treemap)
                       tabPanel("Submission by Enumerator (by Admin2) Treemap",
                                fluidRow(
                                  column(4,
                                         box(
                                           title="Select Enumerator", status="warning", solidHeader=TRUE, width=12,
                                           selectInput("filterEnumerator", "Enumerator:",
                                                       choices = character(0), # updated in server
                                                       selected = NULL,
                                                       multiple = FALSE
                                           )
                                         )
                                  ),
                                  column(8,
                                         box(
                                           title="Treemap by Admin2Name", status="primary", solidHeader=TRUE, width=12,
                                           textOutput("treemapCountEnumerator"),  # Show total # of surveys
                                           plotlyOutput("plotAdm2EnumTree")
                                         )
                                  )
                                )
                       )
                )
              )
      ),

      #########################################################################
      # TAB 4: FCS (with sub-tabs)
      #########################################################################
      tabItem(tabName = "fcs",
              fluidRow(
                tabBox(id = "fcsTabs", width = 12,

                       ## Sub-tab: FCS by Admin1
                       tabPanel("FCS by Admin1",
                                fluidRow(
                                  column(4,
                                         box(
                                           title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                           selectInput("fcsThresholdAdm1", "FCS Threshold:",
                                                       choices = c("FCSCat21", "FCSCat28"),
                                                       selected = "FCSCat21")
                                           # No Admin1 filter here, because you're showing all on one chart
                                         )
                                  ),
                                  column(8,
                                         box(
                                           title = "FCS by Admin1", width = 12, status = "primary", solidHeader = TRUE,
                                           plotlyOutput("plotFCSadm1")
                                         )
                                  )
                                )
                       ),

                       ## Sub-tab: FCS by Admin2
                       tabPanel("FCS by Admin2",
                                fluidRow(
                                  column(4,
                                         box(
                                           title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                           # Force user to pick exactly one Admin1
                                           selectInput("admin1FilterFCS2", "Admin1:",
                                                       choices = character(0),  # we'll populate from server
                                                       selected = NULL),
                                           selectInput("fcsThresholdAdm2", "FCS Threshold:",
                                                       choices = c("FCSCat21", "FCSCat28"),
                                                       selected = "FCSCat21")
                                         )
                                  ),
                                  column(8,
                                         box(
                                           title = "FCS by Admin2", width = 12, status = "primary", solidHeader = TRUE,
                                           plotlyOutput("plotFCSadm2")
                                         )
                                  )
                                )
                       ),

                       ## Sub-tab: FCS by Admin1/2 & Enumerator
                       tabPanel("FCS by Admin1/2 & Enumerator",
                                fluidRow(
                                  column(4,
                                         box(
                                           title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                           # Force user to pick exactly one Admin1
                                           selectInput("admin1FilterEnumFCS", "Admin1:",
                                                       choices = character(0),
                                                       selected = NULL),
                                           # Admin2 filter with "All" option
                                           selectInput("admin2FilterEnumFCS", "Admin2:",
                                                       choices = c("All"),  # we'll populate from server
                                                       selected = "All"),
                                           # FCS threshold filter
                                           selectInput("fcsThresholdAdm1Enum", "FCS Threshold:",
                                                       choices = c("FCSCat21","FCSCat28"),
                                                       selected = "FCSCat21")
                                         )
                                  ),
                                  column(8,
                                         box(
                                           title = "FCS by Admin1/2 & Enumerator", width = 12, status = "primary", solidHeader = TRUE,
                                           plotlyOutput("plotFCSadm1Enum")
                                         )
                                  )
                                )
                       ),

                       ## Sub-tab: Boxplot Median FCS by Enumerator
                       tabPanel("FCS Boxplot (Enumerator)",
                                fluidRow(
                                  box(
                                    title="Boxplot: Median FCS by Enumerator", status="primary", solidHeader=TRUE, width=12,
                                    plotlyOutput("plotFCSEnumNameBox")
                                  )
                                ),
                                fluidRow(
                                  box(
                                    title="Enumerators with Outlier Median FCS", width=12, status="info",
                                    tableOutput("tableFCSOutlier")
                                  )
                                )
                       ),

                       #######################################################################
                       ## ADDITIONAL SUB-TABS FOR TABLES
                       #######################################################################

                       ## Sub-tab: Households with Low Cereal Consumption
                       tabPanel("Low Cereal Consumption",
                                fluidRow(
                                  box(
                                    title="Households with Low Cereal Consumption", width=12, status="primary",
                                    dataTableOutput("tableCereal"),
                                    br(),
                                    downloadButton("downloadCereal","Dowload Low Cereal Table (Excel)")
                                  )
                                )
                       ),

                       ## Sub-tab: Cereal, Vegetable, and Dairy
                       tabPanel("Cereal, Vegetable, Dairy",
                                fluidRow(
                                  box(
                                    title="Cereal, Vegetable, and Dairy", width=12, status="primary",
                                    dataTableOutput("tableCerealVegDairy"),
                                    br(),
                                    downloadButton("downloadCerealVegDairy","Dowload Cereal/Veg/Dairy Table (Excel)")
                                  )
                                )
                       ),

                       ## Sub-tab: Low Vegetable Consumption
                       tabPanel("Low Vegetable Consumption",
                                fluidRow(
                                  box(
                                    title="Low Vegetable Consumption", width=12, status="primary",
                                    dataTableOutput("tableVeg"),
                                    br(),
                                    downloadButton("downloadVeg","Dowload Low Veg Table (Excel)")
                                  )
                                )
                       ),

                       ## Sub-tab: Meat, Oil, and Sugar Consumption
                       tabPanel("Meat, Oil, and Sugar",
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
      tabItem(tabName = "hdds",
              fluidRow(
                tabBox(id = "hddsTabs", width = 12,

                       ## Sub-tab: HDDS by Admin1
                       tabPanel("HDDS by Admin1",
                                fluidRow(
                                  box(
                                    title="HDDS (CH phases) by Admin1", width=12, status="primary",
                                    plotlyOutput("plotHDDSadm1")
                                  )
                                )
                       ),

                       ## Sub-tab: HDDS by Admin2
                       tabPanel("HDDS by Admin2",
                                fluidRow(
                                  column(4,
                                         box(
                                           title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                           # Force user to pick exactly one Admin1
                                           selectInput("admin1FilterHDDS2", "Admin1:",
                                                       choices = character(0),  # we'll populate from server
                                                       selected = NULL),
                                         )
                                  ),
                                  column(8,
                                         box(
                                           title = "HDDS (CH phases) by Admin2", width = 12, status = "primary", solidHeader = TRUE,
                                           plotlyOutput("plotHDDSadm2")
                                         )
                                  )
                                )
                       ),

                       ## Sub-tab: HDDS by Admin1/2 & Enumerator
                       tabPanel("HDDS by Admin1/2 & Enumerator",
                                fluidRow(
                                  column(4,
                                         box(
                                           title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                           # Force user to pick exactly one Admin1
                                           selectInput("admin1FilterEnumHDDS", "Admin1:",
                                                       choices = character(0),
                                                       selected = NULL),
                                           # Admin2 filter with "All" option
                                           selectInput("admin2FilterEnumHDDS", "Admin2:",
                                                       choices = c("All"),  # we'll populate from server
                                                       selected = "All"),
                                         )
                                  ),
                                  column(8,
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
      tabItem(tabName = "rcsi",
              fluidRow(
                tabBox(id = "rcsiTabs", width = 12,

                       ## Sub-tab: rCSI (CH phases) by Admin1
                       tabPanel("rCSI by Admin1",
                                fluidRow(
                                  box(
                                    title="rCSI (CH phases) by Admin1", width=12, status="primary",
                                    plotlyOutput("plotrCSIadm1")
                                  )
                                )
                       ),

                       ## Sub-tab: rCSI (CH phases) by Admin2
                       tabPanel("rCSI by Admin2",
                                fluidRow(
                                  column(4,
                                         box(
                                           title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                           # Force user to pick exactly one Admin1
                                           selectInput("admin1FilterrCSI2", "Admin1:",
                                                       choices = character(0),  # we'll populate from server
                                                       selected = NULL),
                                         )
                                  ),
                                  column(8,
                                         box(
                                           title = "rCSI (CH phases) by Admin2", width = 12, status = "primary", solidHeader = TRUE,
                                           plotlyOutput("plotrCSIadm2")
                                         )
                                  )
                                )
                       ),

                       ## Sub-tab: rCSI by Admin1/2 & Enumerator
                       tabPanel("rCSI by Admin1/2 & Enumerator",
                                fluidRow(
                                  column(4,
                                         box(
                                           title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                           # Force user to pick exactly one Admin1
                                           selectInput("admin1FilterEnumrCSI", "Admin1:",
                                                       choices = character(0),
                                                       selected = NULL),
                                           # Admin2 filter with "All" option
                                           selectInput("admin2FilterEnumrCSI", "Admin2:",
                                                       choices = c("All"),  # we'll populate from server
                                                       selected = "All"),
                                         )
                                  ),
                                  column(8,
                                         box(
                                           title = "rCSI (CH phases) by Admin1/2 & Enumerator", width = 12, status = "primary", solidHeader = TRUE,
                                           plotlyOutput("plotrCSIadm1Enum")
                                         )
                                  )
                                )
                       ),




                       ## Sub-tab: rCSI Boxplot & Outliers Admin1
                       tabPanel("rCSI Boxplot by Admin1",
                                fluidRow(
                                  column(12,
                                         box(
                                           title = "rCSI by Admin1", status = "primary", solidHeader = TRUE, width = 12,
                                           plotlyOutput("plotRCSIBoxAdmin1")
                                         )
                                  )
                                ),
                                fluidRow(
                                  column(12,
                                         box(
                                           title = "Outliers in rCSI by Admin1", status = "primary", solidHeader = TRUE, width = 12,
                                           DT::dataTableOutput("tableRCSIOutliersAdmin1"),
                                           br(),
                                           downloadButton("downloadRCSIOutliersAdmin1", "Download Outlier Table (Excel)")
                                         )
                                  )
                                )
                       ),

                       #Sub-tab: rCSI Boxplot & Outliers Admin2
                       tabPanel("rCSI Boxplot by Admin2",
                                fluidRow(
                                  column(4,
                                         box(
                                           title = "rCSI Filters", status = "warning", solidHeader = TRUE, width = 12,
                                           selectInput("rcsiBoxAdmin2Admin1",
                                                       "Select Admin1:",
                                                       choices = character(0),
                                                       selected = NULL)
                                         )
                                  ),
                                  column(8,
                                         box(
                                           title = "rCSI by Admin2", status = "primary", solidHeader = TRUE, width = 12,
                                           plotlyOutput("plotRCSIBoxAdmin2")
                                         )
                                  )
                                ),
                                fluidRow(
                                  column(12,
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
                       tabPanel("Triangulation",
                                fluidRow(
                                  box(
                                    title = "Triangulation Filters", status = "warning", solidHeader = TRUE, width = 4,
                                    selectInput("triangAdmin1", "Select Admin1:", choices = character(0)),
                                    # New Admin2 filter input; will be updated from server
                                    selectInput("triangAdmin2", "Select Admin2:", choices = c("All"), selected = "All"),
                                    # New FCSCat selector input
                                    selectInput("triangFCSCat", "Select FCS Category:",
                                                choices = c("FCSCat21", "FCSCat28"), selected = "FCSCat21")
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
                                    downloadButton("downloadTriangulation","Download Triangulaion (Excel)")
                                  )
                                )
                       )

                ) # end tabBox
              )
      ),

      #########################################################################
      # TAB 7: HHS (with sub-tabs)
      #########################################################################
      tabItem(tabName = "hhs",
              fluidRow(
                tabBox(id = "hhsTabs", width = 12,

                       ## Sub-tab: HHS by Admin1
                       tabPanel("HHS by Admin1",
                                fluidRow(
                                  box(
                                    title="HHS (CH phases) by Admin1", width=12, status="primary",
                                    plotlyOutput("plotHHSadm1")
                                  )
                                )
                       ),

                       ## Sub-tab: HHS by Admin2
                       tabPanel("HHS by Admin2",
                                fluidRow(
                                  column(4,
                                         box(
                                           title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                           # Force user to pick exactly one Admin1
                                           selectInput("admin1FilterHHS2", "Admin1:",
                                                       choices = character(0),  # we'll populate from server
                                                       selected = NULL),
                                         )
                                  ),
                                  column(8,
                                         box(
                                           title = "HHS (CH phases) by Admin2", width = 12, status = "primary", solidHeader = TRUE,
                                           plotlyOutput("plotHHSadm2")
                                         )
                                  )
                                )
                       ),

                       ## Sub-tab: HHS by Admin1 & Enumerator
                       tabPanel("HHS by Admin1/2 & Enumerator",
                                fluidRow(
                                  column(4,
                                         box(
                                           title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                           # Force user to pick exactly one Admin1
                                           selectInput("admin1FilterEnumHHS", "Admin1:",
                                                       choices = character(0),
                                                       selected = NULL),
                                           # Admin2 filter with "All" option
                                           selectInput("admin2FilterEnumHHS", "Admin2:",
                                                       choices = c("All"),  # we'll populate from server
                                                       selected = "All"),
                                         )
                                  ),
                                  column(8,
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
                  selectInput("matAdmin1","Admin1:", choices=NULL),
                  selectInput("matAdmin2","Admin2:", choices="All"),
                  selectInput("matFCSCat","FCS Category:",
                              choices=c("FCSCat21","FCSCat28"),
                              selected="FCSCat21")
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
      tabItem(tabName = "lcs",
              fluidRow(
                tabBox(id = "lcsTabs", width = 12,

                       ## Sub-tab: "Dynamic LCS" to select strategies
                       tabPanel("Dynamic LCS",
                                fluidRow(
                                  column(4,
                                         box(
                                           title="Select Strategies", status="warning", solidHeader=TRUE, width=12,

                                           # 1) Stress strategies
                                           selectInput("stressVars", "Pick EXACTLY 4 Stress Strategies:",
                                                       multiple = TRUE,
                                                       # No default 'selected' – starts empty
                                                       choices = c(
                                                         "Lcs_stress_Saving", "Lcs_stress_DomAsset", "Lcs_stress_ConsActive",
                                                         "Lcs_stress_SellFoodRation", "Lcs_stress_SellNFIRation", "Lcs_stress_EatOut",
                                                         "Lcs_stress_BorrowCash", "Lcs_stress_Pawn", "Lcs_stress_LessSchool",
                                                         "Lcs_stress_Utilities", "Lcs_stress_Edu", "Lcs_stress_BorrowFood",
                                                         "Lcs_stress_MoreLabour", "Lcs_stress_HHSeparation","Lcs_stress_Housing",
                                                         "Lcs_stress_LessSchool","LcsR_stress_Animals", "LcsR_stress_BorrowCash",
                                                         "LcsR_stress_Pawn",	"LcsR_stress_DomAsset",	"LcsR_stress_EatOut",
                                                         "LcsR_stress_LessSchool",	"LcsR_stress_Saving",	"LcsR_stress_HHSeparation",
                                                         "LcsR_stress_ConsActive",	"LcsR_stress_SellFoodRation",	"LcsR_stress_DomMigration",
                                                         "LcsR_stress_Housing",	"LcsR_stress_SellNFIRation",	"LcsR_stress_Edu"
                                                       )
                                           ),

                                           # 2) Crisis strategies
                                           selectInput("crisisVars", "Pick EXACTLY 3 Crisis Strategies:",
                                                       multiple = TRUE,
                                                       choices = c(
                                                         "Lcs_crisis_ProdAssets", "Lcs_crisis_Barter", "Lcs_crisis_Health",
                                                         "Lcs_crisis_Housing", "Lcs_crisis_HHSeparation", "Lcs_crisis_OutSchool",
                                                         "Lcs_crisis_Migration", "Lcs_crisis_DomMigration", "Lcs_crisis_ChildWork",
                                                         "Lcs_crisis_Edu_Health","Lcs_crisis_Barter","Lcs_crisis_ConsActive",
                                                         "Lcs_crisis_Edu","Lcs_crisis_Health","Lcs_crisis_Marriage","Lcs_crisis_Utilities",
                                                         "LcsR_crisis_AgriCare", "LcsR_crisis_ImmCrops",
                                                         "LcsR_crisis_Seed","LcsR_crisis_Animals",	"LcsR_crisis_Health",
                                                         "LcsR_crisis_Edu",	"LcsR_crisis_ProdAssets",	"LcsR_crisis_Housing",
                                                         "LcsR_crisis_HHSeparation",	"LcsR_crisis_Barter",	"LcsR_crisis_Migration",
                                                         "LcsR_crisis_ChildWork",	"LcsR_crisis_Marriage",	"LcsR_crisis_ConsActive",
                                                         "LcsR_crisis_OutSchool",	"LcsR_crisis_DomMigration"
                                                       )
                                           ),

                                           # 3) Emergency strategies
                                           selectInput("emergencyVars", "Pick EXACTLY 3 Emergency Strategies:",
                                                       multiple = TRUE,
                                                       choices = c(
                                                         "Lcs_em_ChildMigration", "Lcs_em_IllegalAct", "Lcs_em_Begged",
                                                         "Lcs_em_Marriage", "Lcs_em_ResAsset", "Lcs_em_Migration","Lcs_em_ChildWork",
                                                         "Lcs_em_OutSchool","LcsR_em_FemAnimal", "LcsR_em_WildFood", "LcsR_em_Seed",
                                                         "LcsR_em_OutSchool",	"LcsR_em_Migration",	"LcsR_em_ChildWork",	"LcsR_em_Marriage",
                                                         "LcsR_em_ResAsset",	"LcsR_em_Begged",	"LcsR_em_IllegalAct",	"LcsR_em_ChildMigration"
                                                       )
                                           )
                                         )
                                  ),
                                  column(8,
                                         box(title="Dynamic LCS Results", status="primary", solidHeader=TRUE, width=12,
                                             # We'll display a plot (or any other output) here
                                             plotlyOutput("plotDynamicLCS"),
                                             # Also display an info/warning text if there's a problem
                                             textOutput("dynamicLCSWarning")
                                         )
                                  )
                                )
                       ),

                       ## Sub-tab: LCS by Admin1
                       tabPanel("LCS by Admin1",
                                fluidRow(
                                  box(
                                    title="LCS by Admin1", width=12, status="primary",
                                    plotlyOutput("plotLHCSadm1")
                                  )
                                )
                       ),

                       ## Sub-tab: LCS by Admin2
                       tabPanel("LCS by Admin2",
                                fluidRow(
                                  column(4,
                                         box(
                                           title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                           # Force user to pick exactly one Admin1
                                           selectInput("admin1FilterLCS2", "Admin1:",
                                                       choices = character(0),  # we'll populate from server
                                                       selected = NULL),
                                         )
                                  ),
                                  column(8,
                                         box(
                                           title = "LCS by Admin2", width = 12, status = "primary", solidHeader = TRUE,
                                           plotlyOutput("plotLHCSadm2")
                                         )
                                  )
                                )
                       ),

                       ## Sub-tab: LCS by Admin1 & Enumerator
                       tabPanel("LCS by Admin1/2 & Enumerator",
                                fluidRow(
                                  column(4,
                                         box(
                                           title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                           # Force user to pick exactly one Admin1
                                           selectInput("admin1FilterEnumLCS", "Admin1:",
                                                       choices = character(0),
                                                       selected = NULL),
                                           # Admin2 filter with "All" option
                                           selectInput("admin2FilterEnumLCS", "Admin2:",
                                                       choices = c("All"),  # we'll populate from server
                                                       selected = "All"),
                                         )
                                  ),
                                  column(8,
                                         box(
                                           title = "LCS by Admin1/2 & Enumerator", width = 12, status = "primary", solidHeader = TRUE,
                                           plotlyOutput("plotLHCSadm1Enum")
                                         )
                                  )
                                )
                       ),

                       # Sub-tab: LCS by Strategy
                       tabPanel("LCS by Strategy",
                                fluidRow(
                                  column(4,
                                         box(
                                           title = "Filters", status = "warning", solidHeader = TRUE, width = 12,
                                           selectInput("lcsStrategyAdmin1", "Select Admin1:",
                                                       choices = character(0), selected = NULL),
                                           selectInput("lcsStrategyAdmin2", "Select Admin2:",
                                                       choices = c("All"), selected = "All"),
                                           selectInput("lcsStrategyEnu", "Select Enumerator:",
                                                       choices = c("All"), selected = "All")
                                         )
                                  ),
                                  column(8,
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
      tabItem(tabName = "fes",
              fluidRow(
                tabBox(id = "fesTabs", width = 12,

                       # 1) Sub-tab: FES by Admin1
                       tabPanel("FES by Admin1",
                                fluidRow(
                                  column(4,
                                         box(title = "FES Filters", status = "warning", solidHeader = TRUE, width = 12,
                                             # Let user pick recall period
                                             selectInput("fesRecall", "Recall Period:",
                                                         choices = c("7 days" = 7, "1 month" = 30),
                                                         selected = 30
                                             )
                                         )
                                  ),
                                  column(8,
                                         box(title="Food Expenditure Share (FES) by Admin1", status="primary", solidHeader=TRUE, width=12,
                                             plotlyOutput("plotFESadm1")
                                         )
                                  )
                                )
                       ),

                       # 2) Sub-tab: FES by Admin2 (force user to pick an Admin1)
                       tabPanel("FES by Admin2",
                                fluidRow(
                                  column(4,
                                         box(title = "FES Filters", status="warning", solidHeader=TRUE, width=12,
                                             selectInput("admin1FilterFES2", "Select Admin1:",
                                                         choices = character(0), selected = NULL)
                                         )
                                  ),
                                  column(8,
                                         box(title="Food Expenditure Share (FES) by Admin2", status="primary", solidHeader=TRUE, width=12,
                                             plotlyOutput("plotFESadm2")
                                         )
                                  )
                                )
                       ),

                       # 3) Sub-tab: FES by Admin1 & Enumerator
                       tabPanel("FES by Admin1/2 & Enumerator",
                                fluidRow(
                                  column(4,
                                         box(title="FES Filters", status="warning", solidHeader=TRUE, width=12,
                                             selectInput("admin1FilterEnumFES", "Select Admin1:",
                                                         choices = character(0), selected = NULL),
                                             selectInput("admin2FilterEnumFES", "Select Admin2:",
                                                         choices = c("All"), selected = "All")
                                         )
                                  ),
                                  column(8,
                                         box(title="Food Expenditure Share (FES) by Admin1/2 & Enumerator", status="primary", solidHeader=TRUE, width=12,
                                             plotlyOutput("plotFESadm1Enum")
                                         )
                                  )
                                )
                       ),
                       # 4) Sub-tab: Food Expenses by Admin1 (Boxplot and Outliers)
                       tabPanel("FES Food Expenses by Admin1",
                                fluidRow(
                                  # Remove the Admin1 selection dropdown (no filtering needed at Admin1 level)
                                  column(12,
                                         box(title = "Monthly Food Expenses by Admin1", status = "primary", solidHeader = TRUE, width = 12,
                                             plotlyOutput("plotFESBoxAdmin1")
                                         )
                                  )
                                ),
                                fluidRow(
                                  box(title = "Outlier Table: FES Food Expenses by Admin1", status = "primary", solidHeader = TRUE, width = 12,
                                      dataTableOutput("tableFESOutliersAdmin1"),
                                      br(),
                                      downloadButton("downloadOutlierTableAdmin1", "Export to Excel")
                                  )
                                )
                       ),
                       # 5) Sub-tab: Food Expenses by Admin2 (Boxplot and Outliers)

                       tabPanel("FES Food Expenses by Admin2",
                                fluidRow(
                                  box(
                                    title = "FES Filters", status = "warning", solidHeader = TRUE, width = 4,
                                    # This filter lets the user choose an Admin1.
                                    selectInput("fesBoxAdmin2Admin1",
                                                "Select Admin1:",
                                                choices = character(0),  # will be updated in the server
                                                selected = NULL)
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
                       tabPanel("Zero Food Expenditure",
                                DT::dataTableOutput("zeroExpTable")
                       )


                ) # end tabBox
              )
      ),

      #########################################################################
      # TAB 11: CARI (with sub-tabs)
      #########################################################################
      tabItem(tabName = "cari",
              fluidRow(
                tabBox(id = "cariTabs", width = 12,

                       # A) CARI Console
                       tabPanel("CARI Console",
                                fluidRow(
                                  column(4,
                                         box(
                                           title="Select FCS Threshold for CARI", status="warning", solidHeader=TRUE, width=12,
                                           selectInput("cariFcsThreshold", "FCS Threshold:",
                                                       choices=c("21"="FCSCat21","28"="FCSCat28"), selected="FCSCat21")
                                         )
                                  ),
                                  column(8,
                                         box(title="CARI Console Table", status="primary", width=12,
                                             htmlOutput("cariCtable")
                                         )
                                  )
                                )
                       ),

                       # B) CARI by Admin1
                       tabPanel("CARI by Admin1",
                                fluidRow(
                                  box(
                                    title="CARI by Admin1", width=12, status="primary",
                                    plotlyOutput("plotCARIadm1")
                                  )
                                )
                       ),

                       # C) CARI by Admin2
                       tabPanel("CARI by Admin2",
                                fluidRow(
                                  column(4,
                                         box(
                                           title="Filters", width=12, status="warning",
                                           selectInput("admin1FilterCARI2","Admin1:", choices=character(0), selected=NULL)
                                         )
                                  ),
                                  column(8,
                                         box(title="CARI by Admin2", width=12, status="primary",
                                             plotlyOutput("plotCARIadm2")
                                         )
                                  )
                                )
                       ),

                       # D) CARI by Admin1 & Enumerator
                       tabPanel("CARI by Admin1/2 & Enumerator",
                                fluidRow(
                                  column(4,
                                         box(
                                           title="Filters", width=12, status="warning",
                                           selectInput("admin1FilterEnumCARI","Admin1:", choices=character(0), selected=NULL),
                                           selectInput("admin2FilterEnumCARI","Admin2:", choices=c("All"), selected="All")
                                         )
                                  ),
                                  column(8,
                                         box(title="CARI by Admin1/2 & Enumerator", width=12, status="primary",
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

############################
## SERVER
############################
server <- function(input, output, session) {

  round_half_up <- function(x) {
    floor(x + 0.5)
  }

  ###########################################################################
  # 1) READ & PREPARE DATA
  ###########################################################################
  # -- SPSS upload reactive
  spss_data <- reactive({
    req(input$data_source == "spss", input$file)
    ext <- tools::file_ext(input$file$name)
    shiny::validate(
      shiny::need(ext == "sav", "Please upload an SPSS .sav file")
    )
    haven::read_sav(input$file$datapath)
  })
  output$fileUploadedMessage <- renderPrint({
    req(spss_data())
    str(spss_data())
  })

  # -- MoDa API: download .sav, read it once user clicks
  # reactiveVal to track any errors
  moda_error <- reactiveVal(NULL)

  moda_data <- eventReactive(input$moda_load, {
    req(input$data_source == "moda",
        input$moda_formid, input$moda_token)

    # clear previous error
    moda_error(NULL)

    tryCatch({
      # 1) Kick off the async export
      init_url <- sprintf(
        "https://api.moda.wfp.org/api/v1/forms/%s/export_async?format=savzip",
        input$moda_formid
      )
      init_resp <- httr::GET(
        init_url,
        httr::add_headers(Authorization = paste("Token", input$moda_token))
      )
      httr::stop_for_status(init_resp, "initializing export")

      init_json <- httr::content(init_resp, as = "parsed", type = "application/json")

      # 2) Determine where to GET the ZIP from
      if (!is.null(init_json$export_url) && isTRUE(init_json$job_status == "SUCCESS")) {
        export_url <- init_json$export_url

      } else if (!is.null(init_json$job_uuid) && isTRUE(init_json$job_status == "PENDING")) {
        job_uuid <- init_json$job_uuid

        # Poll until SUCCESS or FAILED
        repeat {
          poll_url    <- sprintf(
            "https://api.moda.wfp.org/api/v1/forms/%s/export_async?job_uuid=%s",
            input$moda_formid, job_uuid
          )
          status_resp <- httr::GET(
            poll_url,
            httr::add_headers(Authorization = paste("Token", input$moda_token))
          )
          httr::stop_for_status(status_resp, "polling export job")
          status_json <- httr::content(status_resp, as = "parsed", type = "application/json")

          if (identical(status_json$job_status, "SUCCESS")) {
            export_url <- status_json$export_url
            break
          }
          if (identical(status_json$job_status, "FAILED")) {
            stop("Export failed: ", status_json$error_message)
          }
          Sys.sleep(2)
        }

      } else {
        stop("Unexpected init response: status = ",
             init_json$job_status)
      }

      # 3) Download the ZIP to a temp file
      tmpzip <- tempfile(fileext = ".zip")
      httr::GET(
        export_url,
        httr::add_headers(Authorization = paste("Token", input$moda_token)),
        httr::write_disk(tmpzip, overwrite = TRUE)
      ) %>% httr::stop_for_status("downloading ZIP")

      # 4) Ensure data.sav is present and unzip it
      zip_list <- utils::unzip(tmpzip, list = TRUE)$Name
      if (!"data.sav" %in% zip_list) {
        stop("`data.sav` not found in ZIP archive")
      }
      tmpdir <- tempfile()
      utils::unzip(tmpzip, files = "data.sav", exdir = tmpdir)
      sav_path <- file.path(tmpdir, "data.sav")

      # 5) Read and return
      haven::read_sav(sav_path)

    }, error = function(e) {
      moda_error(e$message)
      showNotification(
        paste("Error loading data from MoDa:", e$message),
        type = "error", duration = 10
      )
      NULL
    })
  })

  #Alert message for MoDa data upload.
  output$moda_status <- renderText({
    req(input$moda_load)
    if (!is.null(moda_error())) {
      paste("❌", moda_error())
    } else if (is.null(moda_data()) || nrow(moda_data()) == 0) {
      "⚠️ No rows returned from MoDa."
    } else {
      paste("✅ Loaded", nrow(moda_data()), "rows from MoDa.")
    }
  })

  # -- unified reactive that picks whichever source
  dataset <- reactive({
    # 1) Pull in the raw data (from SPSS upload or MoDa)
    df <- switch(input$data_source,
                 spss = spss_data(),
                 moda = moda_data()
    )

    # --- 1) List every variable your app expects for FCS, HDDS, rCSI, HHS, etc. ---
    calc_vars <- c(
      # FCS components
      "FCSStap", "FCSPulse", "FCSPr", "FCSVeg", "FCSFruit",
      "FCSDairy", "FCSFat", "FCSSugar", "FCS",
      # HDDS components
      "HDDSStapCer", "HDDSStapRoot", "HDDSVeg", "HDDSFruit",
      "HDDSPrEggs", "HDDSPrFish", "HDDSPulse", "HDDSDairy",
      "HDDSFat", "HDDSSugar", "HDDSCond", "HDDS",
      # rCSI components
      "rCSILessQlty", "rCSIBorrow", "rCSIMealSize",
      "rCSIMealAdult", "rCSIMealNb", "rCSI",
      # HHS components
      "HHSNoFood_FR", "HHSBedHung_FR", "HHSNotEat_FR",
      "HHS",
      #Food expenditure components
      "HHExpFCer_Purch_MN_7D", "HHExpFCer_GiftAid_MN_7D", "HHExpFCer_Own_MN_7D",
      "HHExpFTub_Purch_MN_7D", "HHExpFTub_GiftAid_MN_7D", "HHExpFTub_Own_MN_7D",
      "HHExpFPuls_Purch_MN_7D", "HHExpFPuls_GiftAid_MN_7D", "HHExpFPuls_Own_MN_7D",
      "HHExpFVeg_Purch_MN_7D", "HHExpFVeg_GiftAid_MN_7D", "HHExpFVeg_Own_MN_7D",
      "HHExpFFrt_Purch_MN_7D", "HHExpFFrt_GiftAid_MN_7D", "HHExpFFrt_Own_MN_7D",
      "HHExpFAnimMeat_Purch_MN_7D", "HHExpFAnimMeat_GiftAid_MN_7D", "HHExpFAnimMeat_Own_MN_7D",
      "HHExpFAnimFish_Purch_MN_7D", "HHExpFAnimFish_GiftAid_MN_7D", "HHExpFAnimFish_Own_MN_7D",
      "HHExpFFats_Purch_MN_7D", "HHExpFFats_GiftAid_MN_7D", "HHExpFFats_Own_MN_7D",
      "HHExpFDairy_Purch_MN_7D", "HHExpFDairy_GiftAid_MN_7D", "HHExpFDairy_Own_MN_7D",
      "HHExpFEgg_Purch_MN_7D", "HHExpFEgg_GiftAid_MN_7D", "HHExpFEgg_Own_MN_7D",
      "HHExpFSgr_Purch_MN_7D", "HHExpFSgr_GiftAid_MN_7D", "HHExpFSgr_Own_MN_7D",
      "HHExpFCond_Purch_MN_7D", "HHExpFCond_GiftAid_MN_7D", "HHExpFCond_Own_MN_7D",
      "HHExpFBev_Purch_MN_7D", "HHExpFBev_GiftAid_MN_7D", "HHExpFBev_Own_MN_7D",
      "HHExpFOut_Purch_MN_7D", "HHExpFOut_GiftAid_MN_7D", "HHExpFOut_Own_MN_7D",
      "HHExpFCer_Purch_MN_1M", "HHExpFCer_GiftAid_MN_1M", "HHExpFCer_Own_MN_1M",
      "HHExpFTub_Purch_MN_1M", "HHExpFTub_GiftAid_MN_1M", "HHExpFTub_Own_MN_1M",
      "HHExpFPuls_Purch_MN_1M", "HHExpFPuls_GiftAid_MN_1M", "HHExpFPuls_Own_MN_1M",
      "HHExpFVeg_Purch_MN_1M", "HHExpFVeg_GiftAid_MN_1M", "HHExpFVeg_Own_MN_1M",
      "HHExpFFrt_Purch_MN_1M", "HHExpFFrt_GiftAid_MN_1M", "HHExpFFrt_Own_MN_1M",
      "HHExpFAnimMeat_Purch_MN_1M", "HHExpFAnimMeat_GiftAid_MN_1M", "HHExpFAnimMeat_Own_MN_1M",
      "HHExpFAnimFish_Purch_MN_1M", "HHExpFAnimFish_GiftAid_MN_1M", "HHExpFAnimFish_Own_MN_1M",
      "HHExpFFats_Purch_MN_1M", "HHExpFFats_GiftAid_MN_1M", "HHExpFFats_Own_MN_1M",
      "HHExpFDairy_Purch_MN_1M", "HHExpFDairy_GiftAid_MN_1M", "HHExpFDairy_Own_MN_1M",
      "HHExpFEgg_Purch_MN_1M", "HHExpFEgg_GiftAid_MN_1M", "HHExpFEgg_Own_MN_1M",
      "HHExpFSgr_Purch_MN_1M", "HHExpFSgr_GiftAid_MN_1M", "HHExpFSgr_Own_MN_1M",
      "HHExpFCond_Purch_MN_1M", "HHExpFCond_GiftAid_MN_1M", "HHExpFCond_Own_MN_1M",
      "HHExpFBev_Purch_MN_1M", "HHExpFBev_GiftAid_MN_1M", "HHExpFBev_Own_MN_1M",
      "HHExpFOut_Purch_MN_1M", "HHExpFOut_GiftAid_MN_1M", "HHExpFOut_Own_MN_1M"
    )

    # --- 2) For any of these that already exist, zap labels & coerce numeric;
    #        for any that are missing, fill in a column of zeros. ---
    for (v in calc_vars) {
      if (v %in% names(df)) {
        # drop SPSS labels and make sure it’s numeric
        df[[v]] <- as.numeric(haven::zap_labels(df[[v]]))
      } else {
        # create a zero‐column so sums/etc. don’t break
        df[[v]] <- 0
      }
    }


    req(df)  # ensure we have a data.frame

    # 2) If RESPConsent is present, zap its labels and keep only consenters
    if ("RESPConsent" %in% names(df)) {
      # turn any labelled vector into its numeric codes
      df$RESPConsent <- haven::zap_labels(df$RESPConsent)

      # keep only rows where RESPConsent == 1
      df <- df[df$RESPConsent == 1, , drop = FALSE]
    }

    # 3) Return (filtered or not) for downstream processing
    df
  })



  # -- downstream: same processing for both
  processed <- reactive({
    df <- dataset()
    req(df)  # make sure we have data

    # your entire pipeline in one place

    # Example debug: check structure + labels of one LCS variable
    str(df$Lcs_stress_DomAsset)
    print(attr(df$Lcs_stress_DomAsset, "labels"))

    # 1) Zap labels on LCS columns and HDDS so we see numeric codes (10,20,30,9999, etc.)
    df$Lcs_stress_DomAsset     <- haven::zap_labels(df$Lcs_stress_DomAsset)
    df$Lcs_stress_Saving       <- haven::zap_labels(df$Lcs_stress_Saving)
    df$Lcs_stress_EatOut       <- haven::zap_labels(df$Lcs_stress_EatOut)
    df$Lcs_stress_BorrowCash   <- haven::zap_labels(df$Lcs_stress_BorrowCash)
    df$Lcs_stress_ConsActive   <- haven::zap_labels(df$Lcs_stress_ConsActive)
    df$Lcs_stress_SellFoodRation <- haven::zap_labels(df$Lcs_stress_SellFoodRation)
    df$Lcs_stress_SellNFIRation  <- haven::zap_labels(df$Lcs_stress_SellNFIRation)
    df$Lcs_stress_Pawn           <- haven::zap_labels(df$Lcs_stress_Pawn)
    df$Lcs_stress_LessSchool     <- haven::zap_labels(df$Lcs_stress_LessSchool)
    df$Lcs_stress_Utilities      <- haven::zap_labels(df$Lcs_stress_Utilities)
    df$Lcs_stress_Edu            <- haven::zap_labels(df$Lcs_stress_Edu)
    df$Lcs_stress_BorrowFood     <- haven::zap_labels(df$Lcs_stress_BorrowFood)
    df$Lcs_stress_MoreLabour     <- haven::zap_labels(df$Lcs_stress_MoreLabour)
    df$Lcs_stress_LessSchool     <- haven::zap_labels(df$Lcs_stress_LessSchool)
    df$Lcs_stress_Housing     <- haven::zap_labels(df$Lcs_stress_Housing)
    df$Lcs_stress_MoreLabour     <- haven::zap_labels(df$Lcs_stress_MoreLabour)
    df$LcsR_stress_Animals       <- haven::zap_labels(df$LcsR_stress_Animals)
    df$LcsR_stress_BorrowCash    <- haven::zap_labels(df$LcsR_stress_BorrowCash)
    df$LcsR_stress_Pawn          <- haven::zap_labels(df$LcsR_stress_Pawn)
    df$LcsR_stress_DomAsset      <- haven::zap_labels(df$LcsR_stress_DomAsset)
    df$LcsR_stress_EatOut        <- haven::zap_labels(df$LcsR_stress_EatOut)
    df$LcsR_stress_LessSchool    <- haven::zap_labels(df$LcsR_stress_LessSchool)
    df$LcsR_stress_Saving        <- haven::zap_labels(df$LcsR_stress_Saving)
    LcsR_stress_HHSeparation     <- haven::zap_labels(df$LcsR_stress_HHSeparation)
    df$LcsR_stress_ConsActive    <- haven::zap_labels(df$LcsR_stress_ConsActive)
    df$LcsR_stress_SellFoodRation  <- haven::zap_labels(df$LcsR_stress_SellFoodRation)
    df$LcsR_stress_DomMigration  <- haven::zap_labels(df$LcsR_stress_DomMigration)
    df$LcsR_stress_Housing	     <- haven::zap_labels(df$LcsR_stress_Housing)
    df$LcsR_stress_SellNFIRation  <- haven::zap_labels(df$LcsR_stress_SellNFIRation)
    df$LcsR_stress_Edu           <- haven::zap_labels(df$LcsR_stress_Edu)

    df$Lcs_crisis_ProdAssets     <- haven::zap_labels(df$Lcs_crisis_ProdAssets)
    df$Lcs_crisis_Health         <- haven::zap_labels(df$Lcs_crisis_Health)
    df$Lcs_crisis_OutSchool      <- haven::zap_labels(df$Lcs_crisis_OutSchool)
    df$Lcs_crisis_Barter         <- haven::zap_labels(df$Lcs_crisis_Barter)
    df$Lcs_crisis_Housing        <- haven::zap_labels(df$Lcs_crisis_Housing)
    df$Lcs_crisis_HHSeparation   <- haven::zap_labels(df$Lcs_crisis_HHSeparation)
    df$Lcs_crisis_Migration      <- haven::zap_labels(df$Lcs_crisis_Migration)
    df$Lcs_crisis_DomMigration   <- haven::zap_labels(df$Lcs_crisis_DomMigration)
    df$Lcs_crisis_ChildWork      <- haven::zap_labels(df$Lcs_crisis_ChildWork)
    df$Lcs_crisis_Edu_Health     <- haven::zap_labels(df$Lcs_crisis_Edu_Health)
    df$Lcs_crisis_Barter     <- haven::zap_labels(df$Lcs_crisis_Barter)
    df$Lcs_crisis_ConsActive     <- haven::zap_labels(df$Lcs_crisis_ConsActive)
    df$Lcs_crisis_Edu     <- haven::zap_labels(df$Lcs_crisis_Edu)
    df$Lcs_crisis_Health     <- haven::zap_labels(df$Lcs_crisis_Health)
    df$Lcs_crisis_Marriage     <- haven::zap_labels(df$Lcs_crisis_Marriage)
    df$Lcs_crisis_Utilities     <- haven::zap_labels(df$Lcs_crisis_Utilities)
    df$LcsR_crisis_AgriCare      <- haven::zap_labels(df$LcsR_crisis_AgriCare)
    df$LcsR_crisis_ImmCrops      <- haven::zap_labels(df$LcsR_crisis_ImmCrops)
    df$LcsR_crisis_Seed          <- haven::zap_labels(df$LcsR_crisis_Seed)
    df$LcsR_crisis_Animals       <- haven::zap_labels(df$LcsR_crisis_Animals)
    df$LcsR_crisis_Health        <- haven::zap_labels(df$LcsR_crisis_Health)
    df$LcsR_crisis_Edu           <- haven::zap_labels(df$LcsR_crisis_Edu)
    df$LcsR_crisis_ProdAssets    <- haven::zap_labels(df$LcsR_crisis_ProdAssets)
    df$LcsR_crisis_Housing       <- haven::zap_labels(df$LcsR_crisis_Housing)
    df$LcsR_crisis_HHSeparation  <- haven::zap_labels(df$LcsR_crisis_HHSeparation)
    df$LcsR_crisis_Barter        <- haven::zap_labels(df$LcsR_crisis_Barter)
    df$LcsR_crisis_Migration     <- haven::zap_labels(df$LcsR_crisis_Migration)
    df$LcsR_crisis_ChildWork     <- haven::zap_labels(df$LcsR_crisis_ChildWork)
    df$LcsR_crisis_Marriage      <- haven::zap_labels(df$LcsR_crisis_Marriage)
    df$LcsR_crisis_ConsActive    <- haven::zap_labels(df$LcsR_crisis_ConsActive)
    df$LcsR_crisis_OutSchool     <- haven::zap_labels(df$LcsR_crisis_OutSchool)
    df$LcsR_crisis_DomMigration  <- haven::zap_labels(df$LcsR_crisis_DomMigration)

    df$Lcs_em_ResAsset           <- haven::zap_labels(df$Lcs_em_ResAsset)
    df$Lcs_em_Begged             <- haven::zap_labels(df$Lcs_em_Begged)
    df$Lcs_em_IllegalAct         <- haven::zap_labels(df$Lcs_em_IllegalAct)
    df$Lcs_em_ChildMigration     <- haven::zap_labels(df$Lcs_em_ChildMigration)
    df$Lcs_em_Marriage           <- haven::zap_labels(df$Lcs_em_Marriage)
    df$Lcs_em_Migration          <- haven::zap_labels(df$Lcs_em_Migration)
    df$Lcs_em_ChildWork          <- haven::zap_labels(df$Lcs_em_ChildWork)
    df$Lcs_em_OutSchool          <- haven::zap_labels(df$Lcs_em_OutSchool)
    df$LcsR_em_FemAnimal         <- haven::zap_labels(df$LcsR_em_FemAnimal)
    df$LcsR_em_WildFood          <- haven::zap_labels(df$LcsR_em_WildFood)
    df$LcsR_em_Seed              <- haven::zap_labels(df$LcsR_em_Seed)
    df$LcsR_em_OutSchool         <- haven::zap_labels(df$LcsR_em_OutSchool)
    df$LcsR_em_Migration         <- haven::zap_labels(df$LcsR_em_Migration)
    df$LcsR_em_ChildWork         <- haven::zap_labels(df$LcsR_em_ChildWork)
    df$LcsR_em_Marriage          <- haven::zap_labels(df$LcsR_em_Marriage)
    df$LcsR_em_ResAsset          <- haven::zap_labels(df$LcsR_em_ResAsset)
    df$LcsR_em_Begged	           <- haven::zap_labels(df$LcsR_em_Begged)
    df$LcsR_em_IllegalAct        <- haven::zap_labels(df$LcsR_em_IllegalAct)
    df$LcsR_em_ChildMigration    <- haven::zap_labels(df$LcsR_em_ChildMigration)

    df$HDDSStapCer     <- haven::zap_labels(df$HDDSStapCer)
    df$HDDSStapRoot         <- haven::zap_labels(df$HDDSStapRoot)
    df$HDDSPulse      <- haven::zap_labels(df$HDDSPulse)
    df$HDDSDairy         <- haven::zap_labels(df$HDDSDairy)
    df$HDDSPrMeatF        <- haven::zap_labels(df$HDDSPrMeatF)
    df$HDDSPrMeatO   <- haven::zap_labels(df$HDDSPrMeatO)
    df$HDDSPrMeat   <- haven::zap_labels(df$HDDSPrMeat)
    df$HDDSPrFish      <- haven::zap_labels(df$HDDSPrFish)
    df$HDDSPrEggs   <- haven::zap_labels(df$HDDSPrEggs)
    df$HDDSVeg      <- haven::zap_labels(df$HDDSVeg)
    df$HDDSFruit     <- haven::zap_labels(df$HDDSFruit)
    df$HDDSFat      <- haven::zap_labels(df$HDDSFat)
    df$HDDSSugar      <- haven::zap_labels(df$HDDSSugar)
    df$HDDSCond          <- haven::zap_labels(df$HDDSCond)

    df$EnuSupervisorName <- haven::zap_labels(df$EnuSupervisorName)

    # Example freq table
    table(df$Lcs_stress_DomAsset, useNA="ifany")

    # 2) Convert enumerator and supervisor name to character (exclude value labels)
    df <- df %>%
      set_value_labels(EnuName = NULL) %>%
      to_factor()
    df$EnuName <- as.character(df$EnuName)

    df <- df %>%
      set_value_labels(EnuSupervisorName = NULL) %>%
      to_factor()
    df$EnuSupervisorName <- as.character(df$EnuSupervisorName)

    # Convert ADMIN1Name & ADMIN2Name to text
    df <- df %>%
      set_value_labels(ADMIN2Name = NULL) %>%
      to_factor() %>%
      mutate(
        ADMIN2Name = as.character(ADMIN2Name),
        ADMIN1Name = as.character(ADMIN1Name)
      )

    # If dataset has "@_submission_time", split date & hour
    if ("@_submission_time" %in% names(df)) {
      df <- df %>%
        separate(`@_submission_time`, c("Survey_date", "survey_hour"), sep = " ")
      df$Survey_date <- as.Date(df$Survey_date)
    } else {
      df$Survey_date <- Sys.Date()
    }

    #Indicators calculation Steps

    # ------------------------------------------------------------------
    # FCS Calculation
    # ------------------------------------------------------------------
    df <- df %>%
      mutate(
        FCS = (2 * FCSStap) + (3 * FCSPulse) + (4 * FCSPr) + FCSVeg + FCSFruit +
          (4 * FCSDairy) + (0.5 * FCSFat) + (0.5 * FCSSugar),
        FCSCat21 = case_when(
          FCS <= 21                ~ "Poor",
          between(FCS, 21.5, 35)   ~ "Borderline",
          FCS > 35                 ~ "Acceptable"
        ),
        FCSCat28 = case_when(
          FCS <= 28                ~ "Poor",
          between(FCS, 28, 42)     ~ "Borderline",
          FCS > 42                 ~ "Acceptable"
        )
      )

    # ------------------------------------------------------------------
    # HDDS
    # ------------------------------------------------------------------
    # 1) Create composite meat field if needed
    if (all(c("HDDSPrMeatF","HDDSPrMeatO") %in% names(df))) {
      df <- df %>%
        mutate(
          # zap and test both F and O
          HDDSPrMeat = if_else(
            haven::zap_labels(HDDSPrMeatF) == 1 |
              haven::zap_labels(HDDSPrMeatO) == 1,
            1, 0, missing = 0
          )
        )
    } else if ("HDDSPrMeat" %in% names(df)) {
      df <- df %>%
        mutate(
          HDDSPrMeat = if_else(
            haven::zap_labels(HDDSPrMeat) == 1,
            1, 0, missing = 0
          )
        )
    }

    # 2) List all the “core” HDDS vars we might recode
    hdds_core <- c(
      "HDDSStapCer", "HDDSStapRoot", "HDDSVeg", "HDDSFruit",
      "HDDSPrEggs", "HDDSPrFish", "HDDSPulse",
      "HDDSDairy", "HDDSFat", "HDDSSugar", "HDDSCond"
    )

    # 3) Restrict to those actually present
    present_core <- intersect(names(df), hdds_core)

    # 4) Zap labels + binarize each present core var
    df <- df %>%
      mutate(
        across(
          all_of(present_core),
          ~ if_else(haven::zap_labels(.x) == 1, 1, 0, missing = 0)
        )
      )

    # 5) Now sum everything that’s present (including HDDSPrMeat if created)
    to_sum <- intersect(
      names(df),
      c(present_core, "HDDSPrMeat")
    )

    df <- df %>%
      mutate(
        HDDS = rowSums(across(all_of(to_sum)), na.rm = TRUE),
        HDDS_CH = case_when(
          HDDS >= 5 ~ "Phase 1 (>=5 Food Groups)",
          HDDS == 4 ~ "Phase 2 (=4 Food Groups)",
          HDDS == 3 ~ "Phase 3 (=3 Food Groups)",
          HDDS == 2 ~ "Phase 4 (=2 Food Groups)",
          HDDS <  2 ~ "Phase 5 (<=1 Food Groups)"
        )
      )

    # ------------------------------------------------------------------
    # rCSI
    # ------------------------------------------------------------------
    df <- df %>%
      mutate(
        rCSI = rCSILessQlty + (2 * rCSIBorrow) + rCSIMealSize +
          (3 * rCSIMealAdult) + rCSIMealNb,
        rCSI_CH = case_when(
          rCSI <= 3          ~ "Phase 1 (0-3)",
          between(rCSI,4,18) ~ "Phase 2 (4-18)",
          rCSI >= 19         ~ "Phase 3 (>=19)"
        )
      )

    # ------------------------------------------------------------------
    # HHS
    # ------------------------------------------------------------------
    df <- df %>%
      set_value_labels(
        HHSNoFood_FR = NULL, HHSBedHung_FR = NULL, HHSNotEat_FR = NULL
      ) %>%
      mutate(
        HHSNoFood_FR  = as.numeric(HHSNoFood_FR),
        HHSBedHung_FR = as.numeric(HHSBedHung_FR),
        HHSNotEat_FR  = as.numeric(HHSNotEat_FR)
      ) %>%
      mutate(
        HHhSNoFood_FR_r = case_when(
          HHSNoFood_FR == 1 ~ 1,
          HHSNoFood_FR == 2 ~ 1,
          HHSNoFood_FR == 3 ~ 2,
          TRUE ~ 0
        ),
        HHhSBedHung_FR_r = case_when(
          HHSBedHung_FR == 1 ~ 1,
          HHSBedHung_FR == 2 ~ 1,
          HHSBedHung_FR == 3 ~ 2,
          TRUE ~ 0
        ),
        HHhSNotEat_FR_r = case_when(
          HHSNotEat_FR == 1 ~ 1,
          HHSNotEat_FR == 2 ~ 1,
          HHSNotEat_FR == 3 ~ 2,
          TRUE ~ 0
        )
      ) %>%
      mutate(
        HHS = HHhSNoFood_FR_r + HHhSBedHung_FR_r + HHhSNotEat_FR_r,
        HHS_CH = case_when(
          HHS == 0 ~ "Phase 1 (=0)",
          HHS == 1 ~ "Phase 2 (=1)",
          HHS %in% c(2,3) ~ "Phase 3 (2-3)",
          HHS == 4 ~ "Phase 4 (=4)",
          HHS >= 5 ~ "Phase 5 (5-6)"
        )
      )

    # ------------------------------------------------------------------
    # FES
    # ------------------------------------------------------------------
    # A) Utility function for 7-day => monthly
    convert_to_monthly <- function(df2, columns, recall_period) {
      if (recall_period == 7) {
        df2 <- df2 %>% mutate(across(all_of(columns), ~ . * (30/7)))
      }
      df2
    }

    # B) We'll pick the user-chosen recall from the sub-tab or default to 30
    recall_choice <- input$fesRecall
    if (is.null(recall_choice)) {
      recall_choice <- 30
    }

    # If user picks 7 => do 7-day logic, else 1-month
    if (recall_choice == 7) {
      df <- df %>%
        mutate(
          HHExp_Food_Purch_MN_1M   = rowSums(select(., starts_with("HHExpF") & ends_with("_Purch_MN_7D")), na.rm=TRUE),
          HHExp_Food_GiftAid_MN_1M = rowSums(select(., starts_with("HHExpF") & ends_with("_GiftAid_MN_7D")), na.rm=TRUE),
          HHExp_Food_Own_MN_1M     = rowSums(select(., starts_with("HHExpF") & ends_with("_Own_MN_7D")), na.rm=TRUE)
        )
      df <- convert_to_monthly(df,
                               c("HHExp_Food_Purch_MN_1M","HHExp_Food_GiftAid_MN_1M","HHExp_Food_Own_MN_1M"),
                               7
      )
    } else {
      # 1-month logic
      df <- df %>%
        mutate(
          HHExp_Food_Purch_MN_1M   = rowSums(select(., starts_with("HHExpF") & ends_with("_Purch_MN_1M")), na.rm=TRUE),
          HHExp_Food_GiftAid_MN_1M = rowSums(select(., starts_with("HHExpF") & ends_with("_GiftAid_MN_1M")), na.rm=TRUE),
          HHExp_Food_Own_MN_1M     = rowSums(select(., starts_with("HHExpF") & ends_with("_Own_MN_1M")), na.rm=TRUE)
        )
    }

    # 3) Overall monthly food expenditure
    df <- df %>%
      mutate(
        HHExp_Food_MN_1M = rowSums(select(., starts_with("HHExpF") & ends_with("_MN_1M")), na.rm=TRUE)
      )

    # 4) total non-food (1-month)
    df <- df %>%
      mutate(
        HHExpNFTotal_Purch_MN_30D   = rowSums(select(., starts_with("HHExpNF") & ends_with("_Purch_MN_1M")), na.rm=TRUE),
        HHExpNFTotal_GiftAid_MN_30D = rowSums(select(., starts_with("HHExpNF") & ends_with("_GiftAid_MN_1M")), na.rm=TRUE)
      )

    # 5) Convert 6-month recall => monthly
    df <- df %>%
      mutate(
        HHExpNFTotal_Purch_MN_6M   = rowSums(select(., starts_with("HHExpNF") & ends_with("_Purch_MN_6M")), na.rm=TRUE) / 6,
        HHExpNFTotal_GiftAid_MN_6M = rowSums(select(., starts_with("HHExpNF") & ends_with("_GiftAid_Mn_6M")), na.rm=TRUE) / 6
      )

    # 6) Sum up total non-food monthly
    df <- df %>%
      mutate(
        HHExpNFTotal_Purch_MN_1M   = HHExpNFTotal_Purch_MN_30D + HHExpNFTotal_Purch_MN_6M,
        HHExpNFTotal_GiftAid_MN_1M = HHExpNFTotal_GiftAid_MN_30D + HHExpNFTotal_GiftAid_MN_6M
      )

    # 7) total food & non-food
    df <- df %>%
      mutate(
        HHExpF_1M  = HHExp_Food_Purch_MN_1M + HHExp_Food_GiftAid_MN_1M + HHExp_Food_Own_MN_1M,
        HHExpNF_1M = HHExpNFTotal_Purch_MN_1M + HHExpNFTotal_GiftAid_MN_1M
      )

    # 8) FES (food expenditure share)
    df <- df %>%
      mutate(
        FES = HHExpF_1M / (HHExpF_1M + HHExpNF_1M)
      )

    # 9) FES 4-pt categories
    df <- df %>%
      mutate(
        Foodexp_4pt = case_when(
          FES < 0.50              ~ 1,
          FES >= 0.50 & FES <0.65 ~ 2,
          FES >= 0.65 & FES <0.75 ~ 3,
          FES >= 0.75            ~ 4,
          TRUE                   ~ NA_real_
        )
      )
    df$Foodexp_4pt <- factor(df$Foodexp_4pt,
                             levels = c(1,2,3,4),
                             labels=c("<50%", "50-65%", "65-75%", ">75%")
    )

    # Return final processed data
    df
  }) # <--- ends processed() reactive

  ###########################################################################
  # 2) DYNAMIC LCS: EXACT 4 stress, EXACT 3 crisis, EXACT 3 emergency
  ###########################################################################
  dynamicLCS <- reactive({
    df <- processed()

    req(input$stressVars, input$crisisVars, input$emergencyVars)

    # 1) Gather user picks
    chosen_stress <- input$stressVars
    chosen_crisis <- input$crisisVars
    chosen_emerg  <- input$emergencyVars

    # 2) Validate EXACT counts
    if (length(chosen_stress) != 4) {
      shiny::validate(
        shiny::need(FALSE,
             paste("Must pick EXACTLY 4 stress strategies. Currently:", length(chosen_stress))
        )
      )
    }
    if (length(chosen_crisis) != 3) {
      shiny::validate(
        shiny::need(FALSE,
             paste("Must pick EXACTLY 3 crisis strategies. Currently:", length(chosen_crisis))
        )
      )
    }
    if (length(chosen_emerg) != 3) {
      shiny::validate(
        shiny::need(FALSE,
             paste("Must pick EXACTLY 3 emergency strategies. Currently:", length(chosen_emerg))
        )
      )
    }

    # 3) Check all exist in df
    missingStress <- setdiff(chosen_stress, names(df))
    missingCrisis <- setdiff(chosen_crisis, names(df))
    missingEmerg  <- setdiff(chosen_emerg,  names(df))
    allMissing    <- c(missingStress, missingCrisis, missingEmerg)
    if (length(allMissing) > 0) {
      shiny::validate(
        shiny::need(FALSE,
             paste("The following selected LCS variables are not in your dataset:", paste(allMissing, collapse=", "))
        )
      )
    }

    # 4) If picks are valid, compute dynamic coping
    df <- df %>%
      mutate(
        stress_coping = if_else(
          rowSums(across(all_of(chosen_stress), ~ . %in% c(20,30))) > 0, 1, 0
        ),
        crisis_coping = if_else(
          rowSums(across(all_of(chosen_crisis), ~ . %in% c(20,30))) > 0, 1, 0
        ),
        emergency_coping = if_else(
          rowSums(across(all_of(chosen_emerg), ~ . %in% c(20,30))) > 0, 1, 0
        ),
        LhCSICat = case_when(
          emergency_coping == 1 ~ 4,
          crisis_coping    == 1 ~ 3,
          stress_coping    == 1 ~ 2,
          TRUE ~ 1
        )
      ) %>%
      mutate(
        LhCSICat = factor(
          LhCSICat,
          levels = c(1,2,3,4),
          labels = c("NoStrategies","StressStrategies","CrisisStrategies","EmergencyStrategies")
        )
      )

    df
  })

  ###########################################################################
  # 3) DYNAMIC CARI => merges dynamic LCS + user-chosen FCS threshold
  ###########################################################################
  dynamicCARI <- reactive({
    cat("Entered dynamicCARI()...\n")
    df_lcs <- dynamicLCS()  # This reactive should return df with LhCSICat

    req(input$cariFcsThreshold)  # Expecting "FCSCat21" or "FCSCat28"
    fcsVar <- input$cariFcsThreshold
    cat("User picked FCS threshold =>", fcsVar, "\n")

    # Recode FCS_4pt based on the chosen threshold
    df_lcs <- df_lcs %>%
      mutate(
        FCS_4pt = case_when(
          .data[[fcsVar]] == "Poor"       ~ 4,
          .data[[fcsVar]] == "Borderline" ~ 3,
          .data[[fcsVar]] == "Acceptable" ~ 1,
          TRUE ~ NA_real_
        )
      ) %>%
      # Mandatory adjustment: if rCSI >= 4 and FCS_4pt equals 1, then set FCS_4pt to 2
      mutate(
        FCS_4pt = case_when(
          rCSI >= 4 & FCS_4pt == 1 ~ 2,
          TRUE ~ FCS_4pt
        )
      )

    df_lcs <- df_lcs %>%
      mutate(
        FCS_4pt = factor(FCS_4pt, levels = c(1,2,3,4),
                         labels = c("Acceptable", "Acceptable and rCSI>4", "Borderline", "Poor")),
        FCS_4pt = as.numeric(FCS_4pt)
      )

    cat("Summary of FCS_4pt:\n")
    print(table(df_lcs$FCS_4pt, useNA="ifany"))

    # Convert dynamic LCS factor to a numeric variable (LCS_4pt)
    df_lcs <- df_lcs %>%
      mutate(
        LCS_4pt = case_when(
          LhCSICat == "NoStrategies"        ~ 1,
          LhCSICat == "StressStrategies"    ~ 2,
          LhCSICat == "CrisisStrategies"    ~ 3,
          LhCSICat == "EmergencyStrategies" ~ 4,
          TRUE ~ NA_real_
        )
      )

    # Convert Foodexp_4pt (already calculated in FES section) from factor to numeric.
    # Adjust the mapping as needed.
    df_lcs <- df_lcs %>%
      mutate(
        Foodexp_4pt_num = case_when(
          Foodexp_4pt == "<50%"   ~ 1,
          Foodexp_4pt == "50-65%" ~ 2,
          Foodexp_4pt == "65-75%" ~ 3,
          Foodexp_4pt == ">75%"   ~ 4,
          TRUE ~ NA_real_
        )
      )

    # Compute row means to derive the coping capacity and final CARI_FES:
    df_lcs <- df_lcs %>%
      mutate(
        Mean_coping_capacity_FES = rowMeans(cbind(as.numeric(LCS_4pt), as.numeric(Foodexp_4pt_num)), na.rm = TRUE),
        CARI_unrounded_FES = rowMeans(cbind(Mean_coping_capacity_FES, as.numeric(FCS_4pt)), na.rm = TRUE),
        CARI_FES = round_half_up(CARI_unrounded_FES)
      )

    # Label final CARI_FES
    df_lcs$CARI_FES <- factor(df_lcs$CARI_FES,
                              levels = 1:4,
                              labels = c("Food secure", "Marginally food secure", "Moderately food insecure", "Severely food insecure")
    )

    cat("LCS_4pt summary:\n"); print(summary(df_lcs$LCS_4pt))
    cat("Foodexp_4pt_num summary:\n"); print(summary(df_lcs$Foodexp_4pt_num))
    cat("FCS_4pt summary:\n"); print(summary(df_lcs$FCS_4pt))
    cat("Mean coping capacity:\n"); print(summary(df_lcs$Mean_coping_capacity_FES))
    cat("Unrounded CARI:\n"); print(summary(df_lcs$CARI_unrounded_FES))
    cat("Dynamic CARI done. Summary of CARI_FES:\n")
    print(table(df_lcs$CARI_FES, useNA="ifany"))



    df_lcs
  })


  ###########################################################################
  # 4) LCS Plot => dynamic
  ###########################################################################
  output$plotDynamicLCS <- renderPlotly({
    df_lcs <- req(dynamicLCS())

    dist_data <- df_lcs %>%
      dplyr::count(LhCSICat, name = "n") %>%
      dplyr::mutate(perc = round(100 * n / sum(n, na.rm = TRUE), 1))

    # map your custom colors in the same order as dist_data$LhCSICat
    slice_colors <- unname(LHCS_colors[as.character(dist_data$LhCSICat)])

    plotly::plot_ly(
      data   = dist_data,
      labels = ~LhCSICat,
      values = ~n,
      type   = "pie",
      marker = list(colors = slice_colors),
      text   = ~paste0(n, " obs"),
      hovertemplate = paste(
        "<b>%{label}</b><br>",
        "%{value} obs<br>",
        "%{percent}",
        "<extra></extra>"
      ),
      textinfo = "label+percent"
    ) %>%
      plotly::layout(
        title      = list(text = "Dynamic LhCSICat Distribution", x = 0.5),
        showlegend = FALSE
      )
  })


  ###########################################################################
  # 5) Provide main data (reqData) for older sub-tabs (Survey, FCS, etc.)
  ###########################################################################
  reqData <- reactive({
    req(processed())
    processed()
  })

  output$fileUploadedMessage <- renderPrint({
    if (is.null(input$file)) {
      "No file uploaded yet."
    } else {
      paste("File uploaded:", input$file$name)
    }
  })

  output$dataPreview <- DT::renderDataTable({
    # pull your full dataset
    df <- reqData()

    # specify the exact columns you want
    cols_to_show <- c(
      "ADMIN1Name",
      "ADMIN2Name",
      "ADMIN4Name",
      "EnuName",
      "EnuSupervisorName",
      "HHSizeCalc",
      "FcSStap",
      "FCSPulse",
      "FCSDiary",
      "FCSVeg",
      "FCSPr",
      "FCSFruit",
      "FCSFat",
      "FCSSugar"
    )

    # keep only those that actually exist
    df_sub <- df[, intersect(cols_to_show, names(df)), drop = FALSE]

    # preview the first 10 rows
    DT::datatable(
      head(df_sub, 10),
      options = list(
        scrollY = "300px",
        scrollX = TRUE,
        paging  = FALSE
      ),
      class = "stripe hover"
    )
  })

  # 1) Observations
  output$obsBox <- renderValueBox({
    df <- req(reqData())
    valueBox(
      value    = formatC(nrow(df), big.mark = ","),   # nicely format large numbers
      subtitle = "Observations",
      icon     = icon("table"),
      color    = "aqua"
    )
  })

  # 2) Variables
  output$varBox <- renderValueBox({
    df <- req(reqData())
    valueBox(
      value    = formatC(ncol(df), big.mark = ","),
      subtitle = "Variables",
      icon     = icon("columns"),
      color    = "light-blue"
    )
  })

  # 3) Duplicate rows (from ADMIN1Name up to before instanceID)
  output$dupBox <- renderValueBox({
    df <- req(reqData())

    # locate the block of columns
    startCol <- match("ADMIN1Name", names(df))
    endCol   <- match("instanceID",  names(df))
    num_dup  <- 0
    if (!is.na(startCol) && !is.na(endCol) && startCol < endCol) {
      subset_cols <- df[ , startCol:(endCol - 1), drop = FALSE]
      dup_df <- subset_cols %>%
        group_by(across(everything())) %>%
        tally(name = "count") %>%
        filter(count > 1)

      # total duplicates = total rows in those groups minus one per group
      num_dup <- sum(dup_df$count) - nrow(dup_df)
    }

    valueBox(
      value    = num_dup,
      subtitle = "Duplicate Rows",
      icon     = icon("copy"),
      color    = "red"
    )
  })

  #########################
  # Colors
  #########################
  fcg_colors <- c("Acceptable" = "#27AE60",
                  "Borderline" = "#F1C40F",
                  "Poor"       = "#C0392B")
  HDDS_colors  <- c("Phase 1 (>=5 Food Groups)" = "#c6ffc7",
                    "Phase 2 (=4 Food Groups)" = "#ffe718",
                    "Phase 3 (=3 Food Groups)" = "#e88400",
                    "Phase 4 (=2 Food Groups)" = "#e02d00",
                    "Phase 5 (<=1 Food Groups)" = "#5e0803")

  CH_colors  <- c("Phase1" = "#c6ffc7",
                  "Phase2" = "#ffe718",
                  "Phase3" = "#e88400",
                  "Phase4" = "#e02d00",
                  "Phase5" = "#5e0803")
  rCSI_colors <- c("Phase 1 (0-3)" = "#c6ffc7",
                   "Phase 2 (4-18)" = "#ffe718",
                   "Phase 3 (>=19)" = "#e88400")
  HHS_colors <- c("Phase 1 (=0)" = "#c6ffc7",
                  "Phase 2 (=1)" = "#ffe718",
                  "Phase 3 (2-3)" = "#e88400",
                  "Phase 4 (=4)" = "#e02d00",
                  "Phase 5 (5-6)" = "#5e0803")
  LHCS_colors <- c("NoStrategies" = "#F1ECE8",
                   "StressStrategies" = "#D5B868",
                   "CrisisStrategies" = "#F37847",
                   "EmergencyStrategies" = "#C00000")
  FES_Colors <- c("<50%"="#ECE1B1",
                  "50-65%"="#E6B068",
                  "65-75%"="#E67536",
                  ">75%"="#D70000")
  CARI_Colors <- c("Food secure" = "#FFD7D7",
                   "Marginally food secure" = "#FF6E6E",
                   "Moderately food insecure" = "#D70000",
                   "Severely food insecure" = "#820000")

  ###########################################################################
  # 2) SURVEY PROGRESS
  ###########################################################################
  # A) Submissions Over Time
  output$plotSubmission <- renderPlotly({
    df <- reqData()
    submission <- df %>% group_by(Survey_date) %>% count()
    p <- ggplot(submission, aes(x = Survey_date, y = n)) +
      geom_line(color = "steelblue") +
      theme_minimal() + labs(x = "Date", y = "Number of Submissions")
    ggplotly(p)
  })

  # B) Count by Admin1
  output$plotAdm1 <- renderPlotly({
    df <- reqData()
    countsadm1table <- df %>% group_by(ADMIN1Name) %>% count()
    p <- ggplot(countsadm1table, aes(x = reorder(ADMIN1Name, -n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_minimal() + labs(x = "ADMIN1Name", y = "Number of surveys")
    ggplotly(p)
  })

  # C) Count by Admin2 - Single Admin1Name filter
  observeEvent(reqData(), {
    df <- reqData()
    admin1List <- sort(unique(df$ADMIN1Name))
    updateSelectInput(session, "admin1Filter",
                      choices = admin1List,
                      selected = admin1List[1]
    )
  })

  output$plotAdm2Filter <- renderPlotly({
    df <- reqData()
    req(input$admin1Filter)

    # Filter by the selected Admin1Name
    df_filtered <- df %>% filter(ADMIN1Name == input$admin1Filter)

    counts_by_admin2 <- df_filtered %>%
      group_by(ADMIN2Name) %>%
      count() %>%
      arrange(desc(n))

    p <- ggplot(counts_by_admin2, aes(x = reorder(ADMIN2Name, -n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_minimal() +
      labs(x = "ADMIN2Name", y = "Number of surveys") +
      theme(axis.text.x = element_text(angle = 90))
    ggplotly(p)
  })

  # D) Surveys by Enumerator & Admin1Name (Histogram)
  # 1) when the data first arrives, fill Admin1 dropdown
  observeEvent(reqData(), {
    df <- reqData()
    admin1List <- sort(unique(df$ADMIN1Name))
    updateSelectInput(session, "admin1FilterEnum",
                      choices = admin1List,
                      selected = admin1List[1])
  })

  # 2) when Admin1 changes, fill Admin2 dropdown with only its children
  observeEvent(input$admin1FilterEnum, {
    df <- reqData()
    req(input$admin1FilterEnum)

    # grab the Admin2 list for that Admin1
    admin2List <- sort(unique(df$ADMIN2Name[df$ADMIN1Name == input$admin1FilterEnum]))

    # prepend "All"
    admin2List <- c("All", admin2List)

    # update, default to "All"
    updateSelectInput(
      session,
      "admin2FilterEnum",
      choices  = admin2List,
      selected = "All"
    )
  })

  # 3) now filter on both Admin1 & Admin2 in your histogram
  output$plotAdm1Enum <- renderPlotly({
    df <- req(reqData())
    req(input$admin1FilterEnum, input$admin2FilterEnum)

    # always filter by Admin1
    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnum)

    # only filter by Admin2 if not "All"
    if (input$admin2FilterEnum != "All") {
      df_filtered <- df_filtered %>%
        filter(ADMIN2Name == input$admin2FilterEnum)
    }

    counts <- df_filtered %>%
      count(EnuName, name = "n")

    p <- ggplot(counts, aes(x = reorder(EnuName, -n), y = n)) +
      geom_col(fill = "steelblue") +
      theme_minimal() +
      labs(x = "Enumerator", y = "Number of surveys") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

    ggplotly(p)
  })



  # E) Surveys by Enumerator & Admin2Name (Treemap) - no "All"
  observeEvent(reqData(), {
    df <- reqData()
    enumerators <- sort(unique(df$EnuName))
    # Remove "All"; user must pick one enumerator
    updateSelectInput(session, "filterEnumerator",
                      choices = enumerators,
                      selected = enumerators[1]
    )
  })

  # 1) Text label: total number of surveys for the selected enumerator
  output$treemapCountEnumerator <- renderText({
    df <- reqData()
    req(input$filterEnumerator)
    enumerator <- input$filterEnumerator

    # Filter to the enumerator
    df_enum <- df %>% filter(EnuName == enumerator)
    totalSurveys <- nrow(df_enum)

    paste("Enumerator", enumerator, "conducted", totalSurveys, "surveys in total.")
  })

  # 2) Plotly treemap: number of surveys by Admin2Name
  output$plotAdm2EnumTree <- renderPlotly({
    df <- reqData()
    req(input$filterEnumerator)
    enumerator <- input$filterEnumerator

    # Subset data to the chosen enumerator
    df_enum <- df %>% filter(EnuName == enumerator)

    # Summarize how many surveys in each Admin2Name
    summary_df <- df_enum %>%
      group_by(ADMIN2Name) %>%
      summarize(count = n(), .groups="drop")

    # Create a native Plotly treemap
    plot_ly(
      data = summary_df,
      type = "treemap",
      labels = ~ADMIN2Name,   # what appears on each rectangle
      values = ~count,        # size of rectangle
      parents = NA,           # no hierarchy; top-level
      textinfo = "label+value" # show name + numeric value
    )
  })

  #########################
  # 3) FCS
  #########################
  ################################################################################
  # SERVER LOGIC FOR FCS CHARTS WITH NEW FILTERS
  ################################################################################

  # A) FCS by Admin1
  #    - Single threshold filter: "FCSCat21" or "FCSCat28"
  output$plotFCSadm1 <- renderPlotly({
    df <- reqData()
    req(input$fcsThresholdAdm1)  # e.g. "FCSCat21" or "FCSCat28"

    # We'll pick the column dynamically using .data[[fcsVar]]
    fcsVar <- input$fcsThresholdAdm1  # string: "FCSCat21" or "FCSCat28"

    # Summarize data by ADMIN1Name and the chosen FCS category
    fcs_data <- df %>%
      group_by(ADMIN1Name, .data[[fcsVar]]) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN1Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    # Build a 100% stacked bar chart (or stacked by count if desired)
    p <- ggplot(fcs_data, aes(x = ADMIN1Name, y = perc, fill = .data[[fcsVar]],
                              text  = paste0(
                                "ADMIN1Name: ", ADMIN1Name, "<br>",
                                fcsVar, ": ", .data[[fcsVar]], "<br>",
                                "n: ", n, "<br>",
                                "perc: ", sprintf("%.2f%%", perc * 100)
                              ))) +
      geom_col(position = "stack") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      scale_fill_manual(values = fcg_colors) +    # same color scale you used before
      labs(x = "Admin1", y = "Percentage", fill = fcsVar) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))


    ggplotly(p, tooltip = "text")
  })


  ################################################################################
  # SERVER LOGIC FOR FCS FILTERS
  ################################################################################

  # 2) Observe dataset to populate Admin1 filters
  observeEvent(reqData(), {
    df <- reqData()

    # Gather unique Admin1 from your dataset
    admin1Vals <- sort(unique(df$ADMIN1Name))

    # FCS by Admin2 sub-tab => forced Admin1
    updateSelectInput(session, "admin1FilterFCS2",
                      choices = admin1Vals,
                      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  # 3) FCS by Admin2 (Chart logic)
  output$plotFCSadm2 <- renderPlotly({
    df <- reqData()
    req(input$admin1FilterFCS2)  # forced single Admin1
    req(input$fcsThresholdAdm2)  # e.g. "FCSCat21" or "FCSCat28"

    # Filter data by the chosen Admin1
    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterFCS2)

    # Decide which column to use
    fcsVar <- input$fcsThresholdAdm2

    # Summarize data
    fcs_data <- df_filtered %>%
      group_by(ADMIN2Name, .data[[fcsVar]]) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN2Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(fcs_data, aes(x = ADMIN2Name, y = perc, fill = .data[[fcsVar]],
                              text  = paste0(
                                "ADMIN2Name: ", ADMIN2Name, "<br>",
                                fcsVar, ": ", .data[[fcsVar]], "<br>",
                                "n: ", n, "<br>",
                                "perc: ", sprintf("%.2f%%", perc * 100)
                              ))) +
      geom_col(position = "stack") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      scale_fill_manual(values = fcg_colors) +
      labs(x = "Admin2", y = "Percentage", fill = fcsVar) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

  # (A) Populate Admin1 list when data is loaded
  observeEvent(reqData(), {
    df <- reqData()

    # Gather sorted unique Admin1
    admin1Vals <- sort(unique(df$ADMIN1Name))  # now strings, e.g. "Region1"

    # Update the Admin1 dropdown
    updateSelectInput(session, "admin1FilterEnumFCS",
                      choices = admin1Vals,
                      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  # (B) Populate Admin2 dropdown whenever user picks an Admin1
  observeEvent(input$admin1FilterEnumFCS, {
    df <- reqData()
    req(input$admin1FilterEnumFCS)

    # Subset to chosen Admin1
    df_sub <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumFCS)

    # Gather unique textual Admin2
    admin2Vals <- sort(unique(df_sub$ADMIN2Name))

    # Provide "All" plus the actual Admin2
    updateSelectInput(session, "admin2FilterEnumFCS",
                      choices = c("All", admin2Vals),
                      selected = "All"
    )
  })

  # (C) Produce the chart
  output$plotFCSadm1Enum <- renderPlotly({
    df <- reqData()
    req(input$admin1FilterEnumFCS)   # forced Admin1
    req(input$admin2FilterEnumFCS)   # "All" or text admin2
    req(input$fcsThresholdAdm1Enum)  # "FCSCat21" or "FCSCat28"

    # 1) Filter by chosen Admin1
    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumFCS)

    # 2) Filter by Admin2 if user picked something other than "All"
    if (input$admin2FilterEnumFCS != "All") {
      df_filtered <- df_filtered %>%
        filter(ADMIN2Name == input$admin2FilterEnumFCS)
    }

    # 3) Use the selected threshold column
    fcsVar <- input$fcsThresholdAdm1Enum  # "FCSCat21" or "FCSCat28"

    # Summarize data
    fcs_data <- df_filtered %>%
      group_by(EnuName, .data[[fcsVar]]) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(EnuName) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    # Example stacked bar chart
    p <- ggplot(fcs_data, aes(x = EnuName, y = perc, fill = .data[[fcsVar]],
                              text  = paste0(
                                "EnuName: ", EnuName, "<br>",
                                fcsVar, ": ", .data[[fcsVar]], "<br>",
                                "n: ", n, "<br>",
                                "perc: ", sprintf("%.2f%%", perc * 100)
                              ))) +
      geom_col(position = "stack") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      scale_fill_manual(values = fcg_colors) +   # your color scale
      labs(x = "Enumerator", y = "Percentage", fill = fcsVar) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

  output$plotFCSEnumNameBox <- renderPlotly({
    df <- reqData()
    FCSEnumNametable <- df %>%
      group_by(EnuName) %>%
      summarise(FCS_median = median(FCS, na.rm=TRUE)) %>%
      mutate(FCS_outlier = is_outlier(FCS_median))

    p <- ggplot(FCSEnumNametable, aes(x = "", y =FCS_median)) +
      geom_boxplot() +
      geom_point(aes(color = FCS_outlier))
    ggplotly(p)
  })

  output$tableFCSOutlier <- renderTable({
    df <- reqData()
    FCSEnumNametable <- df %>%
      group_by(EnuName) %>%
      summarise(FCS_median = median(FCS, na.rm=TRUE)) %>%
      mutate(FCS_outlier = is_outlier(FCS_median)) %>%
      filter(FCS_outlier == TRUE) %>%
      arrange(desc(FCS_median))

    FCSEnumNametable
  })

  output$tableCereal <- renderDataTable({
    df <- reqData()
    cereal <- df %>%
      filter(FCSStap <= 4) %>%
      select(ADMIN1Name, ADMIN2Name, ADMIN4Name, EnuSupervisorName,
             EnuName, FCSStap, FCSVeg, Survey_date)
    datatable(cereal, rownames = FALSE, filter = "top",options = list(
      scrollX = TRUE,    # horizontal scroll
      pageLength = 10    # optional: show 10 rows per page
    )
    )
  })

  output$downloadCereal <- downloadHandler(
    filename = function() { paste0("Low_Cereal_Consumption_", Sys.Date(), ".xlsx") },
    content = function(file) {
      df <- reqData() %>%
        filter(FCSStap <= 4) %>%
        select(ADMIN1Name,ADMIN2Name, ADMIN4Name, EnuSupervisorName,
               EnuName, FCSStap, FCSVeg, Survey_date)
      writexl::write_xlsx(df, path = file)
    }
  )

  output$tableCerealVegDairy <- renderDataTable({
    df <- reqData()
    tabcerealelegumelait <- df %>%
      filter(FCSStap <= 4) %>%
      select(ADMIN1Name,ADMIN2Name, ADMIN4Name, EnuSupervisorName,
             EnuName, FCSStap, FCSVeg, FCSDairy, Survey_date)
    datatable(tabcerealelegumelait, rownames = FALSE, filter = "top",options = list(
      scrollX = TRUE,    # horizontal scroll
      pageLength = 10    # optional: show 10 rows per page
    )
    )
  })

  # 2) Cereal, Vegetable, and Dairy
  output$downloadCerealVegDairy <- downloadHandler(
    filename = function() {
      paste0("Cereal_Veg_Dairy_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- reqData() %>%
        filter(FCSStap <= 4) %>%
        select(ADMIN1Name,ADMIN2Name, ADMIN4Name, EnuSupervisorName,
               EnuName, FCSStap, FCSVeg, FCSDairy, Survey_date)
      writexl::write_xlsx(df, path = file)
    }
  )

  output$tableVeg <- renderDataTable({
    df <- reqData()
    legumefeuille <- df %>%
      filter(FCSVeg <= 3) %>%
      select(ADMIN1Name,ADMIN2Name, ADMIN4Name, EnuSupervisorName,
             EnuName, FCSVeg, Survey_date)
    datatable(legumefeuille, rownames = FALSE, filter = "top")
  })

  # 3) Low Vegetable Consumption
  output$downloadVeg <- downloadHandler(
    filename = function() {
      paste0("Low_Vegetable_Consumption_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- reqData() %>%
        filter(FCSVeg <= 3) %>%
        select(ADMIN1Name,ADMIN2Name, ADMIN4Name, EnuSupervisorName,
               EnuName, FCSVeg, Survey_date)
      writexl::write_xlsx(df, path = file)
    }
  )

  # 4) Meat, Oil, and Sugar Consumption
  # Render the datatable for Meat, Oil, and Sugar Consumption
  output$tableMeatOilSugar <- renderDataTable({
    df <- reqData()

    # Select columns related to meat, oil, and sugar.
    # Here, we assume that:
    # - Meat consumption is captured in 'FCSPr'
    # - Oil consumption is in 'FCSFat'
    # - Sugar consumption is in 'FCSSugar'
    meat_oil_sugar <- df %>%
      select(ADMIN1Name,ADMIN2Name, ADMIN4Name, EnuSupervisorName, EnuName, FCSPr, FCSFat, FCSSugar, Survey_date)

    datatable(meat_oil_sugar, rownames = FALSE, filter = "top",options = list(
      scrollX = TRUE,    # horizontal scroll
      pageLength = 10    # optional: show 10 rows per page
    )
    )
  })

  # Download handler to export the Meat, Oil, and Sugar Consumption table as an Excel file
  output$downloadMeatOilSugar <- downloadHandler(
    filename = function() {
      paste0("Meat_Oil_Sugar_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- reqData() %>%
        select(ADMIN1Name,ADMIN2Name, ADMIN4Name, EnuSupervisorName, EnuName, FCSPr, FCSFat, FCSSugar, Survey_date)
      writexl::write_xlsx(df, path = file)
    }
  )


  #########################
  # 4) HDDS
  #########################
  ###########################
  # 1) HDDS by Admin1
  ###########################
  output$plotHDDSadm1 <- renderPlotly({
    df <- reqData()

    # Group by ADMIN1Name, and the HDDS_CH category
    hdds_data <- df %>%
      group_by(ADMIN1Name, HDDS_CH) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN1Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    # Example 100% stacked bar across all Admin1
    p <- ggplot(hdds_data, aes(x = ADMIN1Name, y = perc, fill = HDDS_CH,
                               text  = paste0(
                                 "ADMIN1Name: ", ADMIN1Name, "<br>",
                                 "HDDS_CH: ", HDDS_CH, "<br>",
                                 "n: ", n, "<br>",
                                 "perc: ", sprintf("%.2f%%", perc * 100)
                               ))) +
      geom_bar(position = "fill", stat = "identity") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      # Replace with your color palette for HDDS
      scale_fill_manual(values = HDDS_colors) +
      labs(x = "Admin1", y = "Percentage", fill = "HDDS Phase") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

  ###########################
  # 2) HDDS by Admin2
  #    - Force user to pick exactly one Admin1
  ###########################
  # A) Populate the Admin1 filter for "HDDS by Admin2" when data is loaded
  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(session, "admin1FilterHDDS2",
                      choices = admin1Vals,
                      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  # B) Render the chart
  output$plotHDDSadm2 <- renderPlotly({
    df <- reqData()
    req(input$admin1FilterHDDS2)  # forced single Admin1

    # Filter to chosen Admin1
    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterHDDS2)

    # Summarize by Admin2 & HDDS_CH
    hdds_data <- df_filtered %>%
      group_by(ADMIN2Name, HDDS_CH) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN2Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(hdds_data, aes(x = ADMIN2Name, y = perc, fill = HDDS_CH,
                               text  = paste0(
                                 "ADMIN2Name: ", ADMIN2Name, "<br>",
                                 "HDDS_CH: ", HDDS_CH, "<br>",
                                 "n: ", n, "<br>",
                                 "perc: ", sprintf("%.2f%%", perc * 100)
                               ))) +
      geom_col(position = "stack") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90,vjust = 0.5)) +
      scale_fill_manual(values = HDDS_colors) +
      labs(x = "Admin2", y = "Percentage", fill = "HDDS Phase") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p,tooltip = "text")
  })

  ###########################
  # 3) HDDS by Admin1 & Enumerator
  #    - Force an Admin1, optional "All" for Admin2
  ###########################
  # A) Populate Admin1 filter
  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(session, "admin1FilterEnumHDDS",
                      choices = admin1Vals,
                      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  # B) Populate Admin2 filter whenever user picks an Admin1
  observeEvent(input$admin1FilterEnumHDDS, {
    df <- reqData()
    req(input$admin1FilterEnumHDDS)

    # Subset to chosen Admin1
    df_sub <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumHDDS)

    admin2Vals <- sort(unique(df_sub$ADMIN2Name))

    updateSelectInput(session, "admin2FilterEnumHDDS",
                      choices = c("All", admin2Vals),
                      selected = "All"
    )
  })

  # C) Render chart
  output$plotHDDSadm1Enum <- renderPlotly({
    df <- reqData()
    req(input$admin1FilterEnumHDDS)
    req(input$admin2FilterEnumHDDS)

    # 1) Filter by chosen Admin1
    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumHDDS)

    # 2) If user picks a specific Admin2, filter further
    if (input$admin2FilterEnumHDDS != "All") {
      df_filtered <- df_filtered %>%
        filter(ADMIN2Name == input$admin2FilterEnumHDDS)
    }

    # Summarize by enumerator
    hdds_data <- df_filtered %>%
      group_by(EnuName, HDDS_CH) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(EnuName) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(hdds_data, aes(x = EnuName, y = perc, fill = HDDS_CH,
                               text  = paste0(
                                 "EnuName: ", EnuName, "<br>",
                                 "HDDS_CH: ", HDDS_CH, "<br>",
                                 "n: ", n, "<br>",
                                 "perc: ", sprintf("%.2f%%", perc * 100)
                               ))) +
      geom_col(position = "stack") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      scale_fill_manual(values = HDDS_colors) +
      labs(x = "Enumerator", y = "Percentage", fill = "HDDS Phase") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p,tooltip = "text")
  })

  #########################
  # 5) rCSI
  #########################
  ###########################
  # 1) rCSI by Admin1
  ###########################
  output$plotrCSIadm1 <- renderPlotly({
    df <- reqData()

    rcsi_data <- df %>%
      group_by(ADMIN1Name, rCSI_CH) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN1Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(rcsi_data, aes(x = ADMIN1Name, y = perc, fill = rCSI_CH,
                               text  = paste0(
                                 "ADMIN1Name: ", ADMIN1Name, "<br>",
                                 "rCSI_CH: ", rCSI_CH, "<br>",
                                 "n: ", n, "<br>",
                                 "perc: ", sprintf("%.2f%%", perc * 100)
                               ))) +
      geom_col(position = "stack") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90,vjust = 0.5)) +
      scale_fill_manual(values = rCSI_colors) +
      labs(x = "Admin1", y = "Percentage", fill = "rCSI Phase") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p,tooltip = "text")
  })

  ###########################
  # 2) rCSI by Admin2
  #    - Force user to pick exactly one Admin1
  ###########################
  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(session, "admin1FilterrCSI2",
                      choices = admin1Vals,
                      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  output$plotrCSIadm2 <- renderPlotly({
    df <- reqData()
    req(input$admin1FilterrCSI2)

    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterrCSI2)

    rcsi_data <- df_filtered %>%
      group_by(ADMIN2Name, rCSI_CH) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN2Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(rcsi_data, aes(x = ADMIN2Name, y = perc, fill = rCSI_CH,
                               text  = paste0(
                                 "ADMIN2Name: ", ADMIN2Name, "<br>",
                                 "rCSI_CH: ", rCSI_CH, "<br>",
                                 "n: ", n, "<br>",
                                 "perc: ", sprintf("%.2f%%", perc * 100)
                               ))) +
      geom_col(position = "stack") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90,vjust = 0.5)) +
      scale_fill_manual(values = rCSI_colors) +
      labs(x = "Admin2", y = "Percentage", fill = "rCSI Phase") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p,tooltip = "text")
  })

  ###########################
  # 3) rCSI by Admin1 & Enumerator
  ###########################
  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(session, "admin1FilterEnumrCSI",
                      choices = admin1Vals,
                      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  observeEvent(input$admin1FilterEnumrCSI, {
    df <- reqData()
    req(input$admin1FilterEnumrCSI)

    df_sub <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumrCSI)

    admin2Vals <- sort(unique(df_sub$ADMIN2Name))

    updateSelectInput(session, "admin2FilterEnumrCSI",
                      choices = c("All", admin2Vals),
                      selected = "All"
    )
  })

  output$plotrCSIadm1Enum <- renderPlotly({
    df <- reqData()
    req(input$admin1FilterEnumrCSI)
    req(input$admin2FilterEnumrCSI)

    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumrCSI)

    if (input$admin2FilterEnumrCSI != "All") {
      df_filtered <- df_filtered %>%
        filter(ADMIN2Name == input$admin2FilterEnumrCSI)
    }

    rcsi_data <- df_filtered %>%
      group_by(EnuName, rCSI_CH) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(EnuName) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(rcsi_data, aes(x = EnuName, y = perc, fill = rCSI_CH,
                               text  = paste0(
                                 "EnuName: ", EnuName, "<br>",
                                 "rCSI_CH: ", rCSI_CH, "<br>",
                                 "n: ", n, "<br>",
                                 "perc: ", sprintf("%.2f%%", perc * 100)
                               ))) +
      geom_col(position = "stack") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90,vjust = 0.5)) +
      scale_fill_manual(values = rCSI_colors) +
      labs(x = "Enumerator", y = "Percentage", fill = "rCSI Phase") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p,tooltip = "text")
  })

  ################################
  # 4) rCSI Outlier
  ################################
  # 1) update the Admin1 dropdown for the Admin2 subtab
  observeEvent(reqData(), {
    adm1s <- sort(unique(reqData()$ADMIN1Name))
    updateSelectInput(session,
                      "rcsiBoxAdmin2Admin1",
                      choices = adm1s,
                      selected = adm1s[1] %||% NULL)
  })

  # 2) Boxplot: rCSI by Admin1
  output$plotRCSIBoxAdmin1 <- renderPlotly({
    df <- req(reqData())

    p <- ggplot(df, aes(x = ADMIN1Name, y = rCSI)) +
      geom_boxplot(fill = "steelblue") +
      theme_minimal() +
      labs(x = "Admin1", y = "rCSI", title = "rCSI Distribution by Admin1")

    ggplotly(p) %>%
      layout(showlegend = FALSE)
  })

  # 3) Outlier table: rCSI by Admin1
  output$tableRCSIOutliersAdmin1 <- DT::renderDT({
    df <- req(reqData())

    df_out <- df %>%
      group_by(ADMIN1Name) %>%
      mutate(
        Q1 = quantile(rCSI, .25, na.rm = TRUE),
        Q3 = quantile(rCSI, .75, na.rm = TRUE),
        IQR = Q3 - Q1,
        lower = Q1 - 1.5 * IQR,
        upper = Q3 + 1.5 * IQR
      ) %>%
      ungroup() %>%
      filter(rCSI < lower | rCSI > upper) %>%
      select(ADMIN1Name, ADMIN2Name, ADMIN4Name,
             EnuSupervisorName, EnuName, HHSizeCalc, rCSI,
             rCSILessQlty, rCSIBorrow, rCSIMealSize,
             rCSIMealAdult, rCSIMealNb)

    DT::datatable(
      df_out,
      rownames   = FALSE,
      extensions = 'Buttons',
      filter     = 'top',
      options    = list(
        dom     = 'Bfrtip',
        buttons = list(
          list(extend='csv',   text='Export CSV',   exportOptions = list(modifier=list(page='all'))),
          list(extend='excel', text='Export Excel', exportOptions = list(modifier=list(page='all')))
        ),
        scrollX    = TRUE,
        pageLength = 10
      )
    ) %>%
      formatRound("rCSI", 2, mark=",")
  }, server = FALSE)

  # 4) Download handler for Admin1 outliers
  output$downloadRCSIOutliersAdmin1 <- downloadHandler(
    filename = function() {
      paste0("rCSI_Outliers_Admin1_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- req(reqData()) %>%
        group_by(ADMIN1Name) %>%
        mutate(
          Q1 = quantile(rCSI, .25, na.rm = TRUE),
          Q3 = quantile(rCSI, .75, na.rm = TRUE),
          IQR = Q3 - Q1,
          lower = Q1 - 1.5 * IQR,
          upper = Q3 + 1.5 * IQR
        ) %>%
        ungroup() %>%
        filter(rCSI < lower | rCSI > upper) %>%
        select(ADMIN1Name, ADMIN2Name, ADMIN4Name,
               EnuSupervisorName, EnuName, HHSizeCalc, rCSI,
               rCSILessQlty, rCSIBorrow, rCSIMealSize,
               rCSIMealAdult, rCSIMealNb)

      writexl::write_xlsx(df, path = file)
    }
  )

  # 5) Boxplot: rCSI by Admin2 (with Admin1 filter)
  output$plotRCSIBoxAdmin2 <- renderPlotly({
    df <- req(reqData())
    req(input$rcsiBoxAdmin2Admin1)

    filtered <- df %>% filter(ADMIN1Name == input$rcsiBoxAdmin2Admin1)

    p <- ggplot(filtered, aes(x = ADMIN2Name, y = rCSI)) +
      geom_boxplot(fill = "tomato") +
      theme_minimal() +
      labs(x = "Admin2", y = "rCSI",
           title = paste("rCSI by Admin2 for Admin1 =", input$rcsiBoxAdmin2Admin1))

    ggplotly(p) %>%
      layout(showlegend = FALSE)
  })

  # 6) Outlier table: rCSI by Admin2
  output$tableRCSIOutliersAdmin2 <- DT::renderDT({
    df <- req(reqData())
    req(input$rcsiBoxAdmin2Admin1)

    df_out <- df %>%
      filter(ADMIN1Name == input$rcsiBoxAdmin2Admin1) %>%
      group_by(ADMIN2Name) %>%
      mutate(
        Q1 = quantile(rCSI, .25, na.rm = TRUE),
        Q3 = quantile(rCSI, .75, na.rm = TRUE),
        IQR = Q3 - Q1,
        lower = Q1 - 1.5 * IQR,
        upper = Q3 + 1.5 * IQR
      ) %>%
      ungroup() %>%
      filter(rCSI < lower | rCSI > upper) %>%
      select(ADMIN1Name, ADMIN2Name, ADMIN4Name,
             EnuSupervisorName, EnuName, HHSizeCalc, rCSI,rCSILessQlty,
             rCSIBorrow, rCSIMealSize,
             rCSIMealAdult, rCSIMealNb)

    DT::datatable(
      df_out,
      rownames   = FALSE,
      extensions = 'Buttons',
      filter     = 'top',
      options    = list(
        dom     = 'Bfrtip',
        buttons = list(
          list(extend='csv',   text='Export CSV',   exportOptions = list(modifier=list(page='all'))),
          list(extend='excel', text='Export Excel', exportOptions = list(modifier=list(page='all')))
        ),
        scrollX    = TRUE,
        pageLength = 10
      )
    ) %>%
      formatRound("rCSI", 2, mark=",")
  }, server = FALSE)

  # 7) Download handler for Admin2 outliers
  output$downloadRCSIOutliersAdmin2 <- downloadHandler(
    filename = function() {
      paste0("rCSI_Outliers_Admin2_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- req(reqData()) %>%
        filter(ADMIN1Name == input$rcsiBoxAdmin2Admin1) %>%
        group_by(ADMIN2Name) %>%
        mutate(
          Q1 = quantile(rCSI, .25, na.rm = TRUE),
          Q3 = quantile(rCSI, .75, na.rm = TRUE),
          IQR = Q3 - Q1,
          lower = Q1 - 1.5 * IQR,
          upper = Q3 + 1.5 * IQR
        ) %>%
        ungroup() %>%
        filter(rCSI < lower | rCSI > upper) %>%
        select(ADMIN1Name, ADMIN2Name, ADMIN4Name,
               EnuSupervisorName, EnuName, HHSizeCalc, rCSI, rCSILessQlty,
               rCSIBorrow, rCSIMealSize,
               rCSIMealAdult, rCSIMealNb)

      writexl::write_xlsx(df, path = file)
    }
  )



  ################################
  # 4) rCSI Triangulation with FCS
  ################################
  ### Update Admin1 for Triangulation (if not already done)
  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(session, "triangAdmin1",
                      choices = admin1Vals,
                      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL)
  })

  ### When Admin1 changes, update the Admin2 dropdown so that it lists only those in the selected Admin1.
  observeEvent(input$triangAdmin1, {
    req(reqData())
    df <- reqData()
    selected_admin1 <- input$triangAdmin1
    admin2_choices <- sort(unique(df %>% filter(ADMIN1Name == selected_admin1) %>% pull(ADMIN2Name)))
    updateSelectInput(session, "triangAdmin2",
                      choices = c("All", admin2_choices),
                      selected = "All")
  })

  ### (Optional) If you want to update the FCSCat selector from the dataset, you can leave it fixed to two choices as above.

  ### Render the Triangulation Plot (by Enumerator)
  output$plotTriangulation <- renderPlotly({
    # Require that all three inputs are available
    req(input$triangAdmin1, input$triangAdmin2, input$triangFCSCat)

    # Start with the main dataset filtered by the selected Admin1.
    df <- reqData() %>%
      filter(ADMIN1Name == input$triangAdmin1)

    # If the Admin2 filter is not set to "All", further filter by Admin2.
    if (input$triangAdmin2 != "All") {
      df <- df %>% filter(ADMIN2Name == input$triangAdmin2)
    }

    # Apply triangulation criteria:
    #   - Keep rows where rCSI is very high (>42) or very low (<3)
    #   - And where the selected FCS category (FCSCat21 or FCSCat28) is either "Poor" or "Borderline"
    df_tri <- df %>%
      filter((rCSI > 42 | rCSI < 3) &
               (get(input$triangFCSCat) %in% c("Poor", "Borderline"))) %>%
      mutate(Triang = if_else(rCSI > 42, "Very High (rCSI > 42)", "Very Low (rCSI < 3)"))

    # Summarize the data by enumerator (EnuName) so we can create a stacked bar chart.
    df_plot <- df_tri %>%
      group_by(EnuName, Triang) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(EnuName) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    # Create the 100% stacked bar chart.
    p <- ggplot(df_plot, aes(x = EnuName, y = perc, fill = Triang,
                             text  = paste0(
                               "EnuName: ", EnuName, "<br>",
                               "Trian: ", Triang, "<br>",
                               "n: ", n, "<br>",
                               "perc: ", sprintf("%.2f%%", perc * 100)
                             ))) +
      geom_col(stat = "stack") +
      labs(
        title = "Triangulation of rCSI and FCS",
        x = "Enumerator",
        y = "Proportion",
        fill = "Triangulation"
      ) +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p,tooltip = "text")
  })


  ### Render the Triangulation Details Table with the requested variables and filtering ("top") inputs.
  output$tableTriangulation <- renderDataTable({
    # Require both the Admin1 and the FCSCat selector are set.
    req(input$triangAdmin1, input$triangFCSCat)
    df <- reqData()

    # Filter by selected Admin1. Also filter by Admin2 if one is selected (i.e., not "All").
    df_tri <- df %>% filter(ADMIN1Name == input$triangAdmin1)
    if (!is.null(input$triangAdmin2) && input$triangAdmin2 != "All") {
      df_tri <- df_tri %>% filter(ADMIN2Name == input$triangAdmin2)
    }

    # Apply the triangulation criteria:
    df_tri <- df_tri %>%
      filter((rCSI > 42 | rCSI < 3) &
               (get(input$triangFCSCat) %in% c("Poor", "Borderline"))) %>%
      mutate(Triang = if_else(rCSI > 42, "Very High (rCSI > 42)", "Very Low (rCSI < 3)"))

    # Decide which FCSCat column to include (only the one selected)
    selectedFCSCol <- if (input$triangFCSCat == "FCSCat21") "FCSCat21" else "FCSCat28"

    # Select the detailed columns.
    df_details <- df_tri %>%
      select(ADMIN1Name, ADMIN2Name, ADMIN3Name, ADMIN4Name, EnuSupervisorName, EnuName,
             HHSizeCalc, FCSStap, FCSPulse, FCSDairy, FCSPr, FCSVeg, FCSFruit, FCSFat, FCSSugar, FCS,
             all_of(selectedFCSCol),
             rCSI, Triang, rCSILessQlty, rCSIBorrow, rCSIMealSize, rCSIMealAdult, rCSIMealNb)

    datatable(df_details, rownames = FALSE, filter = "top", options = list(scrollX = TRUE))
  })

  ### Download handler for Triangulation table
  output$downloadTriangulation <- downloadHandler(
    filename = function() {
      paste0("triangulation_details_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # replicate the exact filtering & selection from renderDataTable
      req(input$triangAdmin1, input$triangFCSCat)
      df <- reqData()

      # 1) Filter by Admin1 (and Admin2 if not "All")
      df_tri <- df %>%
        filter(ADMIN1Name == input$triangAdmin1)
      if (!is.null(input$triangAdmin2) && input$triangAdmin2 != "All") {
        df_tri <- df_tri %>% filter(ADMIN2Name == input$triangAdmin2)
      }

      # 2) Apply triangulation criteria
      df_tri <- df_tri %>%
        filter((rCSI > 42 | rCSI < 3) &
                 (get(input$triangFCSCat) %in% c("Poor", "Borderline"))) %>%
        mutate(
          Triang = if_else(
            rCSI > 42,
            "Very High (rCSI > 42)",
            "Very Low (rCSI < 3)"
          )
        )

      # 3) Pick the correct FCS column
      selectedFCSCol <- if (input$triangFCSCat == "FCSCat21") "FCSCat21" else "FCSCat28"

      # 4) Select exactly the same columns you show in the DT
      df_details <- df_tri %>%
        select(
          ADMIN1Name, ADMIN2Name, ADMIN3Name, ADMIN4Name,
          EnuSupervisorName, EnuName, HHSizeCalc,
          FCSStap, FCSPulse, FCSDairy, FCSPr, FCSVeg, FCSFruit,
          FCSFat, FCSSugar, FCS,
          all_of(selectedFCSCol),
          rCSI, Triang,
          rCSILessQlty, rCSIBorrow,
          rCSIMealSize, rCSIMealAdult, rCSIMealNb
        )

      # 5) Write to Excel
      writexl::write_xlsx(df_details, path = file)
    }
  )

  #########################
  # 6) HHS
  #########################
  # HHS by Admin1

  output$plotHHSadm1 <- renderPlotly({
    df <- reqData()

    hhs_data <- df %>%
      group_by(ADMIN1Name, HHS_CH) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN1Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(hhs_data, aes(x = ADMIN1Name, y = perc, fill = HHS_CH,
                              text  = paste0(
                                "ADMIN1Name: ", ADMIN1Name, "<br>",
                                "HHS_CH: ", HHS_CH, "<br>",
                                "n: ", n, "<br>",
                                "perc: ", sprintf("%.2f%%", perc * 100)
                              ))) +
      geom_col(position = "stack") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90,vjust = 0.5)) +
      scale_fill_manual(values = HHS_colors) +
      labs(x = "Admin1", y = "Percentage", fill = "HHS Phase") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p,tooltip = "text")
  })

  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(session, "admin1FilterHHS2",
                      choices = admin1Vals,
                      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  output$plotHHSadm2 <- renderPlotly({
    df <- reqData()
    req(input$admin1FilterHHS2)

    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterHHS2)

    hhs_data <- df_filtered %>%
      group_by(ADMIN2Name, HHS_CH) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN2Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(hhs_data, aes(x = ADMIN2Name, y = perc, fill = HHS_CH,
                              text  = paste0(
                                "ADMIN2Name: ", ADMIN2Name, "<br>",
                                "HHS_CH: ", HHS_CH, "<br>",
                                "n: ", n, "<br>",
                                "perc: ", sprintf("%.2f%%", perc * 100)
                              ))) +
      geom_col(position = "stack") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90,vjust = 0.5)) +
      scale_fill_manual(values = HHS_colors) +
      labs(x = "Admin2", y = "Percentage", fill = "HHS Phase") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p,tooltip = "text")
  })

  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(session, "admin1FilterEnumHHS",
                      choices = admin1Vals,
                      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  observeEvent(input$admin1FilterEnumHHS, {
    df <- reqData()
    req(input$admin1FilterEnumHHS)

    df_sub <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumHHS)

    admin2Vals <- sort(unique(df_sub$ADMIN2Name))

    updateSelectInput(session, "admin2FilterEnumHHS",
                      choices = c("All", admin2Vals),
                      selected = "All"
    )
  })

  output$plotHHSadm1Enum <- renderPlotly({
    df <- reqData()
    req(input$admin1FilterEnumHHS)
    req(input$admin2FilterEnumHHS)

    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumHHS)

    if (input$admin2FilterEnumHHS != "All") {
      df_filtered <- df_filtered %>%
        filter(ADMIN2Name == input$admin2FilterEnumHHS)
    }

    hhs_data <- df_filtered %>%
      group_by(EnuName, HHS_CH) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(EnuName) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(hhs_data, aes(x = EnuName, y = perc, fill = HHS_CH,
                              text  = paste0(
                                "EnuName: ", EnuName, "<br>",
                                "HHS_CH: ", HHS_CH, "<br>",
                                "n: ", n, "<br>",
                                "perc: ", sprintf("%.2f%%", perc * 100)
                              ))) +
      geom_col(position = "stack") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      scale_fill_manual(values = HHS_colors) +
      labs(x = "Enumerator", y = "Percentage", fill = "HHS Phase") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p,tooltip = "text")
  })

  #########################
  # 7) MATRIX
  #########################

  # 1) Populate Admin1 → Admin2 dynamically
  observe({
    df <- reqData()   # your reactive data
    a1 <- sort(unique(df$ADMIN1Name))
    updateSelectInput(session,"matAdmin1",
                      choices = a1,
                      selected = a1[1])
  })

  observeEvent(input$matAdmin1, {
    df1 <- reqData() %>% filter(ADMIN1Name == input$matAdmin1)
    a2  <- c("All", sort(unique(df1$ADMIN2Name)))
    updateSelectInput(session,"matAdmin2",
                      choices = a2,
                      selected = "All")
  })

  # 2) Reactive slice with the 3 categories + cell index
  mat_df <- reactive({
    req(input$matAdmin1, input$matAdmin2, input$matFCSCat)

    df <- reqData() %>%
      filter(ADMIN1Name == input$matAdmin1)
    if (input$matAdmin2 != "All") {
      df <- df %>% filter(ADMIN2Name == input$matAdmin2)
    }

    df %>% mutate(
      # HHS category
      HHS_cat = case_when(
        HHS == 0        ~ "HHS=0",
        HHS == 1        ~ "HHS=1",
        HHS %in% 2:3    ~ "HHS=2-3",
        HHS == 4        ~ "HHS=4",
        HHS %in% 5:6    ~ "HHS=5-6",
        TRUE             ~ NA_character_
      ),
      # rCSI category
      rCSI_cat = case_when(
        rCSI <  4          ~ "<4",
        rCSI >= 4  & rCSI <= 18 ~ "4-18",
        rCSI >  18         ~ ">18",
        TRUE               ~ NA_character_
      ),
      # choose FCS column
      FCS_cat = .data[[ input$matFCSCat ]]
    )
  })

  # 3) The matrix table
  output$matrixTable <- renderDT({
    df    <- mat_df()
    total <- nrow(df)

    # (a) count up the 3‑way cross‑tab
    long0 <- df %>%
      count(HHS_cat, rCSI_cat, FCS_cat, name = "n")

    # (b) make sure every combo appears
    long1 <- long0 %>%
      complete(
        HHS_cat  = c("HHS=0","HHS=1","HHS=2-3","HHS=4","HHS=5-6"),
        rCSI_cat = c("<4","4-18",">18"),
        FCS_cat  = c("Acceptable","Borderline","Poor"),
        fill     = list(n = 0)
      )

    # (c) compute a 1…45 cell index in row‑major order (5 rows × 9 cols)
    long2 <- long1 %>% mutate(
      # 0‑based indices
      hhs_i  = as.integer(factor(HHS_cat,
                                 levels=c("HHS=0","HHS=1","HHS=2-3","HHS=4","HHS=5-6")))-1,
      rcsi_i = as.integer(factor(rCSI_cat,
                                 levels=c("<4","4-18",">18")))-1,
      fcs_i  = as.integer(factor(FCS_cat,
                                 levels=c("Acceptable","Borderline","Poor")))-1,
      # ROW‑MAJOR: each HHS row has 9 cols
      cell = rcsi_i * 15 + fcs_i * 5 + hhs_i + 1
    )

    # your 45‑long vector of colours, in the exact order 1→45
    cell_colors <- c(
      "#CDFACD","#FAE61E","#FAE61E","#E67800","#E67800",
      "#CDFACD","#FAE61E","#E67800","#E67800","#C80000",
      "#FAE61E","#FAE61E","#E67800","#C80000","#C80000",
      "#FAE61E","#FAE61E","#FAE61E","#E67800","#E67800",
      "#FAE61E","#FAE61E","#E67800","#E67800","#C80000",
      "#FAE61E","#E67800","#E67800","#C80000","#640000",
      "#FAE61E","#FAE61E","#E67800","#E67800","#C80000",
      "#FAE61E","#E67800","#E67800","#C80000","#C80000",
      "#E67800","#E67800","#E67800","#C80000","#640000"
    )

    # (d) build the HTML label, pulling the correct colour & text style
    white_cells <- c(10, 14, 15, 25, 29, 30, 35, 39, 40, 44, 45)

    long3 <- long2 %>%
      mutate(
        pct      = 100 * n / total,
        is_white = cell %in% white_cells,
        style    = paste0(
          "background:", cell_colors[cell], ";",
          "padding:6px;",
          "text-align:center;",
          "font-weight:bold;",                  # make everything bold
          if_else(is_white, "color:white;", "") # turn text white for special cells
        ),
        label = sprintf(
          '<div style="%s">%d<br/><small>(%.1f%%)</small></div>',
          style, n, pct
        )
      ) %>%
      select(-is_white, -style)


    # (e) pivot to 5×9
    wide <- long3 %>%
      select(HHS_cat, rCSI_cat, FCS_cat, label) %>%
      pivot_wider(
        names_from  = c(rCSI_cat, FCS_cat),
        names_sep   = "|",
        values_from = label
      )

    # (f) two‑row header
    sketch <- tags$table(
      class = 'display',
      tags$thead(
        tags$tr(
          tags$th(rowspan="2","HHS \\ rCSI \\ FCS"),
          tags$th(colspan="3","rCSI < 4"),
          tags$th(colspan="3","rCSI 4‑18"),
          tags$th(colspan="3","rCSI > 18")
        ),
        tags$tr(
          rep(
            list(
              tags$th("Acceptable"),
              tags$th("Borderline"),
              tags$th("Poor")
            ),
            3
          )
        )
      )
    )

    datatable(
      wide,
      container  = sketch,
      escape     = FALSE,
      rownames   = FALSE,
      class      = 'nowrap',    # prevent automatic wrapping
      options    = list(
        dom        = 't',
        paging     = FALSE,
        ordering   = FALSE,
        scrollX    = TRUE,       # <-- enable horizontal scrolling
        autoWidth  = TRUE,       # <-- let DT calculate column widths
        columnDefs = list(
          list(className = 'dt-center', targets = 1:(ncol(wide)-1))
        )
      )
    )
  })




  # 4) Details table for illogical cells
  output$matrixDetails <- renderDT({
    # illogical cell indices
    illogical <- c(3, 4, 5, 8, 9, 10)

    # capture which column you chose in the matrix:
    # Decide which FCSCat column to include (only the one selected)
    selFCS <- if (input$matFCSCat == "FCSCat21") "FCSCat21" else "FCSCat28"


    df <- mat_df() %>%
      # re‑derive the cell index exactly as in the main matrix
      mutate(
        hhs_i  = as.integer(factor(HHS_cat,
                                   levels = c("HHS=0","HHS=1","HHS=2-3","HHS=4","HHS=5-6")))-1,
        rcsi_i = as.integer(factor(rCSI_cat,
                                   levels = c("<4","4-18",">18")))-1,
        fcs_i  = as.integer(factor(FCS_cat,
                                   levels = c("Acceptable","Borderline","Poor")))-1,
        cell   = rcsi_i * 15 + fcs_i * 5 + hhs_i + 1
      ) %>%
      filter(cell %in% illogical) %>%
      # only show the selected FCSCat column as "FCS"
      select(
        ADMIN1Name, ADMIN2Name, ADMIN3Name, ADMIN4Name,
        EnuSupervisorName, EnuName,
        HHSizeCalc, cell, rCSI, FCS, all_of(selFCS), HHS, FCSStap,
        FCSPulse, FCSDairy, FCSPr,
        FCSVeg, FCSFruit, FCSFat, FCSSugar,
        rCSILessQlty, rCSIBorrow, rCSIMealSize,
        rCSIMealAdult, rCSIMealNb, HHhSNoFood_FR_r, HHhSBedHung_FR_r, HHhSNotEat_FR_r
      )

    datatable(
      df,
      filter    = "top",
      rownames  = FALSE,
      extensions= c("Buttons","Scroller"),
      options   = list(
        dom         = 'Bfrtip',
        buttons     = list(
          list(extend='copy' , exportOptions=list(modifiers=list(page='all'))),
          list(extend='csv'  , exportOptions=list(modifiers=list(page='all'))),
          list(extend='excel', exportOptions=list(modifiers=list(page='all')))
        ),
        pageLength  = 25,
        scrollY     = '400px',
        scrollX     = TRUE,
        scroller    = TRUE,
        deferRender = TRUE     # recommended for performance with Scroller
      )
    )
  },
  server    = TRUE
  )


  #########################
  # 8) LCS
  #########################

  output$plotLHCSadm1 <- renderPlotly({
    df <- dynamicLCS()

    lcs_data <- df %>%
      group_by(ADMIN1Name, LhCSICat) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN1Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(lcs_data, aes(x = ADMIN1Name, y = perc, fill = LhCSICat,
                              text  = paste0(
                                "ADMIN1Name: ", ADMIN1Name, "<br>",
                                "LCS_Cat: ", LhCSICat, "<br>",
                                "n: ", n, "<br>",
                                "perc: ", sprintf("%.2f%%", perc * 100)
                              ))) +
      geom_col(position = "stack") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90,vjust = 0.5)) +
      scale_fill_manual(values = LHCS_colors) +
      labs(x = "Admin1", y = "Percentage", fill = "LCS Category") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p,tooltip = "text")
  })

  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(session, "admin1FilterLCS2",
                      choices = admin1Vals,
                      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  output$plotLHCSadm2 <- renderPlotly({
    df <- dynamicLCS()
    req(input$admin1FilterLCS2)

    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterLCS2)

    lcs_data <- df_filtered %>%
      group_by(ADMIN2Name, LhCSICat) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN2Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(lcs_data, aes(x = ADMIN2Name, y = perc, fill = LhCSICat,
                              text  = paste0(
                                "ADMIN2Name: ", ADMIN2Name, "<br>",
                                "LCS_Cat: ", LhCSICat, "<br>",
                                "n: ", n, "<br>",
                                "perc: ", sprintf("%.2f%%", perc * 100)
                              ))) +
      geom_col(position = "stack") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90,vjust = 0.5)) +
      scale_fill_manual(values = LHCS_colors) +
      labs(x = "Admin2", y = "Percentage", fill = "LCS Category") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p,tooltip = "text")
  })

  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(session, "admin1FilterEnumLCS",
                      choices = admin1Vals,
                      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  observeEvent(input$admin1FilterEnumLCS, {
    df <- reqData()
    req(input$admin1FilterEnumLCS)

    df_sub <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumLCS)

    admin2Vals <- sort(unique(df_sub$ADMIN2Name))

    updateSelectInput(session, "admin2FilterEnumLCS",
                      choices = c("All", admin2Vals),
                      selected = "All"
    )
  })

  output$plotLHCSadm1Enum <- renderPlotly({
    df <- dynamicLCS()
    req(input$admin1FilterEnumLCS)
    req(input$admin2FilterEnumLCS)

    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumLCS)

    if (input$admin2FilterEnumLCS != "All") {
      df_filtered <- df_filtered %>%
        filter(ADMIN2Name == input$admin2FilterEnumLCS)
    }

    lcs_data <- df_filtered %>%
      group_by(EnuName, LhCSICat) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(EnuName) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(lcs_data, aes(x = EnuName, y = perc, fill = LhCSICat,
                              text  = paste0(
                                "EnuName: ", EnuName, "<br>",
                                "LCS_Cat: ", LhCSICat, "<br>",
                                "n: ", n, "<br>",
                                "perc: ", sprintf("%.2f%%", perc * 100)
                              ))) +
      geom_col(position = "stack") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90,vjust = 0.5)) +
      scale_fill_manual(values = LHCS_colors) +
      labs(x = "Enumerator", y = "Percentage", fill = "LCS Category") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p,tooltip = "text")
  })

  # Update Admin1 for LCS by Strategy
  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(session, "lcsStrategyAdmin1",
                      choices = admin1Vals,
                      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL)
  })

  # When Admin1 is selected, update the Admin2 and Enumerator dropdowns
  observeEvent(input$lcsStrategyAdmin1, {
    req(reqData())
    df <- reqData()
    selected_admin1 <- input$lcsStrategyAdmin1

    # Update Admin2 choices: only those in the selected Admin1.
    admin2_choices <- sort(unique(df %>% filter(ADMIN1Name == selected_admin1) %>% pull(ADMIN2Name)))
    updateSelectInput(session, "lcsStrategyAdmin2",
                      choices = c("All", admin2_choices),
                      selected = "All")

    # Update Enumerator choices: those within the selected Admin1.
    enu_choices <- sort(unique(df %>% filter(ADMIN1Name == selected_admin1) %>% pull(EnuName)))
    updateSelectInput(session, "lcsStrategyEnu",
                      choices = c("All", enu_choices),
                      selected = "All")
  })

  # When Admin2 is selected, further restrict the Enumerator dropdown if not "All"
  observeEvent(input$lcsStrategyAdmin2, {
    req(reqData(), input$lcsStrategyAdmin1)
    df <- reqData()
    selected_admin1 <- input$lcsStrategyAdmin1
    if (input$lcsStrategyAdmin2 != "All") {
      enu_choices <- sort(unique(
        df %>% filter(ADMIN1Name == selected_admin1,
                      ADMIN2Name == input$lcsStrategyAdmin2) %>% pull(EnuName)
      ))
    } else {
      enu_choices <- sort(unique(
        df %>% filter(ADMIN1Name == selected_admin1) %>% pull(EnuName)
      ))
    }
    updateSelectInput(session, "lcsStrategyEnu",
                      choices = c("All", enu_choices),
                      selected = "All")
  })


  output$plotLCSStrategy <- renderPlotly({
    # Start with the main dataset
    df <- reqData()

    # Apply cascading filters based on Admin1, Admin2 and Enumerator
    if (!is.null(input$lcsStrategyAdmin1) && input$lcsStrategyAdmin1 != "") {
      df <- df %>% filter(ADMIN1Name == input$lcsStrategyAdmin1)
    }
    if (!is.null(input$lcsStrategyAdmin2) && input$lcsStrategyAdmin2 != "All") {
      df <- df %>% filter(ADMIN2Name == input$lcsStrategyAdmin2)
    }
    if (!is.null(input$lcsStrategyEnu) && input$lcsStrategyEnu != "All") {
      df <- df %>% filter(EnuName == input$lcsStrategyEnu)
    }

    # Get the selected strategy variable names from the dynamic LCS input
    selected_vars <- c(input$stressVars, input$crisisVars, input$emergencyVars)

    # Validate exactly 10 strategies are selected (4 stress, 3 crisis, 3 emergency)
    shiny::validate(shiny::need(length(selected_vars) == 10,
                  "Please select exactly 10 LCS strategies (4 stress, 3 crisis, 3 emergency) in the Dynamic LCS tab.")
    )

    # Predefined orders for each group (update these vectors if your variable names differ)
    stress_order <- c("Lcs_stress_Saving", "Lcs_stress_DomAsset", "Lcs_stress_ConsActive",
                      "Lcs_stress_SellFoodRation", "Lcs_stress_SellNFIRation", "Lcs_stress_EatOut",
                      "Lcs_stress_BorrowCash", "Lcs_stress_Pawn", "Lcs_stress_LessSchool",
                      "Lcs_stress_Utilities", "Lcs_stress_Edu", "Lcs_stress_BorrowFood",
                      "Lcs_stress_MoreLabour", "Lcs_stress_HHSeparation","Lcs_stress_Housing",
                      "Lcs_stress_LessSchool","LcsR_stress_Animals", "LcsR_stress_BorrowCash",
                      "LcsR_stress_Pawn",	"LcsR_stress_DomAsset",	"LcsR_stress_EatOut",
                      "LcsR_stress_LessSchool",	"LcsR_stress_Saving",	"LcsR_stress_HHSeparation",
                      "LcsR_stress_ConsActive",	"LcsR_stress_SellFoodRation",	"LcsR_stress_DomMigration",
                      "LcsR_stress_Housing",	"LcsR_stress_SellNFIRation",	"LcsR_stress_Edu")
    crisis_order <- c("Lcs_crisis_ProdAssets", "Lcs_crisis_Barter", "Lcs_crisis_Health",
                      "Lcs_crisis_Housing", "Lcs_crisis_HHSeparation", "Lcs_crisis_OutSchool",
                      "Lcs_crisis_Migration", "Lcs_crisis_DomMigration", "Lcs_crisis_ChildWork",
                      "Lcs_crisis_Edu_Health","Lcs_crisis_Barter","Lcs_crisis_ConsActive",
                      "Lcs_crisis_Edu","Lcs_crisis_Health","Lcs_crisis_Marriage","Lcs_crisis_Utilities",
                      "LcsR_crisis_AgriCare", "LcsR_crisis_ImmCrops",
                      "LcsR_crisis_Seed",
                      "LcsR_crisis_Animals",	"LcsR_crisis_Health",
                      "LcsR_crisis_Edu",	"LcsR_crisis_ProdAssets",	"LcsR_crisis_Housing",
                      "LcsR_crisis_HHSeparation",	"LcsR_crisis_Barter",	"LcsR_crisis_Migration",
                      "LcsR_crisis_ChildWork",	"LcsR_crisis_Marriage",	"LcsR_crisis_ConsActive",
                      "LcsR_crisis_OutSchool",	"LcsR_crisis_DomMigration")
    emergency_order <- c("Lcs_em_ChildMigration", "Lcs_em_IllegalAct", "Lcs_em_Begged",
                         "Lcs_em_Marriage", "Lcs_em_ResAsset", "Lcs_em_Migration","Lcs_em_ChildWork",
                         "Lcs_em_OutSchool","LcsR_em_FemAnimal", "LcsR_em_WildFood","LcsR_em_Seed",
                         "LcsR_em_OutSchool",	"LcsR_em_Migration",	"LcsR_em_ChildWork",	"LcsR_em_Marriage",
                         "LcsR_em_ResAsset",	"LcsR_em_Begged",	"LcsR_em_IllegalAct",	"LcsR_em_ChildMigration")

    # Order the selected variables by group: stress, crisis, emergency.
    selected_stress <- intersect(stress_order, selected_vars)
    selected_crisis <- intersect(crisis_order, selected_vars)
    selected_emergency <- intersect(emergency_order, selected_vars)
    desired_order <- c(selected_stress, selected_crisis, selected_emergency)

    # Pivot data from wide to long format so that each row is one strategy response per observation.
    df_long <- df %>%
      pivot_longer(
        cols = all_of(selected_vars),
        names_to = "Strategy",
        values_to = "Response"
      )

    # Create grouping variable "Type" and force an order: Stress, Crisis, Emergency.
    df_long <- df_long %>%
      mutate(
        Type = case_when(
          Strategy %in% stress_order ~ "Stress",
          Strategy %in% crisis_order ~ "Crisis",
          Strategy %in% emergency_order ~ "Emergency",
          TRUE ~ "Other"
        )
      ) %>%
      mutate(
        Type = factor(Type, levels = c("Stress", "Crisis", "Emergency", "Other"))
      )

    # Reorder the Strategy factor using the desired order.
    df_long$Strategy <- factor(df_long$Strategy, levels = desired_order)

    # Convert the Response values to a factor with appropriate labels.
    df_long <- df_long %>%
      mutate(
        Response = factor(as.numeric(Response),
                          levels = c(10, 20, 30, 9999),
                          labels = c("No (10)", "No, already used (20)", "Yes (30)", "Not Applicable (NA-9999)"))
      )

    # Build the 100% stacked bar chart.
    # facet_grid() will create separate panels for each type with free spacing on the x-axis.
    p <- ggplot(df_long, aes(x = Strategy, fill = Response)) +
      geom_bar(position = "fill") +
      facet_grid(. ~ Type, scales = "free_x", space = "free_x") +
      labs(x = "LCS Strategy", y = "Percentage",
           fill = "Response Option") +
      theme_minimal() +
      theme(strip.background = element_rect(fill = "grey90", color = "grey50"),
            strip.text = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::percent)

    ggplotly(p)
  })



  #########################
  # 9) FES
  #########################

  ##################################################
  # FES by Admin1
  ##################################################
  output$plotFESadm1 <- renderPlotly({
    df <- reqData()

    # Summarize
    fes_data <- df %>%
      group_by(ADMIN1Name, Foodexp_4pt) %>%
      summarize(n=n(), .groups="drop") %>%
      group_by(ADMIN1Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(fes_data, aes(x=ADMIN1Name, y=perc, fill=Foodexp_4pt,
                              text  = paste0(
                                "ADMIN1Name: ", ADMIN1Name, "<br>",
                                "Foodexp_4pt: ", Foodexp_4pt, "<br>",
                                "n: ", n, "<br>",
                                "perc: ", sprintf("%.2f%%", perc * 100)
                              ))) +
      geom_col(position="stack") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=90,vjust = 0.5)) +
      scale_fill_manual(values = FES_Colors) +
      labs(x="Admin1", y="Percentage", fill="FES Category") +
      scale_y_continuous(labels=scales::percent_format(accuracy = 1))

    ggplotly(p,tooltip = "text")
  })

  ##################################################
  # FES by Admin2
  ##################################################
  # Populate the forced Admin1 list for sub-tab "FES by Admin2"
  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(session, "admin1FilterFES2",
                      choices = admin1Vals,
                      selected = if(length(admin1Vals)>0) admin1Vals[1] else NULL
    )
  })

  output$plotFESadm2 <- renderPlotly({
    df <- reqData()
    req(input$admin1FilterFES2)

    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterFES2)

    fes_data <- df_filtered %>%
      group_by(ADMIN2Name, Foodexp_4pt) %>%
      summarize(n=n(), .groups="drop") %>%
      group_by(ADMIN2Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(fes_data, aes(x=ADMIN2Name, y=perc, fill=Foodexp_4pt,
                              text  = paste0(
                                "ADMIN2Name: ", ADMIN2Name, "<br>",
                                "Foodexp_4pt: ", Foodexp_4pt, "<br>",
                                "n: ", n, "<br>",
                                "perc: ", sprintf("%.2f%%", perc * 100)
                              ))) +
      geom_col(position="stack") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=90, vjust = 0.5)) +
      scale_fill_manual(values = FES_Colors) +
      labs(x="Admin2", y="Percentage", fill="FES Category") +
      scale_y_continuous(labels=scales::percent_format(accuracy = 1))

    ggplotly(p,tooltip = "text")
  })

  ##################################################
  # FES by Admin1 & Enumerator
  ##################################################
  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(session, "admin1FilterEnumFES",
                      choices = admin1Vals,
                      selected = if(length(admin1Vals)>0) admin1Vals[1] else NULL
    )
  })

  observeEvent(input$admin1FilterEnumFES, {
    df <- reqData()
    req(input$admin1FilterEnumFES)

    df_sub <- df %>% filter(ADMIN1Name == input$admin1FilterEnumFES)
    admin2Vals <- sort(unique(df_sub$ADMIN2Name))

    updateSelectInput(session, "admin2FilterEnumFES",
                      choices = c("All", admin2Vals),
                      selected = "All"
    )
  })

  output$plotFESadm1Enum <- renderPlotly({
    df <- reqData()
    req(input$admin1FilterEnumFES)
    req(input$admin2FilterEnumFES)

    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumFES)

    if (input$admin2FilterEnumFES != "All") {
      df_filtered <- df_filtered %>%
        filter(ADMIN2Name == input$admin2FilterEnumFES)
    }

    fes_data <- df_filtered %>%
      group_by(EnuName, Foodexp_4pt) %>%
      summarize(n=n(), .groups="drop") %>%
      group_by(EnuName) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(fes_data, aes(x=EnuName, y=perc, fill=Foodexp_4pt,
                              text  = paste0(
                                "EnuName: ", EnuName, "<br>",
                                "Foodexp_4pt: ", Foodexp_4pt, "<br>",
                                "n: ", n, "<br>",
                                "perc: ", sprintf("%.2f%%", perc * 100)
                              ))) +
      geom_col(position="stack") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=90,vjust = 0.5)) +
      scale_fill_manual(values = FES_Colors) +
      labs(x="Enumerator", y="Percentage", fill="FES Category") +
      scale_y_continuous(labels=scales::percent_format(accuracy = 1))

    ggplotly(p,tooltip = "text")
  })

  ##################################################
  # Food Expenditure Outlier by Admin1
  ##################################################
  # --- Boxplot: Monthly Food Expenses by Admin1 ---
  output$plotFESBoxAdmin1 <- renderPlotly({
    df <- reqData()

    p <- ggplot(df, aes(x = ADMIN1Name, y = HHExpF_1M)) +
      geom_boxplot(fill = "steelblue") +
      scale_y_continuous(
        labels = comma,               # show 1,000 10,000 etc.
        expand = expansion(mult = c(0, 0.05))
      ) +
      theme_minimal() +
      labs(
        title = "",
        x     = "Admin1",
        y     = "Amount of Monthly Food Expenses"
      )

    ggplotly(p) %>%
      layout(
        yaxis = list(
          tickformat = ",.0f"        # D3 format: comma thousand-sep, no decimals
        )
      )
  })

  ##################################################
  # Food Expenditure Outlier Table by Admin1
  ##################################################

  output$tableFESOutliersAdmin1 <- renderDataTable({
    # Use the full dataset grouped by ADMIN1Name (no Admin1 filter here)
    df <- reqData()

    # Retrieve the recall period (7 days or 1 month) from your common input.
    recall <- as.numeric(input$fesRecall)

    # Helper function to calculate monthly expense for a given food-group prefix.
    calc_monthly_expense <- function(df, prefix, recall) {
      if (recall == 7) {
        cols <- paste0(prefix, c("_Purch_MN_7D", "_GiftAid_MN_7D", "_Own_MN_7D"))
        if (!all(cols %in% names(df))) {
          stop(paste("Missing 7‐day recall columns for", prefix, ":",
                     paste(cols[!cols %in% names(df)], collapse = ", ")))
        }
        # Multiply by (30/7) to scale 7-day data to monthly.
        return(rowSums(select(df, all_of(cols)), na.rm = TRUE) * (30 / 7))
      } else {
        cols <- paste0(prefix, c("_Purch_MN_1M", "_GiftAid_MN_1M", "_Own_MN_1M"))
        if (!all(cols %in% names(df))) {
          stop(paste("Missing 1‐month recall columns for", prefix, ":",
                     paste(cols[!cols %in% names(df)], collapse = ", ")))
        }
        return(rowSums(select(df, all_of(cols)), na.rm = TRUE))
      }
    }

    # Compute monthly expenses for each food group.
    df <- df %>%
      mutate(
        HHExpFCer_MN     = calc_monthly_expense(., "HHExpFCer", recall),
        HHExpFTub_MN     = calc_monthly_expense(., "HHExpFTub", recall),
        HHExpFPuls_MN    = calc_monthly_expense(., "HHExpFPuls", recall),
        HHExpFVeg_MN     = calc_monthly_expense(., "HHExpFVeg", recall),
        HHExpFFrt_MN     = calc_monthly_expense(., "HHExpFFrt", recall),
        HHExpFAnimMeat_MN = calc_monthly_expense(., "HHExpFAnimMeat", recall),
        HHExpFAnimFish_MN = calc_monthly_expense(., "HHExpFAnimFish", recall),
        HHExpFFats_MN    = calc_monthly_expense(., "HHExpFFats", recall),
        HHExpFDairy_MN   = calc_monthly_expense(., "HHExpFDairy", recall),
        HHExpFEgg_MN     = calc_monthly_expense(., "HHExpFEgg", recall),
        HHExpFSgr_MN     = calc_monthly_expense(., "HHExpFSgr", recall),
        HHExpFCond_MN    = calc_monthly_expense(., "HHExpFCond", recall),
        HHExpFBev_MN     = calc_monthly_expense(., "HHExpFBev", recall),
        HHExpFOut_MN     = calc_monthly_expense(., "HHExpFOut", recall)
      )

    # Compute IQR-based outlier thresholds on HHExpF_1M at the Admin1 level.
    df_out <- df %>%
      group_by(ADMIN1Name) %>%
      mutate(
        Q1 = quantile(HHExpF_1M, 0.25, na.rm = TRUE),
        Q3 = quantile(HHExpF_1M, 0.75, na.rm = TRUE),
        IQR = Q3 - Q1,
        lower_bound = Q1 - 1.5 * IQR,
        upper_bound = Q3 + 1.5 * IQR
      ) %>%
      ungroup() %>%
      filter(HHExpF_1M < lower_bound | HHExpF_1M > upper_bound) %>%
      select(ADMIN1Name, ADMIN2Name, ADMIN4Name, EnuSupervisorName, EnuName, HHSizeCalc,
             HHExpF_1M,
             HHExpFCer_MN, HHExpFTub_MN, HHExpFPuls_MN, HHExpFVeg_MN, HHExpFFrt_MN,
             HHExpFAnimMeat_MN, HHExpFAnimFish_MN, HHExpFFats_MN,
             HHExpFDairy_MN, HHExpFEgg_MN, HHExpFSgr_MN, HHExpFCond_MN, HHExpFBev_MN, HHExpFOut_MN)

    # If no outlier rows are found, return a simple datatable with a message.
    if(nrow(df_out) == 0) {
      return(datatable(data.frame(Message = "No outlier observations found."),
                       rownames = FALSE,
                       filter = "top",
                       options = list(scrollX = TRUE)))
    }

    # Create the DataTable with top filters; format numeric columns with commas.
    datatable(df_out, rownames = FALSE, filter = "top", options = list(scrollX = TRUE)) %>%
      formatRound(
        columns = c("HHSizeCalc", "HHExpF_1M", "HHExpFCer_MN", "HHExpFTub_MN", "HHExpFPuls_MN",
                    "HHExpFVeg_MN", "HHExpFFrt_MN", "HHExpFAnimMeat_MN", "HHExpFAnimFish_MN",
                    "HHExpFFats_MN", "HHExpFDairy_MN", "HHExpFEgg_MN", "HHExpFSgr_MN", "HHExpFCond_MN",
                    "HHExpFBev_MN", "HHExpFOut_MN"),
        digits = 0,
        mark = ","
      )
  })

  # --- Download Handler for Excel export for FES Food Expenses by Admin1 ---
  output$downloadOutlierTableAdmin1 <- downloadHandler(
    filename = function() {
      paste0("FES_Outliers_Admin1_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- reqData()
      recall <- as.numeric(input$fesRecall)

      calc_monthly_expense <- function(df, prefix, recall) {
        if (recall == 7) {
          cols <- paste0(prefix, c("_Purch_MN_7D", "_GiftAid_MN_7D", "_Own_MN_7D"))
          if (!all(cols %in% names(df))) {
            stop(paste("Missing 7‐day recall columns for", prefix, ":",
                       paste(cols[!cols %in% names(df)], collapse = ", ")))
          }
          return(rowSums(select(df, all_of(cols)), na.rm = TRUE) * (30 / 7))
        } else {
          cols <- paste0(prefix, c("_Purch_MN_1M", "_GiftAid_MN_1M", "_Own_MN_1M"))
          if (!all(cols %in% names(df))) {
            stop(paste("Missing 1‐month recall columns for", prefix, ":",
                       paste(cols[!cols %in% names(df)], collapse = ", ")))
          }
          return(rowSums(select(df, all_of(cols)), na.rm = TRUE))
        }
      }

      df <- df %>%
        mutate(
          HHExpFCer_MN     = calc_monthly_expense(., "HHExpFCer", recall),
          HHExpFTub_MN     = calc_monthly_expense(., "HHExpFTub", recall),
          HHExpFPuls_MN    = calc_monthly_expense(., "HHExpFPuls", recall),
          HHExpFVeg_MN     = calc_monthly_expense(., "HHExpFVeg", recall),
          HHExpFFrt_MN     = calc_monthly_expense(., "HHExpFFrt", recall),
          HHExpFAnimMeat_MN = calc_monthly_expense(., "HHExpFAnimMeat", recall),
          HHExpFAnimFish_MN = calc_monthly_expense(., "HHExpFAnimFish", recall),
          HHExpFFats_MN    = calc_monthly_expense(., "HHExpFFats", recall),
          HHExpFDairy_MN   = calc_monthly_expense(., "HHExpFDairy", recall),
          HHExpFEgg_MN     = calc_monthly_expense(., "HHExpFEgg", recall),
          HHExpFSgr_MN     = calc_monthly_expense(., "HHExpFSgr", recall),
          HHExpFCond_MN    = calc_monthly_expense(., "HHExpFCond", recall),
          HHExpFBev_MN     = calc_monthly_expense(., "HHExpFBev", recall),
          HHExpFOut_MN     = calc_monthly_expense(., "HHExpFOut", recall)
        )

      df_out <- df %>%
        group_by(ADMIN1Name) %>%
        mutate(
          Q1 = quantile(HHExpF_1M, 0.25, na.rm = TRUE),
          Q3 = quantile(HHExpF_1M, 0.75, na.rm = TRUE),
          IQR = Q3 - Q1,
          lower_bound = Q1 - 1.5 * IQR,
          upper_bound = Q3 + 1.5 * IQR
        ) %>%
        ungroup() %>%
        filter(HHExpF_1M < lower_bound | HHExpF_1M > upper_bound) %>%
        select(ADMIN1Name, ADMIN2Name, ADMIN4Name, EnuSupervisorName, EnuName, HHSizeCalc,
               HHExpF_1M,
               HHExpFCer_MN, HHExpFTub_MN, HHExpFPuls_MN, HHExpFVeg_MN, HHExpFFrt_MN,
               HHExpFAnimMeat_MN, HHExpFAnimFish_MN, HHExpFFats_MN,
               HHExpFDairy_MN, HHExpFEgg_MN, HHExpFSgr_MN, HHExpFCond_MN, HHExpFBev_MN, HHExpFOut_MN)

      numeric_cols <- c("HHSizeCalc", "HHExpF_1M", "HHExpFCer_MN", "HHExpFTub_MN", "HHExpFPuls_MN",
                        "HHExpFVeg_MN", "HHExpFFrt_MN", "HHExpFAnimMeat_MN", "HHExpFAnimFish_MN",
                        "HHExpFFats_MN", "HHExpFDairy_MN", "HHExpFEgg_MN", "HHExpFSgr_MN", "HHExpFCond_MN",
                        "HHExpFBev_MN","HHExpFOut_MN")

      df_out <- df_out %>%
        mutate(across(all_of(numeric_cols),
                      ~ as.character(format(round(.x, 0), big.mark = ",", scientific = FALSE))))

      writexl::write_xlsx(df_out, path = file)
    }
  )

  ##################################################
  # Food Expenditure Outlier by Admin2
  ##################################################
  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(session, "fesBoxAdmin2Admin1",
                      choices = admin1Vals,
                      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL)
  })

  output$plotFESBoxAdmin2 <- renderPlotly({
    df <- reqData()
    req(input$fesBoxAdmin2Admin1)

    # Filter the data by the selected Admin1.
    filtered_df <- df %>% filter(ADMIN1Name == input$fesBoxAdmin2Admin1)

    # Create the boxplot: x-axis = Admin2Name, y-axis = HHExpF_1M.
    p <- ggplot(filtered_df, aes(x = ADMIN2Name, y = HHExpF_1M)) +
      geom_boxplot(fill = "tomato") +
      scale_y_continuous(
        labels = comma,      # 1,000 2,000 etc.
        expand = expansion(mult = c(0, 0.05))
      ) +
      theme_minimal() +
      labs(
        title = paste("Monthly Food Expenses by Admin2 for Admin1 =",
                      input$fesBoxAdmin2Admin1),
        x = "Admin2Name",
        y = "Amount of Monthly Food Expenses"
      )

    ggplotly(p) %>%
      layout(
        yaxis = list(
          tickformat = ",.0f"  # D3 format: comma thousand‐sep, 0 decimal places
        )
      )
  })

  ##################################################
  # Food Expenditure Outlier Table by Admin2
  ##################################################
  output$tableFESOutliersAdmin2 <- renderDataTable({
    # Filter the dataset for the selected Admin1 (for the FES outliers sub‑tab)
    req(input$fesBoxAdmin2Admin1)
    df <- reqData() %>%
      filter(ADMIN1Name == input$fesBoxAdmin2Admin1)

    # Get the recall period chosen by the user (7 days or 1 month)
    recall <- as.numeric(input$fesRecall)

    # Helper function to calculate monthly expense for a given food group prefix.
    calc_monthly_expense <- function(df, prefix, recall) {
      if (recall == 7) {
        cols <- paste0(prefix, c("_Purch_MN_7D", "_GiftAid_MN_7D", "_Own_MN_7D"))
        if (!all(cols %in% names(df))) {
          stop(paste("Missing 7‐day recall columns for", prefix,
                     ":", paste(cols[!cols %in% names(df)], collapse = ", ")))
        }
        return(rowSums(select(df, all_of(cols)), na.rm = TRUE) * (30 / 7))
      } else {
        cols <- paste0(prefix, c("_Purch_MN_1M", "_GiftAid_MN_1M", "_Own_MN_1M"))
        if (!all(cols %in% names(df))) {
          stop(paste("Missing 1‐month recall columns for", prefix,
                     ":", paste(cols[!cols %in% names(df)], collapse = ", ")))
        }
        return(rowSums(select(df, all_of(cols)), na.rm = TRUE))
      }
    }

    # Compute monthly expenses for each food group based on recall.
    df <- df %>%
      mutate(
        HHExpFCer_MN     = calc_monthly_expense(., "HHExpFCer", recall),
        HHExpFTub_MN     = calc_monthly_expense(., "HHExpFTub", recall),
        HHExpFPuls_MN     = calc_monthly_expense(., "HHExpFPuls", recall),
        HHExpFVeg_MN     = calc_monthly_expense(., "HHExpFVeg", recall),
        HHExpFFrt_MN     = calc_monthly_expense(., "HHExpFFrt", recall),
        HHExpFAnimMeat_MN = calc_monthly_expense(., "HHExpFAnimMeat", recall),
        HHExpFAnimFish_MN = calc_monthly_expense(., "HHExpFAnimFish", recall),
        HHExpFFats_MN    = calc_monthly_expense(., "HHExpFFats", recall),
        HHExpFDairy_MN   = calc_monthly_expense(., "HHExpFDairy", recall),
        HHExpFEgg_MN     = calc_monthly_expense(., "HHExpFEgg", recall),
        HHExpFSgr_MN     = calc_monthly_expense(., "HHExpFSgr", recall),
        HHExpFCond_MN    = calc_monthly_expense(., "HHExpFCond", recall),
        HHExpFBev_MN     = calc_monthly_expense(., "HHExpFBev", recall),
        HHExpFOut_MN     = calc_monthly_expense(., "HHExpFOut", recall)
      )

    # Assume HHExpF_1M (overall monthly food expenses) is already computed in the dataset.

    # Compute the outlier thresholds for HHExpF_1M.
    df_out <- df %>%
      group_by(ADMIN2Name) %>%
      mutate(
        Q1 = quantile(HHExpF_1M, 0.25, na.rm = TRUE),
        Q3 = quantile(HHExpF_1M, 0.75, na.rm = TRUE),
        IQR = Q3 - Q1,
        lower_bound = Q1 - 1.5 * IQR,
        upper_bound = Q3 + 1.5 * IQR
      ) %>%
      ungroup() %>%
      filter(HHExpF_1M < lower_bound | HHExpF_1M > upper_bound) %>%
      select(ADMIN1Name, ADMIN2Name, ADMIN4Name, EnuSupervisorName, EnuName,HHSizeCalc,
             HHExpF_1M,
             HHExpFCer_MN, HHExpFTub_MN, HHExpFPuls_MN, HHExpFVeg_MN, HHExpFFrt_MN,
             HHExpFAnimMeat_MN, HHExpFAnimFish_MN, HHExpFFats_MN,
             HHExpFDairy_MN, HHExpFEgg_MN, HHExpFSgr_MN, HHExpFCond_MN, HHExpFBev_MN, HHExpFOut_MN)

    # If no outlier rows are found, show a message in the datatable.
    if(nrow(df_out) == 0) {
      return(datatable(data.frame(Message = "No outlier observations found."),
                       rownames = FALSE,
                       filter = "top",
                       options = list(scrollX = TRUE)))
    }

    # Build the DataTable using the default "top" text inputs for filtering.
    datatable(
      df_out,
      rownames = FALSE,
      filter = "top",
      options = list(scrollX = TRUE)
    ) %>%
      formatRound(
        columns = c("HHSizeCalc","HHExpF_1M", "HHExpFCer_MN", "HHExpFTub_MN", "HHExpFPuls_MN","HHExpFVeg_MN",
                    "HHExpFFrt_MN", "HHExpFAnimMeat_MN", "HHExpFAnimFish_MN",
                    "HHExpFFats_MN", "HHExpFDairy_MN", "HHExpFEgg_MN",
                    "HHExpFSgr_MN", "HHExpFCond_MN", "HHExpFBev_MN", "HHExpFOut_MN"),
        digits = 0,
        mark = ","
      )
  })



  output$downloadOutlierTable <- downloadHandler(
    filename = function() {
      paste0("FES_Outliers_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # Recompute the outlier table following the same steps as above.
      req(input$fesBoxAdmin2Admin1)
      df <- reqData() %>% filter(ADMIN1Name == input$fesBoxAdmin2Admin1)
      recall <- as.numeric(input$fesRecall)

      calc_monthly_expense <- function(df, prefix, recall) {
        if (recall == 7) {
          cols <- paste0(prefix, c("_Purch_MN_7D", "_GiftAid_MN_7D", "_Own_MN_7D"))
          if (!all(cols %in% names(df))) {
            stop(paste("Missing 7-day recall columns for", prefix, ":",
                       paste(cols[!cols %in% names(df)], collapse = ", ")))
          }
          return(rowSums(select(df, all_of(cols)), na.rm = TRUE) * (30 / 7))
        } else {
          cols <- paste0(prefix, c("_Purch_MN_1M", "_GiftAid_MN_1M", "_Own_MN_1M"))
          if (!all(cols %in% names(df))) {
            stop(paste("Missing 1-month recall columns for", prefix, ":",
                       paste(cols[!cols %in% names(df)], collapse = ", ")))
          }
          return(rowSums(select(df, all_of(cols)), na.rm = TRUE))
        }
      }

      df <- df %>%
        mutate(
          HHExpFCer_MN     = calc_monthly_expense(., "HHExpFCer", recall),
          HHExpFTub_MN     = calc_monthly_expense(., "HHExpFTub", recall),
          HHExpFPuls_MN     = calc_monthly_expense(., "HHExpFPuls", recall),
          HHExpFVeg_MN     = calc_monthly_expense(., "HHExpFVeg", recall),
          HHExpFFrt_MN     = calc_monthly_expense(., "HHExpFFrt", recall),
          HHExpFAnimMeat_MN = calc_monthly_expense(., "HHExpFAnimMeat", recall),
          HHExpFAnimFish_MN = calc_monthly_expense(., "HHExpFAnimFish", recall),
          HHExpFFats_MN    = calc_monthly_expense(., "HHExpFFats", recall),
          HHExpFDairy_MN   = calc_monthly_expense(., "HHExpFDairy", recall),
          HHExpFEgg_MN     = calc_monthly_expense(., "HHExpFEgg", recall),
          HHExpFSgr_MN     = calc_monthly_expense(., "HHExpFSgr", recall),
          HHExpFCond_MN    = calc_monthly_expense(., "HHExpFCond", recall),
          HHExpFBev_MN     = calc_monthly_expense(., "HHExpFBev", recall),
          HHExpFOut_MN     = calc_monthly_expense(., "HHExpFOut", recall)
        )

      df_out <- df %>%
        group_by(ADMIN2Name) %>%
        mutate(
          Q1 = quantile(HHExpF_1M, 0.25, na.rm = TRUE),
          Q3 = quantile(HHExpF_1M, 0.75, na.rm = TRUE),
          IQR = Q3 - Q1,
          lower_bound = Q1 - 1.5 * IQR,
          upper_bound = Q3 + 1.5 * IQR
        ) %>%
        ungroup() %>%
        filter(HHExpF_1M < lower_bound | HHExpF_1M > upper_bound) %>%
        select(ADMIN1Name, ADMIN2Name, ADMIN4Name, EnuSupervisorName, EnuName,HHSizeCalc,
               HHExpF_1M,
               HHExpFCer_MN, HHExpFTub_MN, HHExpFPuls_MN, HHExpFVeg_MN, HHExpFFrt_MN,
               HHExpFAnimMeat_MN, HHExpFAnimFish_MN, HHExpFFats_MN,
               HHExpFDairy_MN, HHExpFEgg_MN, HHExpFSgr_MN, HHExpFCond_MN, HHExpFBev_MN,HHExpFOut_MN)

      # Specify the names of the numeric columns that you want formatted.
      numeric_cols <- c("HHSizeCalc","HHExpF_1M", "HHExpFCer_MN", "HHExpFTub_MN", "HHExpFPuls_MN","HHExpFVeg_MN",
                        "HHExpFFrt_MN", "HHExpFAnimMeat_MN", "HHExpFAnimFish_MN",
                        "HHExpFFats_MN", "HHExpFDairy_MN", "HHExpFEgg_MN",
                        "HHExpFSgr_MN", "HHExpFCond_MN", "HHExpFBev_MN", "HHExpFOut_MN")

      # Format each numeric column as a whole number with a comma.
      df_out <- df_out %>%
        mutate(across(all_of(numeric_cols),
                      ~ as.character(format(round(.x, 0), big.mark = ",", scientific = FALSE))))

      # Write the resulting table to Excel.
      writexl::write_xlsx(df_out, path = file)
    }
  )

  #Zero monthly Food Expense

  # helper to compute monthly expense from a prefix + recall
  calc_monthly_expense <- function(df, prefix, recall) {
    if (recall == 7) {
      cols <- paste0(prefix, c("_Purch_MN_7D", "_GiftAid_MN_7D", "_Own_MN_7D"))
      stopifnot(all(cols %in% names(df)))
      rowSums(select(df, all_of(cols)), na.rm = TRUE) * (30/7)
    } else {
      cols <- paste0(prefix, c("_Purch_MN_1M", "_GiftAid_MN_1M", "_Own_MN_1M"))
      stopifnot(all(cols %in% names(df)))
      rowSums(select(df, all_of(cols)), na.rm = TRUE)
    }
  }

  output$zeroExpTable <- DT::renderDataTable({
    df <- req(reqData())
    recall <- as.numeric(req(input$fesRecall))

    # 1) compute all the _MN columns exactly as in your outlier code
    df2 <- df %>%
      mutate(
        HHExpFCer_MN       = calc_monthly_expense(., "HHExpFCer",    recall),
        HHExpFTub_MN       = calc_monthly_expense(., "HHExpFTub",    recall),
        HHExpFPuls_MN      = calc_monthly_expense(., "HHExpFPuls",   recall),
        HHExpFVeg_MN       = calc_monthly_expense(., "HHExpFVeg",    recall),
        HHExpFFrt_MN       = calc_monthly_expense(., "HHExpFFrt",    recall),
        HHExpFAnimMeat_MN  = calc_monthly_expense(., "HHExpFAnimMeat", recall),
        HHExpFAnimFish_MN  = calc_monthly_expense(., "HHExpFAnimFish", recall),
        HHExpFFats_MN      = calc_monthly_expense(., "HHExpFFats",   recall),
        HHExpFDairy_MN     = calc_monthly_expense(., "HHExpFDairy",  recall),
        HHExpFEgg_MN       = calc_monthly_expense(., "HHExpFEgg",    recall),
        HHExpFSgr_MN       = calc_monthly_expense(., "HHExpFSgr",    recall),
        HHExpFCond_MN      = calc_monthly_expense(., "HHExpFCond",   recall),
        HHExpFBev_MN       = calc_monthly_expense(., "HHExpFBev",    recall),
        HHExpFOut_MN       = calc_monthly_expense(., "HHExpFOut",    recall)
      )

    # 2) filter to zeros
    df0 <- df2 %>%
      filter(HHExpF_1M == 0) %>%
      select(
        ADMIN1Name, ADMIN2Name, ADMIN4Name,
        EnuSupervisorName, EnuName,
        HHSizeCalc,
        FCSStap, FCSPulse, FCSPr, FCSVeg, FCSFruit, FCSDairy, FCSFat, FCSSugar,
        HHExpF_1M,
        HHExpFCer_MN, HHExpFTub_MN, HHExpFPuls_MN, HHExpFVeg_MN, HHExpFFrt_MN,
        HHExpFAnimMeat_MN, HHExpFAnimFish_MN, HHExpFFats_MN,
        HHExpFDairy_MN, HHExpFEgg_MN, HHExpFSgr_MN, HHExpFCond_MN,
        HHExpFBev_MN, HHExpFOut_MN
      )

    # 3) render with Buttons + horizontal scroll
    DT::datatable(
      df0,
      rownames   = FALSE,
      extensions = 'Buttons',
      filter     = 'top',
      options    = list(
        dom     = 'Bfrtip',
        buttons = list(
          list(
            extend        = 'csv',
            text          = 'Export CSV',
            exportOptions = list(
              columns  = ':visible',
              modifier = list(page = 'all')
            )
          ),
          list(
            extend        = 'excel',
            text          = 'Export Excel',
            exportOptions = list(
              columns  = ':visible',
              modifier = list(page = 'all')
            )
          )
        ),
        scrollX    = TRUE,
        pageLength = 10
      )
    ) %>%
      DT::formatRound(
        columns = c("HHSizeCalc","HHExpF_1M",
                    "HHExpFCer_MN","HHExpFTub_MN","HHExpFPuls_MN","HHExpFVeg_MN",
                    "HHExpFFrt_MN","HHExpFAnimMeat_MN","HHExpFAnimFish_MN",
                    "HHExpFFats_MN","HHExpFDairy_MN","HHExpFEgg_MN",
                    "HHExpFSgr_MN","HHExpFCond_MN","HHExpFBev_MN","HHExpFOut_MN"
        ),
        digits = 0,
        mark   = ","
      )
  }, server = FALSE)


  #########################
  # 10) CARI
  #########################

  ##################################################
  # CARI CONSOLE TABLE
  ##################################################
  # 1) Compute all of the percentages once, inside a reactive block

  pct <- reactive({
    df_raw <- req(dynamicCARI())

    # 1) Recode each factor into the correct 1:4 ordering,
    #    then turn into plain integers.
    df <- df_raw %>%
      mutate(
        # Economic vulnerability: map your factor levels in the
        # order that corresponds to 1 → 4
        Foodexp_4pt = as.integer(
          fct_relevel(
            Foodexp_4pt,
            "<50%",    # will become 1
            "50-65%",  # 2
            "65-75%",  # 3
            ">75%"     # 4
          )
        ),

        # Livelihood coping strategies:
        LhCSICat = as.integer(
          fct_relevel(
            LhCSICat,
            "NoStrategies",  # 1
            "StressStrategies",     # 2
            "CrisisStrategies",     # 3
            "EmergencyStrategies"   # 4
          )
        ),

        # CARI FES:
        CARI_FES = as.integer(
          fct_relevel(
            CARI_FES,
            "Food secure",              # 1
            "Marginally food secure",    # 2
            "Moderately food insecure",              # 3
            "Severely food insecure"                     # 4
          )
        ),

        # (Your FCS_4pt was already integer, so leave it.)
        FCS_4pt = as.integer(FCS_4pt)
      )

    # 2) Now your pct4() helper will see true integers 1–4
    pct4 <- function(var) {
      df %>%
        filter(!is.na({{ var }})) %>%
        count({{ var }}) %>%
        mutate(p = round(100 * n / sum(n), 0)) %>%
        complete({{ var }} := 1:4, fill = list(p = 0)) %>%
        arrange({{ var }}) %>%
        pull(p)
    }

    # 3) Build your four vectors
    f <- pct4(FCS_4pt)
    e <- pct4(Foodexp_4pt)
    c <- pct4(LhCSICat)
    a <- pct4(CARI_FES)

    # 4) Return the named list for glue_data()
    list(
      fcs1 = f[1], fcs2 = f[2], fcs3 = f[3], fcs4 = f[4],
      exp1 = e[1], exp2 = e[2], exp3 = e[3], exp4 = e[4],
      cs1  = c[1], cs2  = c[2], cs3  = c[3], cs4  = c[4],
      ca1  = a[1], ca2  = a[2], ca3  = a[3], ca4  = a[4]
    )
  })



  # 2) Build the HTML table using glue() and renderUI()
  output$cariCtable <- renderUI({
    p <- pct()

    html <- glue_data(p, "
      <table style='border-collapse:collapse;width:100%;text-align:center;'>
        <tr>
          <th style='border:1px solid #000;width:15%;'>Domain</th>
          <th style='border:1px solid #000;width:20%;'>Indicator</th>
          <th style='border:1px solid #000;background-color:#F9E3D3;width:15%;'>Food Secure</th>
          <th style='border:1px solid #000;background-color:#EEAE7F;width:15%;'>Marginally Food Secure</th>
          <th style='border:1px solid #000;background-color:#F43200;width:15%;'>Moderately Food Insecure</th>
          <th style='border:1px solid #000;background-color:#B60000;color:white;width:15%;'>Severely Food Insecure</th>
        </tr>

        <!-- Current Status row -->
      <tr>
        <td style='border:1px solid #000;'><b>Current Status</b></td>
        <td style='border:1px solid #000;'>
          <em>Food consumption</em><br/>
          <em>FCS and rCSI</em>
        </td>
        <td style='border:1px solid #000;'>Acceptable<br/><b>{fcs1}%</b></td>
        <td style='border:1px solid #000;'>Acceptable &amp; rCSI ≥ 4<br/><b>{fcs2}%</b></td>
        <td style='border:1px solid #000;'>Borderline<br/><b>{fcs3}%</b></td>
        <td style='border:1px solid #000;'>Poor<br/><b>{fcs4}%</b></td>
      </tr>

        <!-- Coping Capacity → Economic Vulnerability -->
      <tr>
        <td style='border:1px solid #000;' rowspan='2'><b>Coping Capacity</b></td>
        <td style='border:1px solid #000;'>
          <em>Economic Vulnerability</em><br/>
          <em>Food Expenditure Share</em>
        </td>
        <td style='border:1px solid #000;'>&lt;50%<br/><b>{exp1}%</b></td>
        <td style='border:1px solid #000;'>50–65%<br/><b>{exp2}%</b></td>
        <td style='border:1px solid #000;'>65–75%<br/><b>{exp3}%</b></td>
        <td style='border:1px solid #000;'>&gt;75%<br/><b>{exp4}%</b></td>
      </tr>

      <!-- Coping Capacity → Livelihood Coping Strategies -->
      <tr>
        <td style='border:1px solid #000;'>
          <em>Livelihood Coping Strategies</em><br/>
          <em>Livelihood coping strategies</em>
        </td>
        <td style='border:1px solid #000;'>No coping<br/><b>{cs1}%</b></td>
        <td style='border:1px solid #000;'>Stress<br/><b>{cs2}%</b></td>
        <td style='border:1px solid #000;'>Crisis<br/><b>{cs3}%</b></td>
        <td style='border:1px solid #000;'>Emergency<br/><b>{cs4}%</b></td>
      </tr>

        <!-- Final CARI row, merging Domain + Indicator -->
      <tr>
        <td style='border:1px solid #000; background-color:#ccc;' colspan='2'><b>CARI</b></td>
        <td style='border:1px solid #000; background-color:#ccc;'><b>{ca1}%</b></td>
        <td style='border:1px solid #000; background-color:#ccc;'><b>{ca2}%</b></td>
        <td style='border:1px solid #000; background-color:#ccc;'><b>{ca3}%</b></td>
        <td style='border:1px solid #000; background-color:#ccc;'><b>{ca4}%</b></td>
      </tr>
      </table>
    ") %>%
      as.character()

    HTML(html)
  })



  ##################################################
  # CARI by Admin1
  ##################################################
  # (B) CARI by Admin1 => No admin filter
  output$plotCARIadm1 <- renderPlotly({
    df_cari <- dynamicCARI()

    # Summarize final CARI_FES across all Admin1
    chart_data <- df_cari %>%
      group_by(ADMIN1Name, CARI_FES) %>%
      summarize(n = n(), .groups="drop") %>%
      group_by(ADMIN1Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(chart_data, aes(x=ADMIN1Name, y=perc, fill=CARI_FES,
                                text  = paste0(
                                  "ADMIN1Name: ", ADMIN1Name, "<br>",
                                  "CARI: ", CARI_FES, "<br>",
                                  "n: ", n, "<br>",
                                  "perc: ", sprintf("%.2f%%", perc * 100)
                                ))) +
      geom_col(position="stack") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle=90,vjust = 0.5)) +
      scale_fill_manual(values = CARI_Colors) +
      labs(x="Admin1", y="Percentage", fill="CARI_FES") +
      scale_y_continuous(labels=scales::percent_format(accuracy = 1))

    ggplotly(p,tooltip = "text")
  })


  ##################################################
  # CARI by Admin2
  ##################################################
  observeEvent(dynamicCARI(), {
    df <- dynamicCARI()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(session, "admin1FilterCARI2",
                      choices=admin1Vals,
                      selected=if(length(admin1Vals)>0) admin1Vals[1] else NULL
    )
  })

  output$plotCARIadm2 <- renderPlotly({
    df_cari <- dynamicCARI()
    req(input$admin1FilterCARI2)

    df_filtered <- df_cari %>%
      filter(ADMIN1Name == input$admin1FilterCARI2)

    chart_data <- df_filtered %>%
      group_by(ADMIN2Name, CARI_FES) %>%
      summarize(n=n(), .groups="drop") %>%
      group_by(ADMIN2Name) %>%
      mutate(perc=n/sum(n)) %>%
      ungroup()

    p <- ggplot(chart_data, aes(x=ADMIN2Name, y=perc, fill=CARI_FES,
                                text  = paste0(
                                  "ADMIN2Name: ", ADMIN2Name, "<br>",
                                  "CARI: ", CARI_FES, "<br>",
                                  "n: ", n, "<br>",
                                  "perc: ", sprintf("%.2f%%", perc * 100)
                                ))) +
      geom_col(position="stack") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=90,vjust = 0.5)) +
      scale_fill_manual(values = CARI_Colors) +
      labs(x="Admin2", y="Percentage", fill="CARI_FES") +
      scale_y_continuous(labels=scales::percent_format(accuracy = 1))

    ggplotly(p,tooltip = "text")
  })

  ##################################################
  # CARI by Admin1 & Enumerator
  ##################################################
  ##############################################################################
  # 1) Observe dynamicCARI() => populate Admin1 dropdown
  ##############################################################################
  observeEvent(dynamicCARI(), {
    df <- dynamicCARI()
    admin1Vals <- sort(unique(df$ADMIN1Name))

    updateSelectInput(session, "admin1FilterEnumCARI",
                      choices = admin1Vals,
                      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  ##############################################################################
  # 2) Whenever user picks an Admin1, populate Admin2 with "All" + real Admin2
  ##############################################################################
  observeEvent(input$admin1FilterEnumCARI, {
    df <- dynamicCARI()
    req(input$admin1FilterEnumCARI)

    # Filter data to chosen Admin1
    df_sub <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumCARI)

    # Gather unique textual Admin2
    admin2Vals <- sort(unique(df_sub$ADMIN2Name))

    updateSelectInput(session, "admin2FilterEnumCARI",
                      choices = c("All", admin2Vals),
                      selected = "All"
    )
  })

  ##############################################################################
  # 3) Produce the chart
  ##############################################################################
  output$plotCARIadm1Enum <- renderPlotly({
    df_cari <- dynamicCARI()

    # 1) Force user to pick an Admin1
    req(input$admin1FilterEnumCARI)
    # 2) They also pick an Admin2 or "All"
    req(input$admin2FilterEnumCARI)

    # Filter by chosen Admin1
    df_filtered <- df_cari %>%
      filter(ADMIN1Name == input$admin1FilterEnumCARI)

    # If user picks a specific Admin2 (not "All"), filter further
    if (input$admin2FilterEnumCARI != "All") {
      df_filtered <- df_filtered %>%
        filter(ADMIN2Name == input$admin2FilterEnumCARI)
    }

    # Summarize enumerator distribution of final CARI_FES
    chart_data <- df_filtered %>%
      group_by(EnuName, CARI_FES) %>%
      summarize(n = n(), .groups="drop") %>%
      group_by(EnuName) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(chart_data, aes(x = EnuName, y = perc, fill = CARI_FES,
                                text  = paste0(
                                  "EnuName: ", EnuName, "<br>",
                                  "CARI: ", CARI_FES, "<br>",
                                  "n: ", n, "<br>",
                                  "perc: ", sprintf("%.2f%%", perc * 100)
                                ))) +
      geom_col(position="stack") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90,vjust = 0.5)) +
      scale_fill_manual(values = CARI_Colors) +
      labs(x = "Enumerator", y = "Percentage", fill = "CARI_FES") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p,tooltip = "text")
  })

  ##############################################################################
  # 11) Report - generate html report
  ##############################################################################

  ### Track visited tabs (main ones from sidebar and, optionally, sub-tabs)
  visitedTabs <- reactiveValues(visited = character())

  # Observe the main sidebar menu selection (id = "tabs")
  observe({
    req(input$tabs)
    visitedTabs$visited <- unique(c(visitedTabs$visited, input$tabs))
    cat("Main tab visited:", input$tabs, "\n")
  })

  # Optionally, observe sub-tab selections if you want to include them
  observe({
    if (!is.null(input$surveyTabs)) {
      visitedTabs$visited <- unique(c(visitedTabs$visited, input$surveyTabs))
    }
    if (!is.null(input$fcsTabs)) {
      visitedTabs$visited <- unique(c(visitedTabs$visited, input$fcsTabs))
    }
    if (!is.null(input$hddsTabs)) {
      visitedTabs$visited <- unique(c(visitedTabs$visited, input$hddsTabs))
    }
    if (!is.null(input$rcsiTabs)) {
      visitedTabs$visited <- unique(c(visitedTabs$visited, input$rcsiTabs))
    }
    if (!is.null(input$hhsTabs)) {
      visitedTabs$visited <- unique(c(visitedTabs$visited, input$hhsTabs))
    }
    if (!is.null(input$lcsTabs)) {
      visitedTabs$visited <- unique(c(visitedTabs$visited, input$lcsTabs))
    }
    if (!is.null(input$fesTabs)) {
      visitedTabs$visited <- unique(c(visitedTabs$visited, input$fesTabs))
    }
    if (!is.null(input$cariTabs)) {
      visitedTabs$visited <- unique(c(visitedTabs$visited, input$cariTabs))
    }
  })

  # (Optional) Debug print of visited tabs
  output$debugVisitedTabs <- renderPrint({
    if (length(visitedTabs$visited) == 0)
      "No tabs visited yet."
    else
      visitedTabs$visited
  })

  # Render the UI for the download button: only show when all required main tabs are visited.
  output$reportDownloadUI <- renderUI({
    req(visitedTabs$visited)
    requiredTabs <- c("upload", "survey", "fcs", "hdds", "rcsi", "hhs", "lcs", "fes", "cari")
    missingTabs <- setdiff(requiredTabs, visitedTabs$visited)
    if (length(missingTabs) == 0) {
      downloadButton("downloadReport", "Generate HTML Report")
    } else {
      div(style = "color:red; font-weight:bold;",
          paste("Please visit the following tabs before generating the report:",
                paste(missingTabs, collapse = ", "))
      )
    }
  })

  # Download handler for generating the HTML report
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("Report_", Sys.Date(), ".html")
    },
    content = function(file) {
      # 1. Find the path to the Rmd template shipped in the package:
      rmd_template <- system.file("app", "report.Rmd", package = "myshinyapp")
      if (rmd_template == "") {
        stop("Cannot find report.rmd in the installed package.")
      }

      # 2. Copy it to a temporary directory; render from there:
      temp_rmd <- tempfile(fileext = ".Rmd")
      file.copy(rmd_template, temp_rmd, overwrite = TRUE)

      # Render the R Markdown report to HTML
      rmarkdown::render(input = temp_rmd, output_file = file,
                        params = list(
                          dataset = reqData(),
                          dynamicCARI = dynamicCARI(),
                          admin1Filter = input$admin1Filter,
                          filterEnumerator  = input$filterEnumerator,
                          fcsThresholdAdm1    = input$fcsThresholdAdm1,
                          admin1FilterHDDS2   = input$admin1FilterHDDS2,
                          admin1FilterEnumHDDS     = input$admin1FilterEnumHDDS,
                          admin2FilterEnumHDDS     = input$admin2FilterEnumHDDS,
                          admin1FilterrCSI2   = input$admin1FilterrCSI2,
                          admin1FilterEnumrCSI = input$admin1FilterEnumrCSI,
                          admin2FilterEnumrCSI = input$admin2FilterEnumrCSI,
                          admin1FilterFCS2 = input$admin1FilterFCS2,
                          fcsThresholdAdm2 = input$fcsThresholdAdm2,
                          admin1FilterEnumFCS = input$admin1FilterEnumFCS,
                          admin2FilterEnumFCS = input$admin2FilterEnumFCS,
                          fcsThresholdAdm1Enum = input$fcsThresholdAdm1Enum,
                          rcsiBoxAdmin2Admin1 = input$rcsiBoxAdmin2Admin1,
                          triangAdmin1   = input$triangAdmin1,
                          triangAdmin2   = input$triangAdmin2,
                          triangFCSCat   = input$triangFCSCat,
                          admin1FilterHHS2 = input$admin1FilterHHS2,
                          admin1FilterEnumHHS = input$admin1FilterEnumHHS,
                          admin2FilterEnumHHS = input$admin2FilterEnumHHS,
                          matAdmin1   = input$matAdmin1,
                          matAdmin2   = input$matAdmin2,
                          matFCSCat   = input$matFCSCat,
                          stressVars    = input$stressVars,
                          crisisVars    = input$crisisVars,
                          emergencyVars = input$emergencyVars,
                          admin1FilterLCS2 = input$admin1FilterLCS2,
                          admin1FilterEnumLCS = input$admin1FilterEnumLCS,
                          admin2FilterEnumLCS = input$admin2FilterEnumLCS,
                          lcsStrategyAdmin1    = input$lcsStrategyAdmin1,
                          lcsStrategyAdmin2    = input$lcsStrategyAdmin2,
                          lcsStrategyEnu       = input$lcsStrategyEnu,
                          admin1FilterFES2 = input$admin1FilterFES2,
                          admin1FilterEnumFES = input$admin1FilterEnumFES,
                          admin2FilterEnumFES = input$admin2FilterEnumFES,
                          fesBoxAdmin2Admin1 = input$fesBoxAdmin2Admin1,
                          admin1FilterCARI2 = input$admin1FilterCARI2,
                          admin1FilterEnumCARI = input$admin1FilterEnumCARI,
                          admin2FilterEnumCARI = input$admin2FilterEnumCARI
                        ),
                        envir = new.env(parent = globalenv()))
    }
  )
}

############################
## Run the Shiny App
############################
shinyApp(ui = ui, server = server)
