rcsiUI <- function() {
    tagList(
        # insert here code
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
    )
}