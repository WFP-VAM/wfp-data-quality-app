fesUI <- function() {
    tagList(
        # insert here code
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
    )
}