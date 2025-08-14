fcsUI <- function(id) {
    ns <- NS(id)
    tagList(
        # insert here code
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
    )
}