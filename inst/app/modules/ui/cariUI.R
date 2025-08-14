cariUI <- function(id) {
    ns <- NS(id)
    tagList(
        # insert here code
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
    )
}