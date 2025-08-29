hhdsUI <- function(id) {
    ns <- NS(id)
    tagList(
        # insert here code
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
    )
}

