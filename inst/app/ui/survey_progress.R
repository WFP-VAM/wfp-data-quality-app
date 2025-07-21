surveyProgressUI <- function() {
    tagList(
                        fluidRow(
                            tabBox(
                                id = "surveyTabs", width = 12,

                                ## Sub-tab A) Submissions Over Time
                                tabPanel(
                                    "Submissions Over Time",
                                    fluidRow(
                                        box(
                                            title = "Submissions Over Time", status = "primary", solidHeader = TRUE, width = 12,
                                            plotlyOutput("plotSubmission")
                                        )
                                    )
                                ),

                                ## Sub-tab B) Count by Admin1
                                tabPanel(
                                    "Submission by Admin1",
                                    fluidRow(
                                        box(
                                            title = "Submission by Admin1", status = "primary", solidHeader = TRUE, width = 12,
                                            plotlyOutput("plotAdm1")
                                        )
                                    )
                                ),

                                ## Sub-tab C) Count by Admin2 (single Admin1 filter)
                                tabPanel(
                                    "Submission by Admin2",
                                    fluidRow(
                                        box(
                                            title = "Select Admin1Name", status = "warning", solidHeader = TRUE, width = 4,
                                            selectInput("admin1Filter", "Admin1Name:",
                                                choices = character(0), # populated in server
                                                selected = NULL
                                            )
                                        ),
                                        box(
                                            title = "Submission by Admin2", status = "primary", solidHeader = TRUE, width = 8,
                                            plotlyOutput("plotAdm2Filter")
                                        )
                                    )
                                ),

                                ## Sub-tab D) Enumerator by Admin1Name (Histogram)
                                tabPanel(
                                    "Submission by Enumerator by Admin level",
                                    # first row: two filters side by side
                                    fluidRow(
                                        box(
                                            title = "Select Admin1Name", status = "warning", solidHeader = TRUE, width = 6,
                                            selectInput("admin1FilterEnum", "Admin1Name:",
                                                choices = character(0),
                                                selected = NULL
                                            )
                                        ),
                                        box(
                                            title = "Select Admin2Name", status = "warning", solidHeader = TRUE, width = 6,
                                            selectInput("admin2FilterEnum", "Admin2Name:",
                                                choices = character(0),
                                                selected = NULL
                                            )
                                        )
                                    ),
                                    # second row: the histogram
                                    fluidRow(
                                        box(
                                            title = "Surveys by Enumerator (Histogram)", status = "primary", solidHeader = TRUE, width = 12,
                                            plotlyOutput("plotAdm1Enum")
                                        )
                                    )
                                ),

                                ## Sub-tab E) Enumerator by Admin2Name (Treemap)
                                tabPanel(
                                    "Submission by Enumerator (by Admin2) Treemap",
                                    fluidRow(
                                        column(
                                            4,
                                            box(
                                                title = "Select Enumerator", status = "warning", solidHeader = TRUE, width = 12,
                                                selectInput("filterEnumerator", "Enumerator:",
                                                    choices = character(0), # updated in server
                                                    selected = NULL,
                                                    multiple = FALSE
                                                )
                                            )
                                        ),
                                        column(
                                            8,
                                            box(
                                                title = "Treemap by Admin2Name", status = "primary", solidHeader = TRUE, width = 12,
                                                textOutput("treemapCountEnumerator"), # Show total # of surveys
                                                plotlyOutput("plotAdm2EnumTree")
                                            )
                                        )
                                    )
                                )
                            )
                        )
    )
}