dataUploadUI <- function() {
    tagList(
                        fluidRow(
                    box(
                        width = 12,
                        selectInput("data_source", "Choose data source:",
                            choices = c(
                                "Upload .sav file" = "spss",
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
                            helpText("Allowed size up to 500 MB.")
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
                            textInput("moda_formid", "Form ID", placeholder = "e.g. 152205"),
                            textInput("moda_token", "Bearer token"),
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
    )
}