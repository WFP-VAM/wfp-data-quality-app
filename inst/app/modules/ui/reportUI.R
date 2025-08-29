reportUI <- function(id) {
    ns <- NS(id)
    tagList(
        # insert here code
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
}