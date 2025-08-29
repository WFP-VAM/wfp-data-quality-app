
# data sources
spss <- "spss"
moda <- "moda"


dataUploadUI <- function(id) {
    ns = NS(id)
    tagList(
        dataSelectionUI("dataSelection"),
        fileInputUI("spss_input", spss, ".sav"),
        modaAPIUI("moda_input", moda),
    # Preview & summary (same for both)
    fluidRow(dataPreviewUI("data_preview")),
    fluidRow(dataSummaryUI("data_summary")),
)
}

dataSelectionUI <- function(id){
    ns <- NS(id)
    fluidRow(
        box(
            width = 12,
            selectInput(
                "data_source",
                "Choose data source:",
                choices = c(
                    "Upload .sav file" = "spss",
                    "Load from MoDa API" = "moda"
                )
            )
        )
    )
}

fileInputUI <- function(id, data_source, ext){
    ns <- NS(id)
    conditionalPanel(
        condition = glue("input.data_source == '{data_source}'"),
        fluidRow(
            box(
                title = glue("Upload {data_source} ({ext}) Data"),
                width = 6,
                status = "primary",
                solidHeader = TRUE,
                fileInput("file", glue("Choose a {ext} file"), accept = ext),
                helpText("Allowed size up to 500 MB.")
            ),
            box(
                title = "File Info",
                width = 6,
                status = "info",
                solidHeader = TRUE,
                verbatimTextOutput("fileUploadedMessage")
            )
        )
    )
}

modaAPIUI <- function(id, data_source){
    ns <- NS(id)
    conditionalPanel(
        condition = glue("input.data_source == '{data_source}'"),
        fluidRow(
            box(
                title = "MoDa API settings",
                width = 6,
                status = "warning",
                solidHeader = TRUE,
                textInput(
                    "moda_formid",
                    "Form ID",
                    placeholder = "e.g. 152205"
                ),
                textInput("moda_token", "Bearer token"),
                actionButton("moda_load", "Load from MoDa")
            ),
            box(
                title = "MoDa status",
                width = 6,
                status = "info",
                solidHeader = TRUE,
                verbatimTextOutput("moda_status")
            )
        )
    )
}

dataPreviewUI <- function(id){
    box(
        title = "Preview of Data",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        DT::dataTableOutput("dataPreview")
    )
}

dataSummaryUI <- function(id){
    box(
        title = "Data Summary",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        fluidRow(
            valueBoxOutput("obsBox", width = 4),
            valueBoxOutput("varBox", width = 4),
            valueBoxOutput("dupBox", width = 4)
        )
    )
}