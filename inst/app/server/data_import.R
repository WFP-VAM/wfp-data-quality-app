#' Data Import Module UI
#' @param id Module ID
data_import_ui <- function(id) {
    ns <- NS(id)

    tagList(
        fileInput(ns("file"), "Choose SPSS File (.sav)",
            accept = c(".sav")
        ),

        # MoDa inputs
        textInput(ns("moda_formid"), "MoDa Form ID"),
        textInput(ns("moda_token"), "API Token"),
        actionButton(ns("moda_load"), "Load from MoDa"),

        # Status messages
        verbatimTextOutput(ns("uploadStatus")),
        verbatimTextOutput(ns("modaStatus"))
    )
}

#' Data Import Module Server
#' @param id Module ID
data_import_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # SPSS file handling
        spss_data <- reactive({
            req(input$file)
            ext <- tools::file_ext(input$file$name)
            validate(need(ext == "sav", "Please upload an SPSS .sav file"))
            haven::read_sav(input$file$datapath)
        })

        # MoDa API handling
        moda_error <- reactiveVal(NULL)

        moda_data <- eventReactive(input$moda_load, {
            req(input$moda_formid, input$moda_token)
            moda_error(NULL)

            tryCatch(
                {
                    # API interaction code moved here
                    # ... existing MoDa API code ...
                },
                error = function(e) {
                    moda_error(e$message)
                    NULL
                }
            )
        })

        # Status outputs
        output$uploadStatus <- renderPrint({
            req(spss_data())
            str(spss_data())
        })

        output$modaStatus <- renderText({
            req(input$moda_load)
            if (!is.null(moda_error())) {
                paste("❌", moda_error())
            } else if (is.null(moda_data()) || nrow(moda_data()) == 0) {
                "⚠️ No rows returned from MoDa."
            } else {
                paste("✅ Loaded", nrow(moda_data()), "rows from MoDa.")
            }
        })

        # Return reactive expression with the data
        reactive({
            if (!is.null(input$file)) {
                spss_data()
            } else if (!is.null(input$moda_load)) {
                moda_data()
            }
        })
    })
}