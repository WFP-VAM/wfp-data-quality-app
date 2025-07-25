report_download_module <- function(input, output, session, reqData, dynamicCARI) {
    output$downloadReport <- downloadHandler(
        filename = function() {
            paste0("Report_", Sys.Date(), ".html")
        },
        content = function(file) {
            rmd_template <- system.file("app", "report.Rmd", package = "myshinyapp")
            if (rmd_template == "") stop("Cannot find report.rmd in the installed package.")

            temp_rmd <- tempfile(fileext = ".Rmd")
            file.copy(rmd_template, temp_rmd, overwrite = TRUE)

            rmarkdown::render(
                input = temp_rmd,
                output_file = file,
                params = list(
                    dataset = reqData(),
                    dynamicCARI = dynamicCARI(),
                    admin1Filter = input$admin1Filter,
                    # ... all other parameters ...
                    admin2FilterEnumCARI = input$admin2FilterEnumCARI
                ),
                envir = new.env(parent = globalenv())
            )
        }
    )
}
