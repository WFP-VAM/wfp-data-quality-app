tab_tracking_module <- function(input, output, session) {
    visitedTabs <- reactiveValues(visited = character())

    observe({
        req(input$tabs)
        visitedTabs$visited <- unique(c(visitedTabs$visited, input$tabs))
    })

    observe({
        for (id in c("surveyTabs", "fcsTabs", "hddsTabs", "rcsiTabs", "hhsTabs", "lcsTabs", "fesTabs", "cariTabs")) {
            if (!is.null(input[[id]])) {
                visitedTabs$visited <- unique(c(visitedTabs$visited, input[[id]]))
            }
        }
    })

    output$debugVisitedTabs <- renderPrint({
        if (length(visitedTabs$visited) == 0) "No tabs visited yet." else visitedTabs$visited
    })

    output$reportDownloadUI <- renderUI({
        req(visitedTabs$visited)
        requiredTabs <- c("upload", "survey", "fcs", "hdds", "rcsi", "hhs", "lcs", "fes", "cari")
        missingTabs <- setdiff(requiredTabs, visitedTabs$visited)
        if (length(missingTabs) == 0) {
            downloadButton("downloadReport", "Generate HTML Report")
        } else {
            div(
                style = "color:red; font-weight:bold;",
                paste(
                    "Please visit the following tabs before generating the report:",
                    paste(missingTabs, collapse = ", ")
                )
            )
        }
    })

    return(list(visitedTabs = reactive(visitedTabs$visited)))
}
