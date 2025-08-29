library(shiny)
library(DT)

fewsnetMatrixUI <- function(id) {
    ns <- NS(id)
    tagList(
        # insert here code
                        fluidRow(
                            tabBox(
                                id = "matrixTabs", width = 12,
                                tabPanel(
                                    "Matrix",
                                    sidebarLayout(
                                        sidebarPanel(
                                            selectInput("matAdmin1", "Admin1:", choices = NULL),
                                            selectInput("matAdmin2", "Admin2:", choices = "All"),
                                            selectInput("matFCSCat", "FCS Category:",
                                                choices = c("FCSCat21", "FCSCat28"),
                                                selected = "FCSCat21"
                                            )
                                        ),
                                        mainPanel(
                                            DTOutput("matrixTable"),
                                            br(),
                                            h4("Detail — illogical cells numbers (3,4,5,8,9,10)"),
                                            DTOutput("matrixDetails")
                                        )
                                    )
                                ),
                            ) # end tabBox
                        )
    )
}