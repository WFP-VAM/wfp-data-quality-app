data_preview_module <- function(input, output, reqData) {
  output$fileUploadedMessage <- renderPrint({
    if (is.null(input$file)) {
      "No file uploaded yet."
    } else {
      paste("File uploaded:", input$file$name)
    }
  })

  output$dataPreview <- DT::renderDataTable({
    df <- reqData()

    cols_to_show <- c(
      "ADMIN1Name", "ADMIN2Name", "ADMIN4Name", "EnuName", "EnuSupervisorName",
      "HHSizeCalc", "FcSStap", "FCSPulse", "FCSDiary", "FCSVeg",
      "FCSPr", "FCSFruit", "FCSFat", "FCSSugar"
    )

    df_sub <- df[, intersect(cols_to_show, names(df)), drop = FALSE]

    DT::datatable(
      head(df_sub, 10),
      options = list(scrollY = "300px", scrollX = TRUE, paging = FALSE),
      class = "stripe hover"
    )
  })
}


summary_boxes_module <- function(output, reqData) {
  output$obsBox <- renderValueBox({
    df <- req(reqData())
    valueBox(
      value    = formatC(nrow(df), big.mark = ","),
      subtitle = "Observations",
      icon     = icon("table"),
      color    = "aqua"
    )
  })

  output$varBox <- renderValueBox({
    df <- req(reqData())
    valueBox(
      value    = formatC(ncol(df), big.mark = ","),
      subtitle = "Variables",
      icon     = icon("columns"),
      color    = "light-blue"
    )
  })

  output$dupBox <- renderValueBox({
    df <- req(reqData())
    startCol <- match("ADMIN1Name", names(df))
    endCol   <- match("instanceID",  names(df))
    num_dup  <- 0

    if (!is.na(startCol) && !is.na(endCol) && startCol < endCol) {
      subset_cols <- df[, startCol:(endCol - 1), drop = FALSE]
      dup_df <- subset_cols %>%
        group_by(across(everything())) %>%
        tally(name = "count") %>%
        filter(count > 1)

      num_dup <- sum(dup_df$count) - nrow(dup_df)
    }

    valueBox(
      value    = num_dup,
      subtitle = "Duplicate Rows",
      icon     = icon("copy"),
      color    = "red"
    )
  })
}
