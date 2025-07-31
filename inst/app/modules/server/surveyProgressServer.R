survey_exploration_module <- function(input, output, session, reqData) {

  # A) Submissions Over Time
  output$plotSubmission <- renderPlotly({
    df <- reqData()
    submission <- df %>% group_by(Survey_date) %>% count()
    p <- ggplot(submission, aes(x = Survey_date, y = n)) +
      geom_line(color = "steelblue") +
      theme_minimal() +
      labs(x = "Date", y = "Number of Submissions")
    ggplotly(p)
  })

  # B) Count by Admin1
  output$plotAdm1 <- renderPlotly({
    df <- reqData()
    countsadm1table <- df %>% group_by(ADMIN1Name) %>% count()
    p <- ggplot(countsadm1table, aes(x = reorder(ADMIN1Name, -n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_minimal() +
      labs(x = "ADMIN1Name", y = "Number of surveys")
    ggplotly(p)
  })

  # C) Count by Admin2 - Single Admin1Name filter
  observeEvent(reqData(), {
    df <- reqData()
    admin1List <- sort(unique(df$ADMIN1Name))
    updateSelectInput(session, "admin1Filter", choices = admin1List, selected = admin1List[1])
  })

  output$plotAdm2Filter <- renderPlotly({
    df <- reqData()
    req(input$admin1Filter)
    df_filtered <- df %>% filter(ADMIN1Name == input$admin1Filter)
    counts_by_admin2 <- df_filtered %>% group_by(ADMIN2Name) %>% count() %>% arrange(desc(n))
    p <- ggplot(counts_by_admin2, aes(x = reorder(ADMIN2Name, -n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_minimal() +
      labs(x = "ADMIN2Name", y = "Number of surveys") +
      theme(axis.text.x = element_text(angle = 90))
    ggplotly(p)
  })

  # D) Surveys by Enumerator & Admin1Name (Histogram)
  observeEvent(reqData(), {
    df <- reqData()
    admin1List <- sort(unique(df$ADMIN1Name))
    updateSelectInput(session, "admin1FilterEnum", choices = admin1List, selected = admin1List[1])
  })

  observeEvent(input$admin1FilterEnum, {
    df <- reqData()
    req(input$admin1FilterEnum)
    admin2List <- sort(unique(df$ADMIN2Name[df$ADMIN1Name == input$admin1FilterEnum]))
    admin2List <- c("All", admin2List)
    updateSelectInput(session, "admin2FilterEnum", choices = admin2List, selected = "All")
  })

  output$plotAdm1Enum <- renderPlotly({
    df <- req(reqData())
    req(input$admin1FilterEnum, input$admin2FilterEnum)
    df_filtered <- df %>% filter(ADMIN1Name == input$admin1FilterEnum)
    if (input$admin2FilterEnum != "All") {
      df_filtered <- df_filtered %>% filter(ADMIN2Name == input$admin2FilterEnum)
    }
    counts <- df_filtered %>% count(EnuName, name = "n")
    p <- ggplot(counts, aes(x = reorder(EnuName, -n), y = n)) +
      geom_col(fill = "steelblue") +
      theme_minimal() +
      labs(x = "Enumerator", y = "Number of surveys") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplotly(p)
  })

  # E) Surveys by Enumerator & Admin2Name (Treemap)
  observeEvent(reqData(), {
    df <- reqData()
    enumerators <- sort(unique(df$EnuName))
    updateSelectInput(session, "filterEnumerator", choices = enumerators, selected = enumerators[1])
  })

  output$treemapCountEnumerator <- renderText({
    df <- reqData()
    req(input$filterEnumerator)
    enumerator <- input$filterEnumerator
    df_enum <- df %>% filter(EnuName == enumerator)
    totalSurveys <- nrow(df_enum)
    paste("Enumerator", enumerator, "conducted", totalSurveys, "surveys in total.")
  })

  output$plotAdm2EnumTree <- renderPlotly({
    df <- reqData()
    req(input$filterEnumerator)
    enumerator <- input$filterEnumerator
    df_enum <- df %>% filter(EnuName == enumerator)
    summary_df <- df_enum %>% group_by(ADMIN2Name) %>% summarize(count = n(), .groups = "drop")
    plot_ly(
      data = summary_df,
      type = "treemap",
      labels = ~ADMIN2Name,
      values = ~count,
      parents = NA,
      textinfo = "label+value"
    )
  })
}
