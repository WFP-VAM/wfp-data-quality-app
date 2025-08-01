hhs_module <- function(input, output, session, reqData, HHS_colors) {

    output$plotHHSadm1 <- renderPlotly({
    df <- reqData()

    hhs_data <- df %>%
      group_by(ADMIN1Name, HHS_CH) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN1Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(
      hhs_data,
      aes(
        x = ADMIN1Name,
        y = perc,
        fill = HHS_CH,
        text = paste0(
          "ADMIN1Name: ",
          ADMIN1Name,
          "<br>",
          "HHS_CH: ",
          HHS_CH,
          "<br>",
          "n: ",
          n,
          "<br>",
          "perc: ",
          sprintf("%.2f%%", perc * 100)
        )
      )
    ) +
      geom_col(position = "stack") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      scale_fill_manual(values = HHS_colors) +
      labs(x = "Admin1", y = "Percentage", fill = "HHS Phase") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(
      session,
      "admin1FilterHHS2",
      choices = admin1Vals,
      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  output$plotHHSadm2 <- renderPlotly({
    df <- reqData()
    req(input$admin1FilterHHS2)

    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterHHS2)

    hhs_data <- df_filtered %>%
      group_by(ADMIN2Name, HHS_CH) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN2Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(
      hhs_data,
      aes(
        x = ADMIN2Name,
        y = perc,
        fill = HHS_CH,
        text = paste0(
          "ADMIN2Name: ",
          ADMIN2Name,
          "<br>",
          "HHS_CH: ",
          HHS_CH,
          "<br>",
          "n: ",
          n,
          "<br>",
          "perc: ",
          sprintf("%.2f%%", perc * 100)
        )
      )
    ) +
      geom_col(position = "stack") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      scale_fill_manual(values = HHS_colors) +
      labs(x = "Admin2", y = "Percentage", fill = "HHS Phase") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(
      session,
      "admin1FilterEnumHHS",
      choices = admin1Vals,
      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  observeEvent(input$admin1FilterEnumHHS, {
    df <- reqData()
    req(input$admin1FilterEnumHHS)

    df_sub <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumHHS)

    admin2Vals <- sort(unique(df_sub$ADMIN2Name))

    updateSelectInput(
      session,
      "admin2FilterEnumHHS",
      choices = c("All", admin2Vals),
      selected = "All"
    )
  })

  output$plotHHSadm1Enum <- renderPlotly({
    df <- reqData()
    req(input$admin1FilterEnumHHS)
    req(input$admin2FilterEnumHHS)

    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumHHS)

    if (input$admin2FilterEnumHHS != "All") {
      df_filtered <- df_filtered %>%
        filter(ADMIN2Name == input$admin2FilterEnumHHS)
    }

    hhs_data <- df_filtered %>%
      group_by(EnuName, HHS_CH) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(EnuName) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(
      hhs_data,
      aes(
        x = EnuName,
        y = perc,
        fill = HHS_CH,
        text = paste0(
          "EnuName: ",
          EnuName,
          "<br>",
          "HHS_CH: ",
          HHS_CH,
          "<br>",
          "n: ",
          n,
          "<br>",
          "perc: ",
          sprintf("%.2f%%", perc * 100)
        )
      )
    ) +
      geom_col(position = "stack") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      scale_fill_manual(values = HHS_colors) +
      labs(x = "Enumerator", y = "Percentage", fill = "HHS Phase") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })
}