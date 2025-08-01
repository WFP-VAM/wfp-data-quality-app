hdds_module <- function(input, output, session, reqData, HDDS_colors) {

  ###########################
  # 1) HDDS by Admin1
  ###########################
  output$plotHDDSadm1 <- renderPlotly({
    df <- reqData()

    # Group by ADMIN1Name, and the HDDS_CH category
    hdds_data <- df %>%
      group_by(ADMIN1Name, HDDS_CH) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN1Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    # Example 100% stacked bar across all Admin1
    p <- ggplot(
      hdds_data,
      aes(
        x = ADMIN1Name,
        y = perc,
        fill = HDDS_CH,
        text = paste0(
          "ADMIN1Name: ",
          ADMIN1Name,
          "<br>",
          "HDDS_CH: ",
          HDDS_CH,
          "<br>",
          "n: ",
          n,
          "<br>",
          "perc: ",
          sprintf("%.2f%%", perc * 100)
        )
      )
    ) +
      geom_bar(position = "fill", stat = "identity") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      # Replace with your color palette for HDDS
      scale_fill_manual(values = HDDS_colors) +
      labs(x = "Admin1", y = "Percentage", fill = "HDDS Phase") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

  ###########################
  # 2) HDDS by Admin2
  #    - Force user to pick exactly one Admin1
  ###########################
  # A) Populate the Admin1 filter for "HDDS by Admin2" when data is loaded
  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(
      session,
      "admin1FilterHDDS2",
      choices = admin1Vals,
      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  # B) Render the chart
  output$plotHDDSadm2 <- renderPlotly({
    df <- reqData()
    req(input$admin1FilterHDDS2) # forced single Admin1

    # Filter to chosen Admin1
    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterHDDS2)

    # Summarize by Admin2 & HDDS_CH
    hdds_data <- df_filtered %>%
      group_by(ADMIN2Name, HDDS_CH) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN2Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(
      hdds_data,
      aes(
        x = ADMIN2Name,
        y = perc,
        fill = HDDS_CH,
        text = paste0(
          "ADMIN2Name: ",
          ADMIN2Name,
          "<br>",
          "HDDS_CH: ",
          HDDS_CH,
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
      scale_fill_manual(values = HDDS_colors) +
      labs(x = "Admin2", y = "Percentage", fill = "HDDS Phase") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

  ###########################
  # 3) HDDS by Admin1 & Enumerator
  #    - Force an Admin1, optional "All" for Admin2
  ###########################
  # A) Populate Admin1 filter
  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(
      session,
      "admin1FilterEnumHDDS",
      choices = admin1Vals,
      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  # B) Populate Admin2 filter whenever user picks an Admin1
  observeEvent(input$admin1FilterEnumHDDS, {
    df <- reqData()
    req(input$admin1FilterEnumHDDS)

    # Subset to chosen Admin1
    df_sub <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumHDDS)

    admin2Vals <- sort(unique(df_sub$ADMIN2Name))

    updateSelectInput(
      session,
      "admin2FilterEnumHDDS",
      choices = c("All", admin2Vals),
      selected = "All"
    )
  })

  # C) Render chart
  output$plotHDDSadm1Enum <- renderPlotly({
    df <- reqData()
    req(input$admin1FilterEnumHDDS)
    req(input$admin2FilterEnumHDDS)

    # 1) Filter by chosen Admin1
    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumHDDS)

    # 2) If user picks a specific Admin2, filter further
    if (input$admin2FilterEnumHDDS != "All") {
      df_filtered <- df_filtered %>%
        filter(ADMIN2Name == input$admin2FilterEnumHDDS)
    }

    # Summarize by enumerator
    hdds_data <- df_filtered %>%
      group_by(EnuName, HDDS_CH) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(EnuName) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(
      hdds_data,
      aes(
        x = EnuName,
        y = perc,
        fill = HDDS_CH,
        text = paste0(
          "EnuName: ",
          EnuName,
          "<br>",
          "HDDS_CH: ",
          HDDS_CH,
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
      scale_fill_manual(values = HDDS_colors) +
      labs(x = "Enumerator", y = "Percentage", fill = "HDDS Phase") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

}