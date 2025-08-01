rcsi_module <- function(input, output, session, reqData, rcsi_colors) {
output$plotrCSIadm1 <- renderPlotly({
    df <- reqData()

    rcsi_data <- df %>%
      group_by(ADMIN1Name, rCSI_CH) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN1Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(
      rcsi_data,
      aes(
        x = ADMIN1Name,
        y = perc,
        fill = rCSI_CH,
        text = paste0(
          "ADMIN1Name: ",
          ADMIN1Name,
          "<br>",
          "rCSI_CH: ",
          rCSI_CH,
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
      scale_fill_manual(values = rCSI_colors) +
      labs(x = "Admin1", y = "Percentage", fill = "rCSI Phase") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

  ###########################
  # 2) rCSI by Admin2
  #    - Force user to pick exactly one Admin1
  ###########################
  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(
      session,
      "admin1FilterrCSI2",
      choices = admin1Vals,
      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  output$plotrCSIadm2 <- renderPlotly({
    df <- reqData()
    req(input$admin1FilterrCSI2)

    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterrCSI2)

    rcsi_data <- df_filtered %>%
      group_by(ADMIN2Name, rCSI_CH) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN2Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(
      rcsi_data,
      aes(
        x = ADMIN2Name,
        y = perc,
        fill = rCSI_CH,
        text = paste0(
          "ADMIN2Name: ",
          ADMIN2Name,
          "<br>",
          "rCSI_CH: ",
          rCSI_CH,
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
      scale_fill_manual(values = rCSI_colors) +
      labs(x = "Admin2", y = "Percentage", fill = "rCSI Phase") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

  ###########################
  # 3) rCSI by Admin1 & Enumerator
  ###########################
  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(
      session,
      "admin1FilterEnumrCSI",
      choices = admin1Vals,
      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  observeEvent(input$admin1FilterEnumrCSI, {
    df <- reqData()
    req(input$admin1FilterEnumrCSI)

    df_sub <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumrCSI)

    admin2Vals <- sort(unique(df_sub$ADMIN2Name))

    updateSelectInput(
      session,
      "admin2FilterEnumrCSI",
      choices = c("All", admin2Vals),
      selected = "All"
    )
  })

  output$plotrCSIadm1Enum <- renderPlotly({
    df <- reqData()
    req(input$admin1FilterEnumrCSI)
    req(input$admin2FilterEnumrCSI)

    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumrCSI)

    if (input$admin2FilterEnumrCSI != "All") {
      df_filtered <- df_filtered %>%
        filter(ADMIN2Name == input$admin2FilterEnumrCSI)
    }

    rcsi_data <- df_filtered %>%
      group_by(EnuName, rCSI_CH) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(EnuName) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(
      rcsi_data,
      aes(
        x = EnuName,
        y = perc,
        fill = rCSI_CH,
        text = paste0(
          "EnuName: ",
          EnuName,
          "<br>",
          "rCSI_CH: ",
          rCSI_CH,
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
      scale_fill_manual(values = rCSI_colors) +
      labs(x = "Enumerator", y = "Percentage", fill = "rCSI Phase") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

  ################################
  # 4) rCSI Outlier
  ################################
  # 1) update the Admin1 dropdown for the Admin2 subtab
  observeEvent(reqData(), {
    adm1s <- sort(unique(reqData()$ADMIN1Name))
    updateSelectInput(
      session,
      "rcsiBoxAdmin2Admin1",
      choices = adm1s,
      selected = adm1s[1] %||% NULL
    )
  })

  # 2) Boxplot: rCSI by Admin1
  output$plotRCSIBoxAdmin1 <- renderPlotly({
    df <- req(reqData())

    p <- ggplot(df, aes(x = ADMIN1Name, y = rCSI)) +
      geom_boxplot(fill = "steelblue") +
      theme_minimal() +
      labs(x = "Admin1", y = "rCSI", title = "rCSI Distribution by Admin1")

    ggplotly(p) %>%
      layout(showlegend = FALSE)
  })

  # 3) Outlier table: rCSI by Admin1
  output$tableRCSIOutliersAdmin1 <- DT::renderDT(
    {
      df <- req(reqData())

      df_out <- df %>%
        group_by(ADMIN1Name) %>%
        mutate(
          Q1 = quantile(rCSI, .25, na.rm = TRUE),
          Q3 = quantile(rCSI, .75, na.rm = TRUE),
          IQR = Q3 - Q1,
          lower = Q1 - 1.5 * IQR,
          upper = Q3 + 1.5 * IQR
        ) %>%
        ungroup() %>%
        filter(rCSI < lower | rCSI > upper) %>%
        select(
          ADMIN1Name,
          ADMIN2Name,
          ADMIN4Name,
          EnuSupervisorName,
          EnuName,
          HHSizeCalc,
          rCSI,
          rCSILessQlty,
          rCSIBorrow,
          rCSIMealSize,
          rCSIMealAdult,
          rCSIMealNb
        )

      DT::datatable(
        df_out,
        rownames = FALSE,
        extensions = 'Buttons',
        filter = 'top',
        options = list(
          dom = 'Bfrtip',
          buttons = list(
            list(
              extend = 'csv',
              text = 'Export CSV',
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              text = 'Export Excel',
              exportOptions = list(modifier = list(page = 'all'))
            )
          ),
          scrollX = TRUE,
          pageLength = 10
        )
      ) %>%
        formatRound("rCSI", 2, mark = ",")
    },
    server = FALSE
  )

  # 4) Download handler for Admin1 outliers
  output$downloadRCSIOutliersAdmin1 <- downloadHandler(
    filename = function() {
      paste0("rCSI_Outliers_Admin1_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- req(reqData()) %>%
        group_by(ADMIN1Name) %>%
        mutate(
          Q1 = quantile(rCSI, .25, na.rm = TRUE),
          Q3 = quantile(rCSI, .75, na.rm = TRUE),
          IQR = Q3 - Q1,
          lower = Q1 - 1.5 * IQR,
          upper = Q3 + 1.5 * IQR
        ) %>%
        ungroup() %>%
        filter(rCSI < lower | rCSI > upper) %>%
        select(
          ADMIN1Name,
          ADMIN2Name,
          ADMIN4Name,
          EnuSupervisorName,
          EnuName,
          HHSizeCalc,
          rCSI,
          rCSILessQlty,
          rCSIBorrow,
          rCSIMealSize,
          rCSIMealAdult,
          rCSIMealNb
        )

      writexl::write_xlsx(df, path = file)
    }
  )

  # 5) Boxplot: rCSI by Admin2 (with Admin1 filter)
  output$plotRCSIBoxAdmin2 <- renderPlotly({
    df <- req(reqData())
    req(input$rcsiBoxAdmin2Admin1)

    filtered <- df %>% filter(ADMIN1Name == input$rcsiBoxAdmin2Admin1)

    p <- ggplot(filtered, aes(x = ADMIN2Name, y = rCSI)) +
      geom_boxplot(fill = "tomato") +
      theme_minimal() +
      labs(
        x = "Admin2",
        y = "rCSI",
        title = paste("rCSI by Admin2 for Admin1 =", input$rcsiBoxAdmin2Admin1)
      )

    ggplotly(p) %>%
      layout(showlegend = FALSE)
  })

  # 6) Outlier table: rCSI by Admin2
  output$tableRCSIOutliersAdmin2 <- DT::renderDT(
    {
      df <- req(reqData())
      req(input$rcsiBoxAdmin2Admin1)

      df_out <- df %>%
        filter(ADMIN1Name == input$rcsiBoxAdmin2Admin1) %>%
        group_by(ADMIN2Name) %>%
        mutate(
          Q1 = quantile(rCSI, .25, na.rm = TRUE),
          Q3 = quantile(rCSI, .75, na.rm = TRUE),
          IQR = Q3 - Q1,
          lower = Q1 - 1.5 * IQR,
          upper = Q3 + 1.5 * IQR
        ) %>%
        ungroup() %>%
        filter(rCSI < lower | rCSI > upper) %>%
        select(
          ADMIN1Name,
          ADMIN2Name,
          ADMIN4Name,
          EnuSupervisorName,
          EnuName,
          HHSizeCalc,
          rCSI,
          rCSILessQlty,
          rCSIBorrow,
          rCSIMealSize,
          rCSIMealAdult,
          rCSIMealNb
        )

      DT::datatable(
        df_out,
        rownames = FALSE,
        extensions = 'Buttons',
        filter = 'top',
        options = list(
          dom = 'Bfrtip',
          buttons = list(
            list(
              extend = 'csv',
              text = 'Export CSV',
              exportOptions = list(modifier = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              text = 'Export Excel',
              exportOptions = list(modifier = list(page = 'all'))
            )
          ),
          scrollX = TRUE,
          pageLength = 10
        )
      ) %>%
        formatRound("rCSI", 2, mark = ",")
    },
    server = FALSE
  )

  # 7) Download handler for Admin2 outliers
  output$downloadRCSIOutliersAdmin2 <- downloadHandler(
    filename = function() {
      paste0("rCSI_Outliers_Admin2_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- req(reqData()) %>%
        filter(ADMIN1Name == input$rcsiBoxAdmin2Admin1) %>%
        group_by(ADMIN2Name) %>%
        mutate(
          Q1 = quantile(rCSI, .25, na.rm = TRUE),
          Q3 = quantile(rCSI, .75, na.rm = TRUE),
          IQR = Q3 - Q1,
          lower = Q1 - 1.5 * IQR,
          upper = Q3 + 1.5 * IQR
        ) %>%
        ungroup() %>%
        filter(rCSI < lower | rCSI > upper) %>%
        select(
          ADMIN1Name,
          ADMIN2Name,
          ADMIN4Name,
          EnuSupervisorName,
          EnuName,
          HHSizeCalc,
          rCSI,
          rCSILessQlty,
          rCSIBorrow,
          rCSIMealSize,
          rCSIMealAdult,
          rCSIMealNb
        )

      writexl::write_xlsx(df, path = file)
    }
  )

  ################################
  # 4) rCSI Triangulation with FCS
  ################################
  ### Update Admin1 for Triangulation (if not already done)
  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(
      session,
      "triangAdmin1",
      choices = admin1Vals,
      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  ### When Admin1 changes, update the Admin2 dropdown so that it lists only those in the selected Admin1.
  observeEvent(input$triangAdmin1, {
    req(reqData())
    df <- reqData()
    selected_admin1 <- input$triangAdmin1
    admin2_choices <- sort(unique(
      df %>% filter(ADMIN1Name == selected_admin1) %>% pull(ADMIN2Name)
    ))
    updateSelectInput(
      session,
      "triangAdmin2",
      choices = c("All", admin2_choices),
      selected = "All"
    )
  })

  ### (Optional) If you want to update the FCSCat selector from the dataset, you can leave it fixed to two choices as above.

  ### Render the Triangulation Plot (by Enumerator)
  output$plotTriangulation <- renderPlotly({
    # Require that all three inputs are available
    req(input$triangAdmin1, input$triangAdmin2, input$triangFCSCat)

    # Start with the main dataset filtered by the selected Admin1.
    df <- reqData() %>%
      filter(ADMIN1Name == input$triangAdmin1)

    # If the Admin2 filter is not set to "All", further filter by Admin2.
    if (input$triangAdmin2 != "All") {
      df <- df %>% filter(ADMIN2Name == input$triangAdmin2)
    }

    # Apply triangulation criteria:
    #   - Keep rows where rCSI is very high (>42) or very low (<3)
    #   - And where the selected FCS category (FCSCat21 or FCSCat28) is either "Poor" or "Borderline"
    df_tri <- df %>%
      filter(
        (rCSI > 42 | rCSI < 3) &
          (get(input$triangFCSCat) %in% c("Poor", "Borderline"))
      ) %>%
      mutate(
        Triang = if_else(
          rCSI > 42,
          "Very High (rCSI > 42)",
          "Very Low (rCSI < 3)"
        )
      )

    # Summarize the data by enumerator (EnuName) so we can create a stacked bar chart.
    df_plot <- df_tri %>%
      group_by(EnuName, Triang) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(EnuName) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    # Create the 100% stacked bar chart.
    p <- ggplot(
      df_plot,
      aes(
        x = EnuName,
        y = perc,
        fill = Triang,
        text = paste0(
          "EnuName: ",
          EnuName,
          "<br>",
          "Trian: ",
          Triang,
          "<br>",
          "n: ",
          n,
          "<br>",
          "perc: ",
          sprintf("%.2f%%", perc * 100)
        )
      )
    ) +
      geom_col(stat = "stack") +
      labs(
        title = "Triangulation of rCSI and FCS",
        x = "Enumerator",
        y = "Proportion",
        fill = "Triangulation"
      ) +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p, tooltip = "text")
  })

  ### Render the Triangulation Details Table with the requested variables and filtering ("top") inputs.
  output$tableTriangulation <- renderDataTable({
    # Require both the Admin1 and the FCSCat selector are set.
    req(input$triangAdmin1, input$triangFCSCat)
    df <- reqData()

    # Filter by selected Admin1. Also filter by Admin2 if one is selected (i.e., not "All").
    df_tri <- df %>% filter(ADMIN1Name == input$triangAdmin1)
    if (!is.null(input$triangAdmin2) && input$triangAdmin2 != "All") {
      df_tri <- df_tri %>% filter(ADMIN2Name == input$triangAdmin2)
    }

    # Apply the triangulation criteria:
    df_tri <- df_tri %>%
      filter(
        (rCSI > 42 | rCSI < 3) &
          (get(input$triangFCSCat) %in% c("Poor", "Borderline"))
      ) %>%
      mutate(
        Triang = if_else(
          rCSI > 42,
          "Very High (rCSI > 42)",
          "Very Low (rCSI < 3)"
        )
      )

    # Decide which FCSCat column to include (only the one selected)
    selectedFCSCol <- if (input$triangFCSCat == "FCSCat21") {
      "FCSCat21"
    } else {
      "FCSCat28"
    }

    # Select the detailed columns.
    df_details <- df_tri %>%
      select(
        ADMIN1Name,
        ADMIN2Name,
        ADMIN3Name,
        ADMIN4Name,
        EnuSupervisorName,
        EnuName,
        HHSizeCalc,
        FCSStap,
        FCSPulse,
        FCSDairy,
        FCSPr,
        FCSVeg,
        FCSFruit,
        FCSFat,
        FCSSugar,
        FCS,
        all_of(selectedFCSCol),
        rCSI,
        Triang,
        rCSILessQlty,
        rCSIBorrow,
        rCSIMealSize,
        rCSIMealAdult,
        rCSIMealNb
      )

    datatable(
      df_details,
      rownames = FALSE,
      filter = "top",
      options = list(scrollX = TRUE)
    )
  })

  ### Download handler for Triangulation table
  output$downloadTriangulation <- downloadHandler(
    filename = function() {
      paste0("triangulation_details_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # replicate the exact filtering & selection from renderDataTable
      req(input$triangAdmin1, input$triangFCSCat)
      df <- reqData()

      # 1) Filter by Admin1 (and Admin2 if not "All")
      df_tri <- df %>%
        filter(ADMIN1Name == input$triangAdmin1)
      if (!is.null(input$triangAdmin2) && input$triangAdmin2 != "All") {
        df_tri <- df_tri %>% filter(ADMIN2Name == input$triangAdmin2)
      }

      # 2) Apply triangulation criteria
      df_tri <- df_tri %>%
        filter(
          (rCSI > 42 | rCSI < 3) &
            (get(input$triangFCSCat) %in% c("Poor", "Borderline"))
        ) %>%
        mutate(
          Triang = if_else(
            rCSI > 42,
            "Very High (rCSI > 42)",
            "Very Low (rCSI < 3)"
          )
        )

      # 3) Pick the correct FCS column
      selectedFCSCol <- if (input$triangFCSCat == "FCSCat21") {
        "FCSCat21"
      } else {
        "FCSCat28"
      }

      # 4) Select exactly the same columns you show in the DT
      df_details <- df_tri %>%
        select(
          ADMIN1Name,
          ADMIN2Name,
          ADMIN3Name,
          ADMIN4Name,
          EnuSupervisorName,
          EnuName,
          HHSizeCalc,
          FCSStap,
          FCSPulse,
          FCSDairy,
          FCSPr,
          FCSVeg,
          FCSFruit,
          FCSFat,
          FCSSugar,
          FCS,
          all_of(selectedFCSCol),
          rCSI,
          Triang,
          rCSILessQlty,
          rCSIBorrow,
          rCSIMealSize,
          rCSIMealAdult,
          rCSIMealNb
        )

      # 5) Write to Excel
      writexl::write_xlsx(df_details, path = file)
    }
  )
}