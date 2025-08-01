fcs_module <- function(input, output, session, reqData, fcg_colors) {
  # A) FCS by Admin1
  #    - Single threshold filter: "FCSCat21" or "FCSCat28"
  output$plotFCSadm1 <- renderPlotly({
    df <- reqData()
    req(input$fcsThresholdAdm1) # e.g. "FCSCat21" or "FCSCat28"

    # We'll pick the column dynamically using .data[[fcsVar]]
    fcsVar <- input$fcsThresholdAdm1 # string: "FCSCat21" or "FCSCat28"

    # Summarize data by ADMIN1Name and the chosen FCS category
    fcs_data <- df %>%
      group_by(ADMIN1Name, .data[[fcsVar]]) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN1Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    # Build a 100% stacked bar chart (or stacked by count if desired)
    p <- ggplot(
      fcs_data,
      aes(
        x = ADMIN1Name,
        y = perc,
        fill = .data[[fcsVar]],
        text = paste0(
          "ADMIN1Name: ",
          ADMIN1Name,
          "<br>",
          fcsVar,
          ": ",
          .data[[fcsVar]],
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
      scale_fill_manual(values = fcg_colors) + # same color scale you used before
      labs(x = "Admin1", y = "Percentage", fill = fcsVar) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

  ################################################################################
  # SERVER LOGIC FOR FCS FILTERS
  ################################################################################

  # 2) Observe dataset to populate Admin1 filters
  observeEvent(reqData(), {
    df <- reqData()

    # Gather unique Admin1 from your dataset
    admin1Vals <- sort(unique(df$ADMIN1Name))

    # FCS by Admin2 sub-tab => forced Admin1
    updateSelectInput(
      session,
      "admin1FilterFCS2",
      choices = admin1Vals,
      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  # 3) FCS by Admin2 (Chart logic)
  output$plotFCSadm2 <- renderPlotly({
    df <- reqData()
    req(input$admin1FilterFCS2) # forced single Admin1
    req(input$fcsThresholdAdm2) # e.g. "FCSCat21" or "FCSCat28"

    # Filter data by the chosen Admin1
    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterFCS2)

    # Decide which column to use
    fcsVar <- input$fcsThresholdAdm2

    # Summarize data
    fcs_data <- df_filtered %>%
      group_by(ADMIN2Name, .data[[fcsVar]]) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN2Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(
      fcs_data,
      aes(
        x = ADMIN2Name,
        y = perc,
        fill = .data[[fcsVar]],
        text = paste0(
          "ADMIN2Name: ",
          ADMIN2Name,
          "<br>",
          fcsVar,
          ": ",
          .data[[fcsVar]],
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
      scale_fill_manual(values = fcg_colors) +
      labs(x = "Admin2", y = "Percentage", fill = fcsVar) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

  # (A) Populate Admin1 list when data is loaded
  observeEvent(reqData(), {
    df <- reqData()

    # Gather sorted unique Admin1
    admin1Vals <- sort(unique(df$ADMIN1Name)) # now strings, e.g. "Region1"

    # Update the Admin1 dropdown
    updateSelectInput(
      session,
      "admin1FilterEnumFCS",
      choices = admin1Vals,
      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  # (B) Populate Admin2 dropdown whenever user picks an Admin1
  observeEvent(input$admin1FilterEnumFCS, {
    df <- reqData()
    req(input$admin1FilterEnumFCS)

    # Subset to chosen Admin1
    df_sub <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumFCS)

    # Gather unique textual Admin2
    admin2Vals <- sort(unique(df_sub$ADMIN2Name))

    # Provide "All" plus the actual Admin2
    updateSelectInput(
      session,
      "admin2FilterEnumFCS",
      choices = c("All", admin2Vals),
      selected = "All"
    )
  })

  # (C) Produce the chart
  output$plotFCSadm1Enum <- renderPlotly({
    df <- reqData()
    req(input$admin1FilterEnumFCS) # forced Admin1
    req(input$admin2FilterEnumFCS) # "All" or text admin2
    req(input$fcsThresholdAdm1Enum) # "FCSCat21" or "FCSCat28"

    # 1) Filter by chosen Admin1
    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumFCS)

    # 2) Filter by Admin2 if user picked something other than "All"
    if (input$admin2FilterEnumFCS != "All") {
      df_filtered <- df_filtered %>%
        filter(ADMIN2Name == input$admin2FilterEnumFCS)
    }

    # 3) Use the selected threshold column
    fcsVar <- input$fcsThresholdAdm1Enum # "FCSCat21" or "FCSCat28"

    # Summarize data
    fcs_data <- df_filtered %>%
      group_by(EnuName, .data[[fcsVar]]) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(EnuName) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    # Example stacked bar chart
    p <- ggplot(
      fcs_data,
      aes(
        x = EnuName,
        y = perc,
        fill = .data[[fcsVar]],
        text = paste0(
          "EnuName: ",
          EnuName,
          "<br>",
          fcsVar,
          ": ",
          .data[[fcsVar]],
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
      scale_fill_manual(values = fcg_colors) + # your color scale
      labs(x = "Enumerator", y = "Percentage", fill = fcsVar) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

  output$plotFCSEnumNameBox <- renderPlotly({
    df <- reqData()
    FCSEnumNametable <- df %>%
      group_by(EnuName) %>%
      summarise(FCS_median = median(FCS, na.rm = TRUE)) %>%
      mutate(FCS_outlier = is_outlier(FCS_median))

    p <- ggplot(FCSEnumNametable, aes(x = "", y = FCS_median)) +
      geom_boxplot() +
      geom_point(aes(color = FCS_outlier))
    ggplotly(p)
  })

  output$tableFCSOutlier <- renderTable({
    df <- reqData()
    FCSEnumNametable <- df %>%
      group_by(EnuName) %>%
      summarise(FCS_median = median(FCS, na.rm = TRUE)) %>%
      mutate(FCS_outlier = is_outlier(FCS_median)) %>%
      filter(FCS_outlier == TRUE) %>%
      arrange(desc(FCS_median))

    FCSEnumNametable
  })

  output$tableCereal <- renderDataTable({
    df <- reqData()
    cereal <- df %>%
      filter(FCSStap <= 4) %>%
      select(
        ADMIN1Name,
        ADMIN2Name,
        ADMIN4Name,
        EnuSupervisorName,
        EnuName,
        FCSStap,
        FCSVeg,
        Survey_date
      )
    datatable(
      cereal,
      rownames = FALSE,
      filter = "top",
      options = list(
        scrollX = TRUE, # horizontal scroll
        pageLength = 10 # optional: show 10 rows per page
      )
    )
  })

  output$downloadCereal <- downloadHandler(
    filename = function() {
      paste0("Low_Cereal_Consumption_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- reqData() %>%
        filter(FCSStap <= 4) %>%
        select(
          ADMIN1Name,
          ADMIN2Name,
          ADMIN4Name,
          EnuSupervisorName,
          EnuName,
          FCSStap,
          FCSVeg,
          Survey_date
        )
      writexl::write_xlsx(df, path = file)
    }
  )

  output$tableCerealVegDairy <- renderDataTable({
    df <- reqData()
    tabcerealelegumelait <- df %>%
      filter(FCSStap <= 4) %>%
      select(
        ADMIN1Name,
        ADMIN2Name,
        ADMIN4Name,
        EnuSupervisorName,
        EnuName,
        FCSStap,
        FCSVeg,
        FCSDairy,
        Survey_date
      )
    datatable(
      tabcerealelegumelait,
      rownames = FALSE,
      filter = "top",
      options = list(
        scrollX = TRUE, # horizontal scroll
        pageLength = 10 # optional: show 10 rows per page
      )
    )
  })

  # 2) Cereal, Vegetable, and Dairy
  output$downloadCerealVegDairy <- downloadHandler(
    filename = function() {
      paste0("Cereal_Veg_Dairy_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- reqData() %>%
        filter(FCSStap <= 4) %>%
        select(
          ADMIN1Name,
          ADMIN2Name,
          ADMIN4Name,
          EnuSupervisorName,
          EnuName,
          FCSStap,
          FCSVeg,
          FCSDairy,
          Survey_date
        )
      writexl::write_xlsx(df, path = file)
    }
  )

  output$tableVeg <- renderDataTable({
    df <- reqData()
    legumefeuille <- df %>%
      filter(FCSVeg <= 3) %>%
      select(
        ADMIN1Name,
        ADMIN2Name,
        ADMIN4Name,
        EnuSupervisorName,
        EnuName,
        FCSVeg,
        Survey_date
      )
    datatable(legumefeuille, rownames = FALSE, filter = "top")
  })

  # 3) Low Vegetable Consumption
  output$downloadVeg <- downloadHandler(
    filename = function() {
      paste0("Low_Vegetable_Consumption_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- reqData() %>%
        filter(FCSVeg <= 3) %>%
        select(
          ADMIN1Name,
          ADMIN2Name,
          ADMIN4Name,
          EnuSupervisorName,
          EnuName,
          FCSVeg,
          Survey_date
        )
      writexl::write_xlsx(df, path = file)
    }
  )

  # 4) Meat, Oil, and Sugar Consumption
  # Render the datatable for Meat, Oil, and Sugar Consumption
  output$tableMeatOilSugar <- renderDataTable({
    df <- reqData()

    # Select columns related to meat, oil, and sugar.
    # Here, we assume that:
    # - Meat consumption is captured in 'FCSPr'
    # - Oil consumption is in 'FCSFat'
    # - Sugar consumption is in 'FCSSugar'
    meat_oil_sugar <- df %>%
      select(
        ADMIN1Name,
        ADMIN2Name,
        ADMIN4Name,
        EnuSupervisorName,
        EnuName,
        FCSPr,
        FCSFat,
        FCSSugar,
        Survey_date
      )

    datatable(
      meat_oil_sugar,
      rownames = FALSE,
      filter = "top",
      options = list(
        scrollX = TRUE, # horizontal scroll
        pageLength = 10 # optional: show 10 rows per page
      )
    )
  })

  # Download handler to export the Meat, Oil, and Sugar Consumption table as an Excel file
  output$downloadMeatOilSugar <- downloadHandler(
    filename = function() {
      paste0("Meat_Oil_Sugar_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- reqData() %>%
        select(
          ADMIN1Name,
          ADMIN2Name,
          ADMIN4Name,
          EnuSupervisorName,
          EnuName,
          FCSPr,
          FCSFat,
          FCSSugar,
          Survey_date
        )
      writexl::write_xlsx(df, path = file)
    }
  )
}
