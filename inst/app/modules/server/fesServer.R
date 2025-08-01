fes_module <- function(input, output, session, reqData, FES_Colors) {
    output$plotFESadm1 <- renderPlotly({
    df <- reqData()

    # Summarize
    fes_data <- df %>%
      group_by(ADMIN1Name, Foodexp_4pt) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN1Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(
      fes_data,
      aes(
        x = ADMIN1Name,
        y = perc,
        fill = Foodexp_4pt,
        text = paste0(
          "ADMIN1Name: ",
          ADMIN1Name,
          "<br>",
          "Foodexp_4pt: ",
          Foodexp_4pt,
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
      scale_fill_manual(values = FES_Colors) +
      labs(x = "Admin1", y = "Percentage", fill = "FES Category") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

  ##################################################
  # FES by Admin2
  ##################################################
  # Populate the forced Admin1 list for sub-tab "FES by Admin2"
  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(
      session,
      "admin1FilterFES2",
      choices = admin1Vals,
      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  output$plotFESadm2 <- renderPlotly({
    df <- reqData()
    req(input$admin1FilterFES2)

    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterFES2)

    fes_data <- df_filtered %>%
      group_by(ADMIN2Name, Foodexp_4pt) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN2Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(
      fes_data,
      aes(
        x = ADMIN2Name,
        y = perc,
        fill = Foodexp_4pt,
        text = paste0(
          "ADMIN2Name: ",
          ADMIN2Name,
          "<br>",
          "Foodexp_4pt: ",
          Foodexp_4pt,
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
      scale_fill_manual(values = FES_Colors) +
      labs(x = "Admin2", y = "Percentage", fill = "FES Category") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

  ##################################################
  # FES by Admin1 & Enumerator
  ##################################################
  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(
      session,
      "admin1FilterEnumFES",
      choices = admin1Vals,
      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  observeEvent(input$admin1FilterEnumFES, {
    df <- reqData()
    req(input$admin1FilterEnumFES)

    df_sub <- df %>% filter(ADMIN1Name == input$admin1FilterEnumFES)
    admin2Vals <- sort(unique(df_sub$ADMIN2Name))

    updateSelectInput(
      session,
      "admin2FilterEnumFES",
      choices = c("All", admin2Vals),
      selected = "All"
    )
  })

  output$plotFESadm1Enum <- renderPlotly({
    df <- reqData()
    req(input$admin1FilterEnumFES)
    req(input$admin2FilterEnumFES)

    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumFES)

    if (input$admin2FilterEnumFES != "All") {
      df_filtered <- df_filtered %>%
        filter(ADMIN2Name == input$admin2FilterEnumFES)
    }

    fes_data <- df_filtered %>%
      group_by(EnuName, Foodexp_4pt) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(EnuName) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(
      fes_data,
      aes(
        x = EnuName,
        y = perc,
        fill = Foodexp_4pt,
        text = paste0(
          "EnuName: ",
          EnuName,
          "<br>",
          "Foodexp_4pt: ",
          Foodexp_4pt,
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
      scale_fill_manual(values = FES_Colors) +
      labs(x = "Enumerator", y = "Percentage", fill = "FES Category") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

  ##################################################
  # Food Expenditure Outlier by Admin1
  ##################################################
  # --- Boxplot: Monthly Food Expenses by Admin1 ---
  output$plotFESBoxAdmin1 <- renderPlotly({
    df <- reqData()

    p <- ggplot(df, aes(x = ADMIN1Name, y = HHExpF_1M)) +
      geom_boxplot(fill = "steelblue") +
      scale_y_continuous(
        labels = comma, # show 1,000 10,000 etc.
        expand = expansion(mult = c(0, 0.05))
      ) +
      theme_minimal() +
      labs(
        title = "",
        x = "Admin1",
        y = "Amount of Monthly Food Expenses"
      )

    ggplotly(p) %>%
      layout(
        yaxis = list(
          tickformat = ",.0f" # D3 format: comma thousand-sep, no decimals
        )
      )
  })

  ##################################################
  # Food Expenditure Outlier Table by Admin1
  ##################################################

  output$tableFESOutliersAdmin1 <- renderDataTable({
    # Use the full dataset grouped by ADMIN1Name (no Admin1 filter here)
    df <- reqData()

    # Retrieve the recall period (7 days or 1 month) from your common input.
    recall <- as.numeric(input$fesRecall)

    # Helper function to calculate monthly expense for a given food-group prefix.
    calc_monthly_expense <- function(df, prefix, recall) {
      if (recall == 7) {
        cols <- paste0(
          prefix,
          c("_Purch_MN_7D", "_GiftAid_MN_7D", "_Own_MN_7D")
        )
        if (!all(cols %in% names(df))) {
          stop(paste(
            "Missing 7‐day recall columns for",
            prefix,
            ":",
            paste(cols[!cols %in% names(df)], collapse = ", ")
          ))
        }
        # Multiply by (30/7) to scale 7-day data to monthly.
        return(rowSums(select(df, all_of(cols)), na.rm = TRUE) * (30 / 7))
      } else {
        cols <- paste0(
          prefix,
          c("_Purch_MN_1M", "_GiftAid_MN_1M", "_Own_MN_1M")
        )
        if (!all(cols %in% names(df))) {
          stop(paste(
            "Missing 1‐month recall columns for",
            prefix,
            ":",
            paste(cols[!cols %in% names(df)], collapse = ", ")
          ))
        }
        return(rowSums(select(df, all_of(cols)), na.rm = TRUE))
      }
    }

    # Compute monthly expenses for each food group.
    df <- df %>%
      mutate(
        HHExpFCer_MN = calc_monthly_expense(., "HHExpFCer", recall),
        HHExpFTub_MN = calc_monthly_expense(., "HHExpFTub", recall),
        HHExpFPuls_MN = calc_monthly_expense(., "HHExpFPuls", recall),
        HHExpFVeg_MN = calc_monthly_expense(., "HHExpFVeg", recall),
        HHExpFFrt_MN = calc_monthly_expense(., "HHExpFFrt", recall),
        HHExpFAnimMeat_MN = calc_monthly_expense(., "HHExpFAnimMeat", recall),
        HHExpFAnimFish_MN = calc_monthly_expense(., "HHExpFAnimFish", recall),
        HHExpFFats_MN = calc_monthly_expense(., "HHExpFFats", recall),
        HHExpFDairy_MN = calc_monthly_expense(., "HHExpFDairy", recall),
        HHExpFEgg_MN = calc_monthly_expense(., "HHExpFEgg", recall),
        HHExpFSgr_MN = calc_monthly_expense(., "HHExpFSgr", recall),
        HHExpFCond_MN = calc_monthly_expense(., "HHExpFCond", recall),
        HHExpFBev_MN = calc_monthly_expense(., "HHExpFBev", recall),
        HHExpFOut_MN = calc_monthly_expense(., "HHExpFOut", recall)
      )

    # Compute IQR-based outlier thresholds on HHExpF_1M at the Admin1 level.
    df_out <- df %>%
      group_by(ADMIN1Name) %>%
      mutate(
        Q1 = quantile(HHExpF_1M, 0.25, na.rm = TRUE),
        Q3 = quantile(HHExpF_1M, 0.75, na.rm = TRUE),
        IQR = Q3 - Q1,
        lower_bound = Q1 - 1.5 * IQR,
        upper_bound = Q3 + 1.5 * IQR
      ) %>%
      ungroup() %>%
      filter(HHExpF_1M < lower_bound | HHExpF_1M > upper_bound) %>%
      select(
        ADMIN1Name,
        ADMIN2Name,
        ADMIN4Name,
        EnuSupervisorName,
        EnuName,
        HHSizeCalc,
        HHExpF_1M,
        HHExpFCer_MN,
        HHExpFTub_MN,
        HHExpFPuls_MN,
        HHExpFVeg_MN,
        HHExpFFrt_MN,
        HHExpFAnimMeat_MN,
        HHExpFAnimFish_MN,
        HHExpFFats_MN,
        HHExpFDairy_MN,
        HHExpFEgg_MN,
        HHExpFSgr_MN,
        HHExpFCond_MN,
        HHExpFBev_MN,
        HHExpFOut_MN
      )

    # If no outlier rows are found, return a simple datatable with a message.
    if (nrow(df_out) == 0) {
      return(datatable(
        data.frame(Message = "No outlier observations found."),
        rownames = FALSE,
        filter = "top",
        options = list(scrollX = TRUE)
      ))
    }

    # Create the DataTable with top filters; format numeric columns with commas.
    datatable(
      df_out,
      rownames = FALSE,
      filter = "top",
      options = list(scrollX = TRUE)
    ) %>%
      formatRound(
        columns = c(
          "HHSizeCalc",
          "HHExpF_1M",
          "HHExpFCer_MN",
          "HHExpFTub_MN",
          "HHExpFPuls_MN",
          "HHExpFVeg_MN",
          "HHExpFFrt_MN",
          "HHExpFAnimMeat_MN",
          "HHExpFAnimFish_MN",
          "HHExpFFats_MN",
          "HHExpFDairy_MN",
          "HHExpFEgg_MN",
          "HHExpFSgr_MN",
          "HHExpFCond_MN",
          "HHExpFBev_MN",
          "HHExpFOut_MN"
        ),
        digits = 0,
        mark = ","
      )
  })

  # --- Download Handler for Excel export for FES Food Expenses by Admin1 ---
  output$downloadOutlierTableAdmin1 <- downloadHandler(
    filename = function() {
      paste0("FES_Outliers_Admin1_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- reqData()
      recall <- as.numeric(input$fesRecall)

      calc_monthly_expense <- function(df, prefix, recall) {
        if (recall == 7) {
          cols <- paste0(
            prefix,
            c("_Purch_MN_7D", "_GiftAid_MN_7D", "_Own_MN_7D")
          )
          if (!all(cols %in% names(df))) {
            stop(paste(
              "Missing 7‐day recall columns for",
              prefix,
              ":",
              paste(cols[!cols %in% names(df)], collapse = ", ")
            ))
          }
          return(rowSums(select(df, all_of(cols)), na.rm = TRUE) * (30 / 7))
        } else {
          cols <- paste0(
            prefix,
            c("_Purch_MN_1M", "_GiftAid_MN_1M", "_Own_MN_1M")
          )
          if (!all(cols %in% names(df))) {
            stop(paste(
              "Missing 1‐month recall columns for",
              prefix,
              ":",
              paste(cols[!cols %in% names(df)], collapse = ", ")
            ))
          }
          return(rowSums(select(df, all_of(cols)), na.rm = TRUE))
        }
      }

      df <- df %>%
        mutate(
          HHExpFCer_MN = calc_monthly_expense(., "HHExpFCer", recall),
          HHExpFTub_MN = calc_monthly_expense(., "HHExpFTub", recall),
          HHExpFPuls_MN = calc_monthly_expense(., "HHExpFPuls", recall),
          HHExpFVeg_MN = calc_monthly_expense(., "HHExpFVeg", recall),
          HHExpFFrt_MN = calc_monthly_expense(., "HHExpFFrt", recall),
          HHExpFAnimMeat_MN = calc_monthly_expense(., "HHExpFAnimMeat", recall),
          HHExpFAnimFish_MN = calc_monthly_expense(., "HHExpFAnimFish", recall),
          HHExpFFats_MN = calc_monthly_expense(., "HHExpFFats", recall),
          HHExpFDairy_MN = calc_monthly_expense(., "HHExpFDairy", recall),
          HHExpFEgg_MN = calc_monthly_expense(., "HHExpFEgg", recall),
          HHExpFSgr_MN = calc_monthly_expense(., "HHExpFSgr", recall),
          HHExpFCond_MN = calc_monthly_expense(., "HHExpFCond", recall),
          HHExpFBev_MN = calc_monthly_expense(., "HHExpFBev", recall),
          HHExpFOut_MN = calc_monthly_expense(., "HHExpFOut", recall)
        )

      df_out <- df %>%
        group_by(ADMIN1Name) %>%
        mutate(
          Q1 = quantile(HHExpF_1M, 0.25, na.rm = TRUE),
          Q3 = quantile(HHExpF_1M, 0.75, na.rm = TRUE),
          IQR = Q3 - Q1,
          lower_bound = Q1 - 1.5 * IQR,
          upper_bound = Q3 + 1.5 * IQR
        ) %>%
        ungroup() %>%
        filter(HHExpF_1M < lower_bound | HHExpF_1M > upper_bound) %>%
        select(
          ADMIN1Name,
          ADMIN2Name,
          ADMIN4Name,
          EnuSupervisorName,
          EnuName,
          HHSizeCalc,
          HHExpF_1M,
          HHExpFCer_MN,
          HHExpFTub_MN,
          HHExpFPuls_MN,
          HHExpFVeg_MN,
          HHExpFFrt_MN,
          HHExpFAnimMeat_MN,
          HHExpFAnimFish_MN,
          HHExpFFats_MN,
          HHExpFDairy_MN,
          HHExpFEgg_MN,
          HHExpFSgr_MN,
          HHExpFCond_MN,
          HHExpFBev_MN,
          HHExpFOut_MN
        )

      numeric_cols <- c(
        "HHSizeCalc",
        "HHExpF_1M",
        "HHExpFCer_MN",
        "HHExpFTub_MN",
        "HHExpFPuls_MN",
        "HHExpFVeg_MN",
        "HHExpFFrt_MN",
        "HHExpFAnimMeat_MN",
        "HHExpFAnimFish_MN",
        "HHExpFFats_MN",
        "HHExpFDairy_MN",
        "HHExpFEgg_MN",
        "HHExpFSgr_MN",
        "HHExpFCond_MN",
        "HHExpFBev_MN",
        "HHExpFOut_MN"
      )

      df_out <- df_out %>%
        mutate(across(
          all_of(numeric_cols),
          ~ as.character(format(
            round(.x, 0),
            big.mark = ",",
            scientific = FALSE
          ))
        ))

      writexl::write_xlsx(df_out, path = file)
    }
  )

  ##################################################
  # Food Expenditure Outlier by Admin2
  ##################################################
  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(
      session,
      "fesBoxAdmin2Admin1",
      choices = admin1Vals,
      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  output$plotFESBoxAdmin2 <- renderPlotly({
    df <- reqData()
    req(input$fesBoxAdmin2Admin1)

    # Filter the data by the selected Admin1.
    filtered_df <- df %>% filter(ADMIN1Name == input$fesBoxAdmin2Admin1)

    # Create the boxplot: x-axis = Admin2Name, y-axis = HHExpF_1M.
    p <- ggplot(filtered_df, aes(x = ADMIN2Name, y = HHExpF_1M)) +
      geom_boxplot(fill = "tomato") +
      scale_y_continuous(
        labels = comma, # 1,000 2,000 etc.
        expand = expansion(mult = c(0, 0.05))
      ) +
      theme_minimal() +
      labs(
        title = paste(
          "Monthly Food Expenses by Admin2 for Admin1 =",
          input$fesBoxAdmin2Admin1
        ),
        x = "Admin2Name",
        y = "Amount of Monthly Food Expenses"
      )

    ggplotly(p) %>%
      layout(
        yaxis = list(
          tickformat = ",.0f" # D3 format: comma thousand‐sep, 0 decimal places
        )
      )
  })

  ##################################################
  # Food Expenditure Outlier Table by Admin2
  ##################################################
  output$tableFESOutliersAdmin2 <- renderDataTable({
    # Filter the dataset for the selected Admin1 (for the FES outliers sub‑tab)
    req(input$fesBoxAdmin2Admin1)
    df <- reqData() %>%
      filter(ADMIN1Name == input$fesBoxAdmin2Admin1)

    # Get the recall period chosen by the user (7 days or 1 month)
    recall <- as.numeric(input$fesRecall)

    # Helper function to calculate monthly expense for a given food group prefix.
    calc_monthly_expense <- function(df, prefix, recall) {
      if (recall == 7) {
        cols <- paste0(
          prefix,
          c("_Purch_MN_7D", "_GiftAid_MN_7D", "_Own_MN_7D")
        )
        if (!all(cols %in% names(df))) {
          stop(paste(
            "Missing 7‐day recall columns for",
            prefix,
            ":",
            paste(cols[!cols %in% names(df)], collapse = ", ")
          ))
        }
        return(rowSums(select(df, all_of(cols)), na.rm = TRUE) * (30 / 7))
      } else {
        cols <- paste0(
          prefix,
          c("_Purch_MN_1M", "_GiftAid_MN_1M", "_Own_MN_1M")
        )
        if (!all(cols %in% names(df))) {
          stop(paste(
            "Missing 1‐month recall columns for",
            prefix,
            ":",
            paste(cols[!cols %in% names(df)], collapse = ", ")
          ))
        }
        return(rowSums(select(df, all_of(cols)), na.rm = TRUE))
      }
    }

    # Compute monthly expenses for each food group based on recall.
    df <- df %>%
      mutate(
        HHExpFCer_MN = calc_monthly_expense(., "HHExpFCer", recall),
        HHExpFTub_MN = calc_monthly_expense(., "HHExpFTub", recall),
        HHExpFPuls_MN = calc_monthly_expense(., "HHExpFPuls", recall),
        HHExpFVeg_MN = calc_monthly_expense(., "HHExpFVeg", recall),
        HHExpFFrt_MN = calc_monthly_expense(., "HHExpFFrt", recall),
        HHExpFAnimMeat_MN = calc_monthly_expense(., "HHExpFAnimMeat", recall),
        HHExpFAnimFish_MN = calc_monthly_expense(., "HHExpFAnimFish", recall),
        HHExpFFats_MN = calc_monthly_expense(., "HHExpFFats", recall),
        HHExpFDairy_MN = calc_monthly_expense(., "HHExpFDairy", recall),
        HHExpFEgg_MN = calc_monthly_expense(., "HHExpFEgg", recall),
        HHExpFSgr_MN = calc_monthly_expense(., "HHExpFSgr", recall),
        HHExpFCond_MN = calc_monthly_expense(., "HHExpFCond", recall),
        HHExpFBev_MN = calc_monthly_expense(., "HHExpFBev", recall),
        HHExpFOut_MN = calc_monthly_expense(., "HHExpFOut", recall)
      )

    # Assume HHExpF_1M (overall monthly food expenses) is already computed in the dataset.

    # Compute the outlier thresholds for HHExpF_1M.
    df_out <- df %>%
      group_by(ADMIN2Name) %>%
      mutate(
        Q1 = quantile(HHExpF_1M, 0.25, na.rm = TRUE),
        Q3 = quantile(HHExpF_1M, 0.75, na.rm = TRUE),
        IQR = Q3 - Q1,
        lower_bound = Q1 - 1.5 * IQR,
        upper_bound = Q3 + 1.5 * IQR
      ) %>%
      ungroup() %>%
      filter(HHExpF_1M < lower_bound | HHExpF_1M > upper_bound) %>%
      select(
        ADMIN1Name,
        ADMIN2Name,
        ADMIN4Name,
        EnuSupervisorName,
        EnuName,
        HHSizeCalc,
        HHExpF_1M,
        HHExpFCer_MN,
        HHExpFTub_MN,
        HHExpFPuls_MN,
        HHExpFVeg_MN,
        HHExpFFrt_MN,
        HHExpFAnimMeat_MN,
        HHExpFAnimFish_MN,
        HHExpFFats_MN,
        HHExpFDairy_MN,
        HHExpFEgg_MN,
        HHExpFSgr_MN,
        HHExpFCond_MN,
        HHExpFBev_MN,
        HHExpFOut_MN
      )

    # If no outlier rows are found, show a message in the datatable.
    if (nrow(df_out) == 0) {
      return(datatable(
        data.frame(Message = "No outlier observations found."),
        rownames = FALSE,
        filter = "top",
        options = list(scrollX = TRUE)
      ))
    }

    # Build the DataTable using the default "top" text inputs for filtering.
    datatable(
      df_out,
      rownames = FALSE,
      filter = "top",
      options = list(scrollX = TRUE)
    ) %>%
      formatRound(
        columns = c(
          "HHSizeCalc",
          "HHExpF_1M",
          "HHExpFCer_MN",
          "HHExpFTub_MN",
          "HHExpFPuls_MN",
          "HHExpFVeg_MN",
          "HHExpFFrt_MN",
          "HHExpFAnimMeat_MN",
          "HHExpFAnimFish_MN",
          "HHExpFFats_MN",
          "HHExpFDairy_MN",
          "HHExpFEgg_MN",
          "HHExpFSgr_MN",
          "HHExpFCond_MN",
          "HHExpFBev_MN",
          "HHExpFOut_MN"
        ),
        digits = 0,
        mark = ","
      )
  })

  output$downloadOutlierTable <- downloadHandler(
    filename = function() {
      paste0("FES_Outliers_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # Recompute the outlier table following the same steps as above.
      req(input$fesBoxAdmin2Admin1)
      df <- reqData() %>% filter(ADMIN1Name == input$fesBoxAdmin2Admin1)
      recall <- as.numeric(input$fesRecall)

      calc_monthly_expense <- function(df, prefix, recall) {
        if (recall == 7) {
          cols <- paste0(
            prefix,
            c("_Purch_MN_7D", "_GiftAid_MN_7D", "_Own_MN_7D")
          )
          if (!all(cols %in% names(df))) {
            stop(paste(
              "Missing 7-day recall columns for",
              prefix,
              ":",
              paste(cols[!cols %in% names(df)], collapse = ", ")
            ))
          }
          return(rowSums(select(df, all_of(cols)), na.rm = TRUE) * (30 / 7))
        } else {
          cols <- paste0(
            prefix,
            c("_Purch_MN_1M", "_GiftAid_MN_1M", "_Own_MN_1M")
          )
          if (!all(cols %in% names(df))) {
            stop(paste(
              "Missing 1-month recall columns for",
              prefix,
              ":",
              paste(cols[!cols %in% names(df)], collapse = ", ")
            ))
          }
          return(rowSums(select(df, all_of(cols)), na.rm = TRUE))
        }
      }

      df <- df %>%
        mutate(
          HHExpFCer_MN = calc_monthly_expense(., "HHExpFCer", recall),
          HHExpFTub_MN = calc_monthly_expense(., "HHExpFTub", recall),
          HHExpFPuls_MN = calc_monthly_expense(., "HHExpFPuls", recall),
          HHExpFVeg_MN = calc_monthly_expense(., "HHExpFVeg", recall),
          HHExpFFrt_MN = calc_monthly_expense(., "HHExpFFrt", recall),
          HHExpFAnimMeat_MN = calc_monthly_expense(., "HHExpFAnimMeat", recall),
          HHExpFAnimFish_MN = calc_monthly_expense(., "HHExpFAnimFish", recall),
          HHExpFFats_MN = calc_monthly_expense(., "HHExpFFats", recall),
          HHExpFDairy_MN = calc_monthly_expense(., "HHExpFDairy", recall),
          HHExpFEgg_MN = calc_monthly_expense(., "HHExpFEgg", recall),
          HHExpFSgr_MN = calc_monthly_expense(., "HHExpFSgr", recall),
          HHExpFCond_MN = calc_monthly_expense(., "HHExpFCond", recall),
          HHExpFBev_MN = calc_monthly_expense(., "HHExpFBev", recall),
          HHExpFOut_MN = calc_monthly_expense(., "HHExpFOut", recall)
        )

      df_out <- df %>%
        group_by(ADMIN2Name) %>%
        mutate(
          Q1 = quantile(HHExpF_1M, 0.25, na.rm = TRUE),
          Q3 = quantile(HHExpF_1M, 0.75, na.rm = TRUE),
          IQR = Q3 - Q1,
          lower_bound = Q1 - 1.5 * IQR,
          upper_bound = Q3 + 1.5 * IQR
        ) %>%
        ungroup() %>%
        filter(HHExpF_1M < lower_bound | HHExpF_1M > upper_bound) %>%
        select(
          ADMIN1Name,
          ADMIN2Name,
          ADMIN4Name,
          EnuSupervisorName,
          EnuName,
          HHSizeCalc,
          HHExpF_1M,
          HHExpFCer_MN,
          HHExpFTub_MN,
          HHExpFPuls_MN,
          HHExpFVeg_MN,
          HHExpFFrt_MN,
          HHExpFAnimMeat_MN,
          HHExpFAnimFish_MN,
          HHExpFFats_MN,
          HHExpFDairy_MN,
          HHExpFEgg_MN,
          HHExpFSgr_MN,
          HHExpFCond_MN,
          HHExpFBev_MN,
          HHExpFOut_MN
        )

      # Specify the names of the numeric columns that you want formatted.
      numeric_cols <- c(
        "HHSizeCalc",
        "HHExpF_1M",
        "HHExpFCer_MN",
        "HHExpFTub_MN",
        "HHExpFPuls_MN",
        "HHExpFVeg_MN",
        "HHExpFFrt_MN",
        "HHExpFAnimMeat_MN",
        "HHExpFAnimFish_MN",
        "HHExpFFats_MN",
        "HHExpFDairy_MN",
        "HHExpFEgg_MN",
        "HHExpFSgr_MN",
        "HHExpFCond_MN",
        "HHExpFBev_MN",
        "HHExpFOut_MN"
      )

      # Format each numeric column as a whole number with a comma.
      df_out <- df_out %>%
        mutate(across(
          all_of(numeric_cols),
          ~ as.character(format(
            round(.x, 0),
            big.mark = ",",
            scientific = FALSE
          ))
        ))

      # Write the resulting table to Excel.
      writexl::write_xlsx(df_out, path = file)
    }
  )

  #Zero monthly Food Expense

  # helper to compute monthly expense from a prefix + recall
  calc_monthly_expense <- function(df, prefix, recall) {
    if (recall == 7) {
      cols <- paste0(prefix, c("_Purch_MN_7D", "_GiftAid_MN_7D", "_Own_MN_7D"))
      stopifnot(all(cols %in% names(df)))
      rowSums(select(df, all_of(cols)), na.rm = TRUE) * (30 / 7)
    } else {
      cols <- paste0(prefix, c("_Purch_MN_1M", "_GiftAid_MN_1M", "_Own_MN_1M"))
      stopifnot(all(cols %in% names(df)))
      rowSums(select(df, all_of(cols)), na.rm = TRUE)
    }
  }

  output$zeroExpTable <- DT::renderDataTable(
    {
      df <- req(reqData())
      recall <- as.numeric(req(input$fesRecall))

      # 1) compute all the _MN columns exactly as in your outlier code
      df2 <- df %>%
        mutate(
          HHExpFCer_MN = calc_monthly_expense(., "HHExpFCer", recall),
          HHExpFTub_MN = calc_monthly_expense(., "HHExpFTub", recall),
          HHExpFPuls_MN = calc_monthly_expense(., "HHExpFPuls", recall),
          HHExpFVeg_MN = calc_monthly_expense(., "HHExpFVeg", recall),
          HHExpFFrt_MN = calc_monthly_expense(., "HHExpFFrt", recall),
          HHExpFAnimMeat_MN = calc_monthly_expense(., "HHExpFAnimMeat", recall),
          HHExpFAnimFish_MN = calc_monthly_expense(., "HHExpFAnimFish", recall),
          HHExpFFats_MN = calc_monthly_expense(., "HHExpFFats", recall),
          HHExpFDairy_MN = calc_monthly_expense(., "HHExpFDairy", recall),
          HHExpFEgg_MN = calc_monthly_expense(., "HHExpFEgg", recall),
          HHExpFSgr_MN = calc_monthly_expense(., "HHExpFSgr", recall),
          HHExpFCond_MN = calc_monthly_expense(., "HHExpFCond", recall),
          HHExpFBev_MN = calc_monthly_expense(., "HHExpFBev", recall),
          HHExpFOut_MN = calc_monthly_expense(., "HHExpFOut", recall)
        )

      # 2) filter to zeros
      df0 <- df2 %>%
        filter(HHExpF_1M == 0) %>%
        select(
          ADMIN1Name,
          ADMIN2Name,
          ADMIN4Name,
          EnuSupervisorName,
          EnuName,
          HHSizeCalc,
          FCSStap,
          FCSPulse,
          FCSPr,
          FCSVeg,
          FCSFruit,
          FCSDairy,
          FCSFat,
          FCSSugar,
          HHExpF_1M,
          HHExpFCer_MN,
          HHExpFTub_MN,
          HHExpFPuls_MN,
          HHExpFVeg_MN,
          HHExpFFrt_MN,
          HHExpFAnimMeat_MN,
          HHExpFAnimFish_MN,
          HHExpFFats_MN,
          HHExpFDairy_MN,
          HHExpFEgg_MN,
          HHExpFSgr_MN,
          HHExpFCond_MN,
          HHExpFBev_MN,
          HHExpFOut_MN
        )

      # 3) render with Buttons + horizontal scroll
      DT::datatable(
        df0,
        rownames = FALSE,
        extensions = 'Buttons',
        filter = 'top',
        options = list(
          dom = 'Bfrtip',
          buttons = list(
            list(
              extend = 'csv',
              text = 'Export CSV',
              exportOptions = list(
                columns = ':visible',
                modifier = list(page = 'all')
              )
            ),
            list(
              extend = 'excel',
              text = 'Export Excel',
              exportOptions = list(
                columns = ':visible',
                modifier = list(page = 'all')
              )
            )
          ),
          scrollX = TRUE,
          pageLength = 10
        )
      ) %>%
        DT::formatRound(
          columns = c(
            "HHSizeCalc",
            "HHExpF_1M",
            "HHExpFCer_MN",
            "HHExpFTub_MN",
            "HHExpFPuls_MN",
            "HHExpFVeg_MN",
            "HHExpFFrt_MN",
            "HHExpFAnimMeat_MN",
            "HHExpFAnimFish_MN",
            "HHExpFFats_MN",
            "HHExpFDairy_MN",
            "HHExpFEgg_MN",
            "HHExpFSgr_MN",
            "HHExpFCond_MN",
            "HHExpFBev_MN",
            "HHExpFOut_MN"
          ),
          digits = 0,
          mark = ","
        )
    },
    server = FALSE
  )
}