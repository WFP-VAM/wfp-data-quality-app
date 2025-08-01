# Import server files
my_path <- c("modules/server/") # set your path
source_files <- list.files(my_path, "*.R$")  # locate all .R files
map(paste0(my_path, source_files), source) 


############################
## SERVER
############################
server <- function(input, output, session) {
  ##############################yy#############################################
  # 1) READ & PREPARE DATA
  ###########################################################################
  # Get the reactives from the imported functions
  spss_import_data <- spss_import(input, output, session)
  moda_import_data <- moda_import(input, output, session)

  # Extract the reactive functions from the returned lists
  spss_data <- spss_import_data$spss_data
  moda_data <- moda_import_data$moda_data

  # -- unified reactive that picks whichever source
  prepared_data <- data_preparation(input, spss_data, moda_data)

  # -- downstream: same processing for both
  processed_data <- process_data(input, prepared_data)

  ###########################################################################
  # 5) Provide main data (reqData) for older sub-tabs (Survey, FCS, etc.)
  ###########################################################################
  reqData <- reactive({
      req(processed_data())
      processed_data()
    })

  data_preview_module(input, output, reqData)

  summary_boxes_module(output, reqData)

  summary_boxes_module(output, reqData)

  ###########################################################################
  # 2) DYNAMIC LCS: EXACT 4 stress, EXACT 3 crisis, EXACT 3 emergency
  ###########################################################################
  dynamicLCS <- get_lcs_variables(input, processed_data)

  ###########################################################################
  # 3) DYNAMIC CARI => merges dynamic LCS + user-chosen FCS threshold
  ###########################################################################
  dynamicCARI <- calculate_cari(input, dynamicLCS)

  ###########################################################################
  # 4) LCS Plot => dynamic
  ###########################################################################
  plot_dynamic_lcs(input, output, dynamicLCS, LHCS_colors)

  ###########################################################################
  # 2) SURVEY PROGRESS
  ###########################################################################
  survey_exploration_module(input, output, session, reqData)

  #########################
  # 3) FCS
  #########################
  fcs_module(input, output, session, reqData, fcg_colors)

  #########################
  # 4) HDDS
  #########################
  hdds_module(input, output, session, reqData, HDDS_colors)

  #########################
  # 5) rCSI
  #########################
  ###########################
  # 1) rCSI by Admin1
  ###########################
  rcsi_module(input, output, session, reqData, RCSI_colors)

  #########################
  # 6) HHS
  #########################
  # HHS by Admin1

  hhs_module(input, output, session, reqData, HHS_colors)

  #########################
  # 7) MATRIX
  #########################

  fewsnet_matrix_module(input, output, session, reqData)

  #########################
  # 8) LCS
  #########################

  output$plotLHCSadm1 <- renderPlotly({
    df <- dynamicLCS()

    lcs_data <- df %>%
      group_by(ADMIN1Name, LhCSICat) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN1Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(
      lcs_data,
      aes(
        x = ADMIN1Name,
        y = perc,
        fill = LhCSICat,
        text = paste0(
          "ADMIN1Name: ",
          ADMIN1Name,
          "<br>",
          "LCS_Cat: ",
          LhCSICat,
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
      scale_fill_manual(values = LHCS_colors) +
      labs(x = "Admin1", y = "Percentage", fill = "LCS Category") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(
      session,
      "admin1FilterLCS2",
      choices = admin1Vals,
      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  output$plotLHCSadm2 <- renderPlotly({
    df <- dynamicLCS()
    req(input$admin1FilterLCS2)

    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterLCS2)

    lcs_data <- df_filtered %>%
      group_by(ADMIN2Name, LhCSICat) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN2Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(
      lcs_data,
      aes(
        x = ADMIN2Name,
        y = perc,
        fill = LhCSICat,
        text = paste0(
          "ADMIN2Name: ",
          ADMIN2Name,
          "<br>",
          "LCS_Cat: ",
          LhCSICat,
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
      scale_fill_manual(values = LHCS_colors) +
      labs(x = "Admin2", y = "Percentage", fill = "LCS Category") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(
      session,
      "admin1FilterEnumLCS",
      choices = admin1Vals,
      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  observeEvent(input$admin1FilterEnumLCS, {
    df <- reqData()
    req(input$admin1FilterEnumLCS)

    df_sub <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumLCS)

    admin2Vals <- sort(unique(df_sub$ADMIN2Name))

    updateSelectInput(
      session,
      "admin2FilterEnumLCS",
      choices = c("All", admin2Vals),
      selected = "All"
    )
  })

  output$plotLHCSadm1Enum <- renderPlotly({
    df <- dynamicLCS()
    req(input$admin1FilterEnumLCS)
    req(input$admin2FilterEnumLCS)

    df_filtered <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumLCS)

    if (input$admin2FilterEnumLCS != "All") {
      df_filtered <- df_filtered %>%
        filter(ADMIN2Name == input$admin2FilterEnumLCS)
    }

    lcs_data <- df_filtered %>%
      group_by(EnuName, LhCSICat) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(EnuName) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(
      lcs_data,
      aes(
        x = EnuName,
        y = perc,
        fill = LhCSICat,
        text = paste0(
          "EnuName: ",
          EnuName,
          "<br>",
          "LCS_Cat: ",
          LhCSICat,
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
      scale_fill_manual(values = LHCS_colors) +
      labs(x = "Enumerator", y = "Percentage", fill = "LCS Category") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

  # Update Admin1 for LCS by Strategy
  observeEvent(reqData(), {
    df <- reqData()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(
      session,
      "lcsStrategyAdmin1",
      choices = admin1Vals,
      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  # When Admin1 is selected, update the Admin2 and Enumerator dropdowns
  observeEvent(input$lcsStrategyAdmin1, {
    req(reqData())
    df <- reqData()
    selected_admin1 <- input$lcsStrategyAdmin1

    # Update Admin2 choices: only those in the selected Admin1.
    admin2_choices <- sort(unique(
      df %>% filter(ADMIN1Name == selected_admin1) %>% pull(ADMIN2Name)
    ))
    updateSelectInput(
      session,
      "lcsStrategyAdmin2",
      choices = c("All", admin2_choices),
      selected = "All"
    )

    # Update Enumerator choices: those within the selected Admin1.
    enu_choices <- sort(unique(
      df %>% filter(ADMIN1Name == selected_admin1) %>% pull(EnuName)
    ))
    updateSelectInput(
      session,
      "lcsStrategyEnu",
      choices = c("All", enu_choices),
      selected = "All"
    )
  })

  # When Admin2 is selected, further restrict the Enumerator dropdown if not "All"
  observeEvent(input$lcsStrategyAdmin2, {
    req(reqData(), input$lcsStrategyAdmin1)
    df <- reqData()
    selected_admin1 <- input$lcsStrategyAdmin1
    if (input$lcsStrategyAdmin2 != "All") {
      enu_choices <- sort(unique(
        df %>%
          filter(
            ADMIN1Name == selected_admin1,
            ADMIN2Name == input$lcsStrategyAdmin2
          ) %>%
          pull(EnuName)
      ))
    } else {
      enu_choices <- sort(unique(
        df %>% filter(ADMIN1Name == selected_admin1) %>% pull(EnuName)
      ))
    }
    updateSelectInput(
      session,
      "lcsStrategyEnu",
      choices = c("All", enu_choices),
      selected = "All"
    )
  })

  output$plotLCSStrategy <- renderPlotly({
    # Start with the main dataset
    df <- reqData()

    # Apply cascading filters based on Admin1, Admin2 and Enumerator
    if (!is.null(input$lcsStrategyAdmin1) && input$lcsStrategyAdmin1 != "") {
      df <- df %>% filter(ADMIN1Name == input$lcsStrategyAdmin1)
    }
    if (!is.null(input$lcsStrategyAdmin2) && input$lcsStrategyAdmin2 != "All") {
      df <- df %>% filter(ADMIN2Name == input$lcsStrategyAdmin2)
    }
    if (!is.null(input$lcsStrategyEnu) && input$lcsStrategyEnu != "All") {
      df <- df %>% filter(EnuName == input$lcsStrategyEnu)
    }

    # Get the selected strategy variable names from the dynamic LCS input
    selected_vars <- c(input$stressVars, input$crisisVars, input$emergencyVars)

    # Validate exactly 10 strategies are selected (4 stress, 3 crisis, 3 emergency)
    shiny::validate(shiny::need(
      length(selected_vars) == 10,
      "Please select exactly 10 LCS strategies (4 stress, 3 crisis, 3 emergency) in the Dynamic LCS tab."
    ))

    # Predefined orders for each group (update these vectors if your variable names differ)
    stress_order <- c(
      "Lcs_stress_Saving",
      "Lcs_stress_DomAsset",
      "Lcs_stress_ConsActive",
      "Lcs_stress_SellFoodRation",
      "Lcs_stress_SellNFIRation",
      "Lcs_stress_EatOut",
      "Lcs_stress_BorrowCash",
      "Lcs_stress_Pawn",
      "Lcs_stress_LessSchool",
      "Lcs_stress_Utilities",
      "Lcs_stress_Edu",
      "Lcs_stress_BorrowFood",
      "Lcs_stress_MoreLabour",
      "Lcs_stress_HHSeparation",
      "Lcs_stress_Housing",
      "Lcs_stress_LessSchool",
      "LcsR_stress_Animals",
      "LcsR_stress_BorrowCash",
      "LcsR_stress_Pawn",
      "LcsR_stress_DomAsset",
      "LcsR_stress_EatOut",
      "LcsR_stress_LessSchool",
      "LcsR_stress_Saving",
      "LcsR_stress_HHSeparation",
      "LcsR_stress_ConsActive",
      "LcsR_stress_SellFoodRation",
      "LcsR_stress_DomMigration",
      "LcsR_stress_Housing",
      "LcsR_stress_SellNFIRation",
      "LcsR_stress_Edu"
    )
    crisis_order <- c(
      "Lcs_crisis_ProdAssets",
      "Lcs_crisis_Barter",
      "Lcs_crisis_Health",
      "Lcs_crisis_Housing",
      "Lcs_crisis_HHSeparation",
      "Lcs_crisis_OutSchool",
      "Lcs_crisis_Migration",
      "Lcs_crisis_DomMigration",
      "Lcs_crisis_ChildWork",
      "Lcs_crisis_Edu_Health",
      "Lcs_crisis_Barter",
      "Lcs_crisis_ConsActive",
      "Lcs_crisis_Edu",
      "Lcs_crisis_Health",
      "Lcs_crisis_Marriage",
      "Lcs_crisis_Utilities",
      "LcsR_crisis_AgriCare",
      "LcsR_crisis_ImmCrops",
      "LcsR_crisis_Seed",
      "LcsR_crisis_Animals",
      "LcsR_crisis_Health",
      "LcsR_crisis_Edu",
      "LcsR_crisis_ProdAssets",
      "LcsR_crisis_Housing",
      "LcsR_crisis_HHSeparation",
      "LcsR_crisis_Barter",
      "LcsR_crisis_Migration",
      "LcsR_crisis_ChildWork",
      "LcsR_crisis_Marriage",
      "LcsR_crisis_ConsActive",
      "LcsR_crisis_OutSchool",
      "LcsR_crisis_DomMigration"
    )
    emergency_order <- c(
      "Lcs_em_ChildMigration",
      "Lcs_em_IllegalAct",
      "Lcs_em_Begged",
      "Lcs_em_Marriage",
      "Lcs_em_ResAsset",
      "Lcs_em_Migration",
      "Lcs_em_ChildWork",
      "Lcs_em_OutSchool",
      "LcsR_em_FemAnimal",
      "LcsR_em_WildFood",
      "LcsR_em_Seed",
      "LcsR_em_OutSchool",
      "LcsR_em_Migration",
      "LcsR_em_ChildWork",
      "LcsR_em_Marriage",
      "LcsR_em_ResAsset",
      "LcsR_em_Begged",
      "LcsR_em_IllegalAct",
      "LcsR_em_ChildMigration"
    )

    # Order the selected variables by group: stress, crisis, emergency.
    selected_stress <- intersect(stress_order, selected_vars)
    selected_crisis <- intersect(crisis_order, selected_vars)
    selected_emergency <- intersect(emergency_order, selected_vars)
    desired_order <- c(selected_stress, selected_crisis, selected_emergency)

    # Pivot data from wide to long format so that each row is one strategy response per observation.
    df_long <- df %>%
      pivot_longer(
        cols = all_of(selected_vars),
        names_to = "Strategy",
        values_to = "Response"
      )

    # Create grouping variable "Type" and force an order: Stress, Crisis, Emergency.
    df_long <- df_long %>%
      mutate(
        Type = case_when(
          Strategy %in% stress_order ~ "Stress",
          Strategy %in% crisis_order ~ "Crisis",
          Strategy %in% emergency_order ~ "Emergency",
          TRUE ~ "Other"
        )
      ) %>%
      mutate(
        Type = factor(
          Type,
          levels = c("Stress", "Crisis", "Emergency", "Other")
        )
      )

    # Reorder the Strategy factor using the desired order.
    df_long$Strategy <- factor(df_long$Strategy, levels = desired_order)

    # Convert the Response values to a factor with appropriate labels.
    df_long <- df_long %>%
      mutate(
        Response = factor(
          as.numeric(Response),
          levels = c(10, 20, 30, 9999),
          labels = c(
            "No (10)",
            "No, already used (20)",
            "Yes (30)",
            "Not Applicable (NA-9999)"
          )
        )
      )

    # Build the 100% stacked bar chart.
    # facet_grid() will create separate panels for each type with free spacing on the x-axis.
    p <- ggplot(df_long, aes(x = Strategy, fill = Response)) +
      geom_bar(position = "fill") +
      facet_grid(. ~ Type, scales = "free_x", space = "free_x") +
      labs(x = "LCS Strategy", y = "Percentage", fill = "Response Option") +
      theme_minimal() +
      theme(
        strip.background = element_rect(fill = "grey90", color = "grey50"),
        strip.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_y_continuous(labels = scales::percent)

    ggplotly(p)
  })

  #########################
  # 9) FES
  #########################

  ##################################################
  # FES by Admin1
  ##################################################
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

  #########################
  # 10) CARI
  #########################

  ##################################################
  # CARI CONSOLE TABLE
  ##################################################
  # 1) Compute all of the percentages once, inside a reactive block

  pct <- reactive({
    df_raw <- req(dynamicCARI())

    # 1) Recode each factor into the correct 1:4 ordering,
    #    then turn into plain integers.
    df <- df_raw %>%
      mutate(
        # Economic vulnerability: map your factor levels in the
        # order that corresponds to 1 → 4
        Foodexp_4pt = as.integer(
          fct_relevel(
            Foodexp_4pt,
            "<50%", # will become 1
            "50-65%", # 2
            "65-75%", # 3
            ">75%" # 4
          )
        ),

        # Livelihood coping strategies:
        LhCSICat = as.integer(
          fct_relevel(
            LhCSICat,
            "NoStrategies", # 1
            "StressStrategies", # 2
            "CrisisStrategies", # 3
            "EmergencyStrategies" # 4
          )
        ),

        # CARI FES:
        CARI_FES = as.integer(
          fct_relevel(
            CARI_FES,
            "Food secure", # 1
            "Marginally food secure", # 2
            "Moderately food insecure", # 3
            "Severely food insecure" # 4
          )
        ),

        # (Your FCS_4pt was already integer, so leave it.)
        FCS_4pt = as.integer(FCS_4pt)
      )

    # 2) Now your pct4() helper will see true integers 1–4
    pct4 <- function(var) {
      df %>%
        filter(!is.na({{ var }})) %>%
        count({{ var }}) %>%
        mutate(p = round(100 * n / sum(n), 0)) %>%
        complete({{ var }} := 1:4, fill = list(p = 0)) %>%
        arrange({{ var }}) %>%
        pull(p)
    }

    # 3) Build your four vectors
    f <- pct4(FCS_4pt)
    e <- pct4(Foodexp_4pt)
    c <- pct4(LhCSICat)
    a <- pct4(CARI_FES)

    # 4) Return the named list for glue_data()
    list(
      fcs1 = f[1],
      fcs2 = f[2],
      fcs3 = f[3],
      fcs4 = f[4],
      exp1 = e[1],
      exp2 = e[2],
      exp3 = e[3],
      exp4 = e[4],
      cs1 = c[1],
      cs2 = c[2],
      cs3 = c[3],
      cs4 = c[4],
      ca1 = a[1],
      ca2 = a[2],
      ca3 = a[3],
      ca4 = a[4]
    )
  })

  # 2) Build the HTML table using glue() and renderUI()
  output$cariCtable <- renderUI({
    p <- pct()

    html <- glue_data(
      p,
      "
      <table style='border-collapse:collapse;width:100%;text-align:center;'>
        <tr>
          <th style='border:1px solid #000;width:15%;'>Domain</th>
          <th style='border:1px solid #000;width:20%;'>Indicator</th>
          <th style='border:1px solid #000;background-color:#F9E3D3;width:15%;'>Food Secure</th>
          <th style='border:1px solid #000;background-color:#EEAE7F;width:15%;'>Marginally Food Secure</th>
          <th style='border:1px solid #000;background-color:#F43200;width:15%;'>Moderately Food Insecure</th>
          <th style='border:1px solid #000;background-color:#B60000;color:white;width:15%;'>Severely Food Insecure</th>
        </tr>

        <!-- Current Status row -->
      <tr>
        <td style='border:1px solid #000;'><b>Current Status</b></td>
        <td style='border:1px solid #000;'>
          <em>Food consumption</em><br/>
          <em>FCS and rCSI</em>
        </td>
        <td style='border:1px solid #000;'>Acceptable<br/><b>{fcs1}%</b></td>
        <td style='border:1px solid #000;'>Acceptable &amp; rCSI ≥ 4<br/><b>{fcs2}%</b></td>
        <td style='border:1px solid #000;'>Borderline<br/><b>{fcs3}%</b></td>
        <td style='border:1px solid #000;'>Poor<br/><b>{fcs4}%</b></td>
      </tr>

        <!-- Coping Capacity → Economic Vulnerability -->
      <tr>
        <td style='border:1px solid #000;' rowspan='2'><b>Coping Capacity</b></td>
        <td style='border:1px solid #000;'>
          <em>Economic Vulnerability</em><br/>
          <em>Food Expenditure Share</em>
        </td>
        <td style='border:1px solid #000;'>&lt;50%<br/><b>{exp1}%</b></td>
        <td style='border:1px solid #000;'>50–65%<br/><b>{exp2}%</b></td>
        <td style='border:1px solid #000;'>65–75%<br/><b>{exp3}%</b></td>
        <td style='border:1px solid #000;'>&gt;75%<br/><b>{exp4}%</b></td>
      </tr>

      <!-- Coping Capacity → Livelihood Coping Strategies -->
      <tr>
        <td style='border:1px solid #000;'>
          <em>Livelihood Coping Strategies</em><br/>
          <em>Livelihood coping strategies</em>
        </td>
        <td style='border:1px solid #000;'>No coping<br/><b>{cs1}%</b></td>
        <td style='border:1px solid #000;'>Stress<br/><b>{cs2}%</b></td>
        <td style='border:1px solid #000;'>Crisis<br/><b>{cs3}%</b></td>
        <td style='border:1px solid #000;'>Emergency<br/><b>{cs4}%</b></td>
      </tr>

        <!-- Final CARI row, merging Domain + Indicator -->
      <tr>
        <td style='border:1px solid #000; background-color:#ccc;' colspan='2'><b>CARI</b></td>
        <td style='border:1px solid #000; background-color:#ccc;'><b>{ca1}%</b></td>
        <td style='border:1px solid #000; background-color:#ccc;'><b>{ca2}%</b></td>
        <td style='border:1px solid #000; background-color:#ccc;'><b>{ca3}%</b></td>
        <td style='border:1px solid #000; background-color:#ccc;'><b>{ca4}%</b></td>
      </tr>
      </table>
    "
    ) %>%
      as.character()

    HTML(html)
  })

  ##################################################
  # CARI by Admin1
  ##################################################
  # (B) CARI by Admin1 => No admin filter
  output$plotCARIadm1 <- renderPlotly({
    df_cari <- dynamicCARI()

    # Summarize final CARI_FES across all Admin1
    chart_data <- df_cari %>%
      group_by(ADMIN1Name, CARI_FES) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN1Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(
      chart_data,
      aes(
        x = ADMIN1Name,
        y = perc,
        fill = CARI_FES,
        text = paste0(
          "ADMIN1Name: ",
          ADMIN1Name,
          "<br>",
          "CARI: ",
          CARI_FES,
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
      scale_fill_manual(values = CARI_Colors) +
      labs(x = "Admin1", y = "Percentage", fill = "CARI_FES") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

  ##################################################
  # CARI by Admin2
  ##################################################
  observeEvent(dynamicCARI(), {
    df <- dynamicCARI()
    admin1Vals <- sort(unique(df$ADMIN1Name))
    updateSelectInput(
      session,
      "admin1FilterCARI2",
      choices = admin1Vals,
      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  output$plotCARIadm2 <- renderPlotly({
    df_cari <- dynamicCARI()
    req(input$admin1FilterCARI2)

    df_filtered <- df_cari %>%
      filter(ADMIN1Name == input$admin1FilterCARI2)

    chart_data <- df_filtered %>%
      group_by(ADMIN2Name, CARI_FES) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(ADMIN2Name) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(
      chart_data,
      aes(
        x = ADMIN2Name,
        y = perc,
        fill = CARI_FES,
        text = paste0(
          "ADMIN2Name: ",
          ADMIN2Name,
          "<br>",
          "CARI: ",
          CARI_FES,
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
      scale_fill_manual(values = CARI_Colors) +
      labs(x = "Admin2", y = "Percentage", fill = "CARI_FES") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

  ##################################################
  # CARI by Admin1 & Enumerator
  ##################################################
  ##############################################################################
  # 1) Observe dynamicCARI() => populate Admin1 dropdown
  ##############################################################################
  observeEvent(dynamicCARI(), {
    df <- dynamicCARI()
    admin1Vals <- sort(unique(df$ADMIN1Name))

    updateSelectInput(
      session,
      "admin1FilterEnumCARI",
      choices = admin1Vals,
      selected = if (length(admin1Vals) > 0) admin1Vals[1] else NULL
    )
  })

  ##############################################################################
  # 2) Whenever user picks an Admin1, populate Admin2 with "All" + real Admin2
  ##############################################################################
  observeEvent(input$admin1FilterEnumCARI, {
    df <- dynamicCARI()
    req(input$admin1FilterEnumCARI)

    # Filter data to chosen Admin1
    df_sub <- df %>%
      filter(ADMIN1Name == input$admin1FilterEnumCARI)

    # Gather unique textual Admin2
    admin2Vals <- sort(unique(df_sub$ADMIN2Name))

    updateSelectInput(
      session,
      "admin2FilterEnumCARI",
      choices = c("All", admin2Vals),
      selected = "All"
    )
  })

  ##############################################################################
  # 3) Produce the chart
  ##############################################################################
  output$plotCARIadm1Enum <- renderPlotly({
    df_cari <- dynamicCARI()

    # 1) Force user to pick an Admin1
    req(input$admin1FilterEnumCARI)
    # 2) They also pick an Admin2 or "All"
    req(input$admin2FilterEnumCARI)

    # Filter by chosen Admin1
    df_filtered <- df_cari %>%
      filter(ADMIN1Name == input$admin1FilterEnumCARI)

    # If user picks a specific Admin2 (not "All"), filter further
    if (input$admin2FilterEnumCARI != "All") {
      df_filtered <- df_filtered %>%
        filter(ADMIN2Name == input$admin2FilterEnumCARI)
    }

    # Summarize enumerator distribution of final CARI_FES
    chart_data <- df_filtered %>%
      group_by(EnuName, CARI_FES) %>%
      summarize(n = n(), .groups = "drop") %>%
      group_by(EnuName) %>%
      mutate(perc = n / sum(n)) %>%
      ungroup()

    p <- ggplot(
      chart_data,
      aes(
        x = EnuName,
        y = perc,
        fill = CARI_FES,
        text = paste0(
          "EnuName: ",
          EnuName,
          "<br>",
          "CARI: ",
          CARI_FES,
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
      scale_fill_manual(values = CARI_Colors) +
      labs(x = "Enumerator", y = "Percentage", fill = "CARI_FES") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))

    ggplotly(p, tooltip = "text")
  })

  ##############################################################################
  # 11) Report - generate html report
  ##############################################################################

  ### Track visited tabs (main ones from sidebar and, optionally, sub-tabs)
  visitedTabs <- reactiveValues(visited = character())

  # Observe the main sidebar menu selection (id = "tabs")
  observe({
    req(input$tabs)
    visitedTabs$visited <- unique(c(visitedTabs$visited, input$tabs))
    cat("Main tab visited:", input$tabs, "\n")
  })

  # Optionally, observe sub-tab selections if you want to include them
  observe({
    if (!is.null(input$surveyTabs)) {
      visitedTabs$visited <- unique(c(visitedTabs$visited, input$surveyTabs))
    }
    if (!is.null(input$fcsTabs)) {
      visitedTabs$visited <- unique(c(visitedTabs$visited, input$fcsTabs))
    }
    if (!is.null(input$hddsTabs)) {
      visitedTabs$visited <- unique(c(visitedTabs$visited, input$hddsTabs))
    }
    if (!is.null(input$rcsiTabs)) {
      visitedTabs$visited <- unique(c(visitedTabs$visited, input$rcsiTabs))
    }
    if (!is.null(input$hhsTabs)) {
      visitedTabs$visited <- unique(c(visitedTabs$visited, input$hhsTabs))
    }
    if (!is.null(input$lcsTabs)) {
      visitedTabs$visited <- unique(c(visitedTabs$visited, input$lcsTabs))
    }
    if (!is.null(input$fesTabs)) {
      visitedTabs$visited <- unique(c(visitedTabs$visited, input$fesTabs))
    }
    if (!is.null(input$cariTabs)) {
      visitedTabs$visited <- unique(c(visitedTabs$visited, input$cariTabs))
    }
  })

  # (Optional) Debug print of visited tabs
  output$debugVisitedTabs <- renderPrint({
    if (length(visitedTabs$visited) == 0) {
      "No tabs visited yet."
    } else {
      visitedTabs$visited
    }
  })

  # Render the UI for the download button: only show when all required main tabs are visited.
  output$reportDownloadUI <- renderUI({
    req(visitedTabs$visited)
    requiredTabs <- c(
      "upload",
      "survey",
      "fcs",
      "hdds",
      "rcsi",
      "hhs",
      "lcs",
      "fes",
      "cari"
    )
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

  # Download handler for generating the HTML report
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("Report_", Sys.Date(), ".html")
    },
    content = function(file) {
      # 1. Find the path to the Rmd template shipped in the package:
      rmd_template <- system.file("app", "report.Rmd", package = "myshinyapp")
      if (rmd_template == "") {
        stop("Cannot find report.rmd in the installed package.")
      }

      # 2. Copy it to a temporary directory; render from there:
      temp_rmd <- tempfile(fileext = ".Rmd")
      file.copy(rmd_template, temp_rmd, overwrite = TRUE)

      # Render the R Markdown report to HTML
      rmarkdown::render(
        input = temp_rmd,
        output_file = file,
        params = list(
          dataset = reqData(),
          dynamicCARI = dynamicCARI(),
          admin1Filter = input$admin1Filter,
          filterEnumerator = input$filterEnumerator,
          fcsThresholdAdm1 = input$fcsThresholdAdm1,
          admin1FilterHDDS2 = input$admin1FilterHDDS2,
          admin1FilterEnumHDDS = input$admin1FilterEnumHDDS,
          admin2FilterEnumHDDS = input$admin2FilterEnumHDDS,
          admin1FilterrCSI2 = input$admin1FilterrCSI2,
          admin1FilterEnumrCSI = input$admin1FilterEnumrCSI,
          admin2FilterEnumrCSI = input$admin2FilterEnumrCSI,
          admin1FilterFCS2 = input$admin1FilterFCS2,
          fcsThresholdAdm2 = input$fcsThresholdAdm2,
          admin1FilterEnumFCS = input$admin1FilterEnumFCS,
          admin2FilterEnumFCS = input$admin2FilterEnumFCS,
          fcsThresholdAdm1Enum = input$fcsThresholdAdm1Enum,
          rcsiBoxAdmin2Admin1 = input$rcsiBoxAdmin2Admin1,
          triangAdmin1 = input$triangAdmin1,
          triangAdmin2 = input$triangAdmin2,
          triangFCSCat = input$triangFCSCat,
          admin1FilterHHS2 = input$admin1FilterHHS2,
          admin1FilterEnumHHS = input$admin1FilterEnumHHS,
          admin2FilterEnumHHS = input$admin2FilterEnumHHS,
          matAdmin1 = input$matAdmin1,
          matAdmin2 = input$matAdmin2,
          matFCSCat = input$matFCSCat,
          stressVars = input$stressVars,
          crisisVars = input$crisisVars,
          emergencyVars = input$emergencyVars,
          admin1FilterLCS2 = input$admin1FilterLCS2,
          admin1FilterEnumLCS = input$admin1FilterEnumLCS,
          admin2FilterEnumLCS = input$admin2FilterEnumLCS,
          lcsStrategyAdmin1 = input$lcsStrategyAdmin1,
          lcsStrategyAdmin2 = input$lcsStrategyAdmin2,
          lcsStrategyEnu = input$lcsStrategyEnu,
          admin1FilterFES2 = input$admin1FilterFES2,
          admin1FilterEnumFES = input$admin1FilterEnumFES,
          admin2FilterEnumFES = input$admin2FilterEnumFES,
          fesBoxAdmin2Admin1 = input$fesBoxAdmin2Admin1,
          admin1FilterCARI2 = input$admin1FilterCARI2,
          admin1FilterEnumCARI = input$admin1FilterEnumCARI,
          admin2FilterEnumCARI = input$admin2FilterEnumCARI
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
}