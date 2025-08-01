lcs_module <- function(input, output, session, reqData, LHCS_colors) {
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
}