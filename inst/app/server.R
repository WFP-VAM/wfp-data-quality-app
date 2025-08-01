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

  lcs_module(input, output, session, reqData, LHCS_colors)

  #########################
  # 9) FES
  #########################

  ##################################################
  # FES by Admin1
  ##################################################
  fes_module(input, output, session, reqData, FES_Colors) 

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