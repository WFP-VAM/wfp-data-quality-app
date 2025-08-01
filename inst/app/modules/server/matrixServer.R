fewsnet_matrix_module <- function(input, output, session, reqData) {


      # 1) Populate Admin1 → Admin2 dynamically
  observe({
    df <- reqData() # your reactive data
    a1 <- sort(unique(df$ADMIN1Name))
    updateSelectInput(session, "matAdmin1", choices = a1, selected = a1[1])
  })

  observeEvent(input$matAdmin1, {
    df1 <- reqData() %>% filter(ADMIN1Name == input$matAdmin1)
    a2 <- c("All", sort(unique(df1$ADMIN2Name)))
    updateSelectInput(session, "matAdmin2", choices = a2, selected = "All")
  })

  # 2) Reactive slice with the 3 categories + cell index
  mat_df <- reactive({
    req(input$matAdmin1, input$matAdmin2, input$matFCSCat)

    df <- reqData() %>%
      filter(ADMIN1Name == input$matAdmin1)
    if (input$matAdmin2 != "All") {
      df <- df %>% filter(ADMIN2Name == input$matAdmin2)
    }

    df %>%
      mutate(
        # HHS category
        HHS_cat = case_when(
          HHS == 0 ~ "HHS=0",
          HHS == 1 ~ "HHS=1",
          HHS %in% 2:3 ~ "HHS=2-3",
          HHS == 4 ~ "HHS=4",
          HHS %in% 5:6 ~ "HHS=5-6",
          TRUE ~ NA_character_
        ),
        # rCSI category
        rCSI_cat = case_when(
          rCSI < 4 ~ "<4",
          rCSI >= 4 & rCSI <= 18 ~ "4-18",
          rCSI > 18 ~ ">18",
          TRUE ~ NA_character_
        ),
        # choose FCS column
        FCS_cat = .data[[input$matFCSCat]]
      )
  })

  # 3) The matrix table
  output$matrixTable <- renderDT({
    df <- mat_df()
    total <- nrow(df)

    # (a) count up the 3‑way cross‑tab
    long0 <- df %>%
      count(HHS_cat, rCSI_cat, FCS_cat, name = "n")

    # (b) make sure every combo appears
    long1 <- long0 %>%
      complete(
        HHS_cat = c("HHS=0", "HHS=1", "HHS=2-3", "HHS=4", "HHS=5-6"),
        rCSI_cat = c("<4", "4-18", ">18"),
        FCS_cat = c("Acceptable", "Borderline", "Poor"),
        fill = list(n = 0)
      )

    # (c) compute a 1…45 cell index in row‑major order (5 rows × 9 cols)
    long2 <- long1 %>%
      mutate(
        # 0‑based indices
        hhs_i = as.integer(factor(
          HHS_cat,
          levels = c("HHS=0", "HHS=1", "HHS=2-3", "HHS=4", "HHS=5-6")
        )) -
          1,
        rcsi_i = as.integer(factor(rCSI_cat, levels = c("<4", "4-18", ">18"))) -
          1,
        fcs_i = as.integer(factor(
          FCS_cat,
          levels = c("Acceptable", "Borderline", "Poor")
        )) -
          1,
        # ROW‑MAJOR: each HHS row has 9 cols
        cell = rcsi_i * 15 + fcs_i * 5 + hhs_i + 1
      )

    # your 45‑long vector of colours, in the exact order 1→45
    cell_colors <- c( "#CDFACD", "#FAE61E", "#FAE61E", "#E67800", "#E67800", "#CDFACD", "#FAE61E", "#E67800", "#E67800", "#C80000", "#FAE61E", "#FAE61E", "#E67800", "#C80000", "#C80000", "#FAE61E", "#FAE61E", "#FAE61E", "#E67800", "#E67800", "#FAE61E", "#FAE61E", "#E67800", "#E67800", "#C80000", "#FAE61E", "#E67800", "#E67800", "#C80000", "#640000", "#FAE61E", "#FAE61E", "#E67800", "#E67800", "#C80000", "#FAE61E", "#E67800", "#E67800", "#C80000", "#C80000", "#E67800", "#E67800", "#E67800", "#C80000", "#640000" )

    # (d) build the HTML label, pulling the correct colour & text style
    white_cells <- c(10, 14, 15, 25, 29, 30, 35, 39, 40, 44, 45)

    long3 <- long2 %>%
      mutate(
        pct = 100 * n / total,
        is_white = cell %in% white_cells,
        style = paste0(
          "background:",
          cell_colors[cell],
          ";",
          "padding:6px;",
          "text-align:center;",
          "font-weight:bold;", # make everything bold
          if_else(is_white, "color:white;", "") # turn text white for special cells
        ),
        label = sprintf(
          '<div style="%s">%d<br/><small>(%.1f%%)</small></div>',
          style,
          n,
          pct
        )
      ) %>%
      select(-is_white, -style)

    # (e) pivot to 5×9
    wide <- long3 %>%
      select(HHS_cat, rCSI_cat, FCS_cat, label) %>%
      pivot_wider(
        names_from = c(rCSI_cat, FCS_cat),
        names_sep = "|",
        values_from = label
      )

    # (f) two‑row header
    sketch <- tags$table(
      class = 'display',
      tags$thead(
        tags$tr(
          tags$th(rowspan = "2", "HHS \\ rCSI \\ FCS"),
          tags$th(colspan = "3", "rCSI < 4"),
          tags$th(colspan = "3", "rCSI 4‑18"),
          tags$th(colspan = "3", "rCSI > 18")
        ),
        tags$tr(
          rep(
            list(
              tags$th("Acceptable"),
              tags$th("Borderline"),
              tags$th("Poor")
            ),
            3
          )
        )
      )
    )

    datatable(
      wide,
      container = sketch,
      escape = FALSE,
      rownames = FALSE,
      class = 'nowrap', # prevent automatic wrapping
      options = list(
        dom = 't',
        paging = FALSE,
        ordering = FALSE,
        scrollX = TRUE, # <-- enable horizontal scrolling
        autoWidth = TRUE, # <-- let DT calculate column widths
        columnDefs = list(
          list(className = 'dt-center', targets = 1:(ncol(wide) - 1))
        )
      )
    )
  })

  # 4) Details table for illogical cells
  output$matrixDetails <- renderDT(
    {
      # illogical cell indices
      illogical <- c(3, 4, 5, 8, 9, 10)

      # capture which column you chose in the matrix:
      # Decide which FCSCat column to include (only the one selected)
      selFCS <- if (input$matFCSCat == "FCSCat21") "FCSCat21" else "FCSCat28"

      df <- mat_df() %>%
        # re‑derive the cell index exactly as in the main matrix
        mutate(
          hhs_i = as.integer(factor(
            HHS_cat,
            levels = c("HHS=0", "HHS=1", "HHS=2-3", "HHS=4", "HHS=5-6")
          )) -
            1,
          rcsi_i = as.integer(factor(
            rCSI_cat,
            levels = c("<4", "4-18", ">18")
          )) -
            1,
          fcs_i = as.integer(factor(
            FCS_cat,
            levels = c("Acceptable", "Borderline", "Poor")
          )) -
            1,
          cell = rcsi_i * 15 + fcs_i * 5 + hhs_i + 1
        ) %>%
        filter(cell %in% illogical) %>%
        # only show the selected FCSCat column as "FCS"
        select(
          ADMIN1Name,
          ADMIN2Name,
          ADMIN3Name,
          ADMIN4Name,
          EnuSupervisorName,
          EnuName,
          HHSizeCalc,
          cell,
          rCSI,
          FCS,
          all_of(selFCS),
          HHS,
          FCSStap,
          FCSPulse,
          FCSDairy,
          FCSPr,
          FCSVeg,
          FCSFruit,
          FCSFat,
          FCSSugar,
          rCSILessQlty,
          rCSIBorrow,
          rCSIMealSize,
          rCSIMealAdult,
          rCSIMealNb,
          HHhSNoFood_FR_r,
          HHhSBedHung_FR_r,
          HHhSNotEat_FR_r
        )

      datatable(
        df,
        filter = "top",
        rownames = FALSE,
        extensions = c("Buttons", "Scroller"),
        options = list(
          dom = 'Bfrtip',
          buttons = list(
            list(
              extend = 'copy',
              exportOptions = list(modifiers = list(page = 'all'))
            ),
            list(
              extend = 'csv',
              exportOptions = list(modifiers = list(page = 'all'))
            ),
            list(
              extend = 'excel',
              exportOptions = list(modifiers = list(page = 'all'))
            )
          ),
          pageLength = 25,
          scrollY = '400px',
          scrollX = TRUE,
          scroller = TRUE,
          deferRender = TRUE # recommended for performance with Scroller
        )
      )
    },
    server = TRUE
  )
}