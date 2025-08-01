hdds_module <- function(input, output, session, reqData, HDDS_colors) {
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