plot_dynamic_lcs <- function(input, output, dynamicLCS, LHCS_colors) {
  output$plotDynamicLCS <- renderPlotly({
    df_lcs <- req(dynamicLCS())

    dist_data <- df_lcs %>%
      dplyr::count(LhCSICat, name = "n") %>%
      dplyr::mutate(perc = round(100 * n / sum(n, na.rm = TRUE), 1))

    slice_colors <- unname(LHCS_colors[as.character(dist_data$LhCSICat)])

    plotly::plot_ly(
      data   = dist_data,
      labels = ~LhCSICat,
      values = ~n,
      type   = "pie",
      marker = list(colors = slice_colors),
      text   = ~paste0(n, " obs"),
      hovertemplate = paste(
        "<b>%{label}</b><br>",
        "%{value} obs<br>",
        "%{percent}",
        "<extra></extra>"
      ),
      textinfo = "label+percent"
    ) %>%
      plotly::layout(
        title      = list(text = "Dynamic LhCSICat Distribution", x = 0.5),
        showlegend = FALSE
      )
  })
}
