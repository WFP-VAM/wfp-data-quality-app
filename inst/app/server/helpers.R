# helpers.R
calculate_percentages <- function(df, group_cols, value_col) {
  df %>%
    group_by(across(all_of(group_cols))) %>%
    summarize(n = n(), .groups = "drop") %>%
    group_by(across(group_cols[1])) %>% 
    mutate(perc = n / sum(n)) %>%
    ungroup()
}

create_stacked_bar <- function(data, x, fill, colors, title = NULL) {
  ggplot(data, aes(x = {{x}}, y = perc, fill = {{fill}})) +
    geom_col(position = "stack") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
}