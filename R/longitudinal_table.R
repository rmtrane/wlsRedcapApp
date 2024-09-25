#' Longitudinal Table of Standardized Scores
#'
#' Create a table of standardized scores across multiple visits.
#'
#' @param dat Data to use. Should include one or more standardized cognitive variables.
#' @param fill_values a named vector of length seven with hex color values to use for categories
#'   "Impaired", "Borderline", "Low Average", "Average", "High Average", "Superior", "Very Superior"
#'
#' @export
longitudinal_table <- function(
    dat,
    fill_values = setNames(
      RColorBrewer::brewer.pal(n = 7, "RdYlGn"),
      nm = c(
        "Impaired", # = "red"
        "Borderline", # = "darkorange",
        "Low Average", # = "orange",
        "Average", # = "yellow"
        "High Average", # = "",
        "Superior", # = "lightgreen",
        "Very Superior" # = "green",
      )
    )
) {

  stopifnot("Data provided must pertain to only one individual" = length(unique(dat$cog_studyid)) == 1)

  stopifnot(
    "Data must contain a column labelled 'cog_studyid' giving unique IDs for participants" =
      "cog_studyid" %in% colnames(dat)
  )

  out <- dat |>
    dplyr::select(
      .data$cog_test_date,
      # tidyselect::starts_with("standardized_")
      tidyselect::any_of(paste0("standardized_", names(cog_vars_labels)))
    ) |>
    dplyr::select(
      tidyselect::where(\(x) sum(!is.na(x)) > 0)
    ) |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("standardized_")
    ) |>
    tidyr::pivot_wider(
      names_from = "cog_test_date",
      values_from = "value"
    ) |>
    dplyr::mutate(
      name = stringr::str_remove(.data$name, "^standardized_"),
      labels = stringr::str_replace_all(
        .data$name,
        setNames(cog_vars_labels, paste0("^", names(cog_vars_labels), "$"))
      ) |>
        factor(
          levels = unname(cog_vars_labels)
        ),
      group = stringr::str_replace_all(
        .data$name,
        setNames(cog_vars_groups, paste0("^", names(cog_vars_groups), "$"))
      )
    ) |>
    dplyr::arrange("labels")

  out <- gt::gt(
    out,
    rowname_col = "labels",
    groupname_col = "group"
  ) |>
    gt::row_group_order(
      groups = unique(cog_vars_groups)[unique(cog_vars_groups) %in% out$group]
    ) |>
    gt::cols_hide("name") |>
    gt::cols_align("left", "labels") |>
    gt::cols_align_decimal(-c("name", "labels", "group")) |>
    gt::cols_width(
      -c("name", "labels", "group") ~ px(95),
      labels ~ px(350)
    ) |>
    ## Make parts of the table bold
    gt::tab_options(
      table.font.names = "Arial",
      column_labels.font.weight = "bold",
      row_group.font.weight = "bold"
    ) |>
    ## Some formatting
    gt::fmt(
      columns = -c("name", "labels", "group"),
      fns = \(x) sprintf("%.2f", x)
    ) |>
    gt::tab_stub_indent(
      rows = tidyselect::everything(),
      indent = 5
    ) |>
    gt::sub_missing() # |>

  z_scores_from_percentiles <- c(-Inf, qnorm(c(0.03, 0.10, 0.25, 0.76, 0.92, 0.97)), Inf)

  for (cur_col in unique(as.character(dat$cog_test_date))) {
    for (i in seq_along(fill_values)) {
      out <- out |>
        gt::tab_style(
          style = gt::cell_fill(fill_values[[i]]),
          locations = gt::cells_body(
            columns = {{ cur_col }},
            rows = .data[[cur_col]] < z_scores_from_percentiles[[i+1]] &
              .data[[cur_col]] > z_scores_from_percentiles[[i]]
          )
        )
    }
  }

  out

}
