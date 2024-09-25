#' Summary Table with Functional Measures
#'
#' @param dat Data set to use; see `demo_data` for example
#'
#' @examples
#' functional_measures_table(dplyr::filter(demo_data, .data$cog_studyid == .data$cog_studyid[1]))
#'
#' @export

functional_measures_table <- function(
    dat
) {

  stopifnot("More than one study ID in data" = length(unique(dat$cog_studyid)) == 1)

  dat |>
    dplyr::mutate(
      visit = paste("Visit", order(.data$cog_test_date))
    ) |>
    dplyr::select(
      "visit",
      `FAS:` = "cog_fas",
      `Informant:` = "cog_iqcode_inform",
      `Self:` = "cog_iqcode_self",
      `Global:` = "cog_cdr_global",
      `SOB:` = "cog_cdr_sob",
    ) |>
    tidyr::pivot_longer(
      cols = -"visit"
    ) |>
    tidyr::pivot_wider(
      names_from = "visit",
      values_from = "value"
    ) |>
    dplyr::mutate(
      name = dplyr::if_else(.data$name == "FAS", "**FAS**", .data$name)
    ) |>
    gt::gt(rowname_col = 'name') |>
    gt::tab_header(
      gt::md("**Functional Measures**")
    ) |>
    gt::sub_missing() |>
    gt::tab_row_group(
      label = gt::md("<u>CDR</u>"),
      rows = .data$name %in% c("Global:", "SOB:"),
      id = "2"
    ) |>
    gt::tab_row_group(
      label = gt::md("<u>IQ-CODES</u>"),
      rows = stringr::str_detect(.data$name, "Informant|Self"), #c(2,3),
      id = "3"
    ) |>
    gt::row_group_order(
      groups = c(NA, "2", "3")
    ) |>
    gt::cols_align("center") |>
    gt::cols_align(
      "right", "name"
    ) |>
    gt::fmt_markdown() |>
    gt::tab_stub_indent(
      indent = 4,
      rows = c(2,3)
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("left"),
        style = 'hidden'
      ),
      location = gt::cells_body()
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("top"),
        style = 'hidden'
      ),
      location = gt::cells_row_groups()
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("top"),
        style = 'solid',
        weight = gt::px(2),
        color = "lightgrey"
      ),
      location = gt::cells_row_groups(c("2", "3"))
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = "bottom",
        style = "hidden"
      ),
      locations = list(
        gt::cells_body(
          rows = .data$name %in% c("Global:", "Informant:")
        ),
        gt::cells_stub(
          rows = .data$name %in% c("Global:", "Informant:")
        ),
        gt::cells_row_groups(
          groups = c("2", "3")
        )
      )
    ) |>
    gt::tab_options(
      row_group.border.top.style = "solid",
      table.border.left.style = 'solid',
      table.border.right.style = 'solid',
      #table_body.border.bottom.style = 'hidden',
      heading.title.font.size = gt::px(16),
      row_group.font.size = gt::px(16),
      table.font.size = gt::px(16),
      table.width = gt::px(275)
    )
}
