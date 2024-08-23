#' Summary Table with Functional Measures
#'
#' @param dat Data set to use; see `demo_data` for example
#' @param studyid String giving participant id. Must be length 1 and one of
#' the IDs in `dat$cog_studyid`.
#'
#' @examples
#' functional_measures_table(demo_data, demo_data$cog_studyid[1])
#'
#' @export

functional_measures_table <- function(
    dat,
    studyid = "102038g"
) {

  stopifnot("More than one study ID provided" = length(studyid) == 1)
  stopifnot("Study ID not in data set" = studyid %in% dat$cog_studyid)

  dat |>
    dplyr::filter(
      .data$cog_studyid == studyid
    ) |>
    dplyr::mutate(
      visit = paste("Visit", order(.data$cog_test_date))
    ) |>
    dplyr::select(
      "visit",
      `FAS:` = "cog_fas",
      `Informant:` = "cog_iqcode_inform",
      `Self:` = "cog_iqcode_self"
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
    gt::tab_row_group(
      label = gt::md("<u>IQ-CODES<u>"),
      rows = c(2,3),
      id = "2"
    ) |>
    gt::row_group_order(
      groups = c(NA, "2")
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
      location = gt::cells_row_groups("2")
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("bottom"),
        style = 'hidden'
      ),
      location = gt::cells_row_groups("2")
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = "bottom",
        style = "hidden"
      ),
      locations = gt::cells_body(
        rows = 2
      )
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = "bottom",
        style = "hidden"
      ),
      locations = gt::cells_stub(
        rows = 2
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
      table.width = gt::px(250)
    )
}
