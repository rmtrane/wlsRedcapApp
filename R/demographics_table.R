#' Summary Table with Demographic Information
#'
#'
#' @param dat Data from REDCap
#' @param studyid String with ID for participant of interest. Much be length 1.
#'
#' @examples
#' demographics_table(demo_data, "326231g")
#'
#' @export

demographics_table <- function(
    dat,
    studyid = "102038g" # "101015g"
) {

  stopifnot("More than one study ID provided" = length(studyid) == 1)
  stopifnot("Study ID not in data set" = studyid %in% dat$cog_studyid)

  cur_pt_dat <- dat |>
    dplyr::filter(
      .data$cog_studyid == studyid
    ) |>
    dplyr::select(
      `Study ID:` = .data$cog_studyid,
      `Education (years):` = .data$cog_education,
      .data$cog_age,
      `Gender:` = .data$cog_sex,
      `Handedness:` = .data$cog_handedness,
      `Race:` = .data$cog_race
    ) |>
    dplyr::arrange(
      .data$cog_age
    )

  if (nrow(cur_pt_dat) > 1) {
    # Combine ages
    cur_pt_dat <- cur_pt_dat |>
      dplyr::mutate(
        cog_age = paste(.data$cog_age, collapse = "/")
      ) |>
      unique() |>
      dplyr::rename(
        `Age at Visits:` = .data$cog_age
      )

    stopifnot("One or more of the following varies between visits: education, gender, handedness, race" = nrow(cur_pt_dat) == 1)
  } else {
    cur_pt_dat <- cur_pt_dat |>
      dplyr::rename(
        `Age at Visit:` = .data$cog_age
      )
  }

  cur_pt_dat |>
    tidyr::pivot_longer(
      tidyselect::everything(),
      values_transform = \(x) as.character(x)
    ) |>
    gt::gt() |>
    gt::cols_align(
      "right",
      .data$name
    ) |>
    gt::tab_style(
      style = gt::cell_text(
        weight = "bold",
        size = 16
      ),
      locations = gt::cells_body(
        rows = .data$name == "Study ID:"
      )
    ) |>
    gt::tab_options(
      column_labels.hidden = T,
      table.border.left.style = "solid",
      table.border.right.style = "solid",
      table.font.size = gt::px(16),
      table.width = gt::px(250)
    )

}
