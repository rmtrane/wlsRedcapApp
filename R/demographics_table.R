#' Summary Table with Demographic Information
#'
#'
#' @param dat Data from REDCap. Should be a
#'
#' @examples
#' demographics_table(demo_data[1,])
#'
#' @export

demographics_table <- function(
    dat
) {
  stopifnot("More than one study ID in data" = length(unique(dat$cog_studyid)) == 1)

  # print(paste("Updating demographics with", unique(dat$cog_studyid)))
  # stopifnot("Study ID not in data set" = studyid %in% dat$cog_studyid)

  cur_pt_dat <- dat |>
    dplyr::select(
      `Study ID:` = "cog_studyid",
      `Education (years):` = "cog_education",
      "cog_age",
      `Gender:` = "cog_sex",
      `Handedness:` = "cog_handedness",
      `Race:` = "cog_race"
    ) |>
    # dplyr::reframe(
    #   dplyr::across(everything(), unique)
    # ) |>
    unique() |>
    dplyr::arrange(
      "cog_age"
    )

  include_footnote <- max(sapply(c("Education (years):","Gender:","Handedness:","Race:"),
                                 FUN = \(x) length(unique(cur_pt_dat[[x]])))) > 1

  if (nrow(cur_pt_dat) > 1) {

    cur_pt_dat <- cur_pt_dat |>
      dplyr::summarize(
        dplyr::across(tidyselect::everything(), \(x) {
          uniq <- unique(x)
          if (length(uniq) > 1)
            return(paste(x, collapse = "/"))

          uniq
        })
      ) |>
      dplyr::rename(
        "Age at Visits:" = "cog_age"
      )
  } else {
    cur_pt_dat <- cur_pt_dat |>
      dplyr::rename(
        "Age at Visit:" = "cog_age"
      )
  }

  out <- cur_pt_dat |>
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
      table.width = gt::px(275)
    )

  if (include_footnote) {
    out <- out |>
      gt::tab_footnote(footnote = paste("One or more variables other than 'Age' vary between visits and have been highlighted in red.")) |>
      gt::tab_style(
        style = gt::cell_text(color = "red", weight = "bold"),
        locations = gt::cells_body(
          rows = !stringr::str_detect(.data$name, "Age") & stringr::str_detect(.data$value, "/")
        )
      )
  }

  out
}
