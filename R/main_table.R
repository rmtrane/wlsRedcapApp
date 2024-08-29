#' Create NACC T-Cog Neuropsychological Assessment Summary Table
#'
#' This function create a summary table of measures from the NACC T-Cog Neuropsychological
#' Assessment.
#'
#' @param dat Data to use
#' @param studyid String with ID of a participant. Must be length 1 and present in
#' `dat$cog_studyid`
#' @param cur_date String with visit date to use. Must be present in
#' `dplyr::filter(dat, cog_studyid == studyid)$cog_test_date`.
#' @param bar_height In pixels. Height of the percentile bars. Default: 16
#'
#' @returns An object of class `gt::gt_tbl`
#'
#' @examples
#' main_table(
#'   dat = demo_data,
#'   studyid = demo_data$cog_studyid[1],
#'   cur_date = demo_data$cog_test_date[1]
#' )
#'
#' @export
main_table <- function(
    dat,
    studyid = "102038g",
    cur_date = "2022-06-13",
    bar_height = 16
) {

  cur_pt_dat <- dat |>
    dplyr::filter(
      .data$cog_studyid == studyid,
      .data$cog_test_date == cur_date
    )

  if (nrow(cur_pt_dat) > 1) {
    warning("More than one row in data")
    stop()
  }

  ## Create labels to be used. First, if any of the necessary variables are
  ## missing, set to NA
  cur_labels <- with(
    cur_pt_dat,
    var_labels(
      cog_cdr_sob,
      cog_tmta_err,
      cog_tmta_lines,
      round(raw_cog_benson_delay / 16 * 100, digits = 1),
      cog_benson_recog,
      cog_ravlt_a1,
      cog_ravlt_a2,
      cog_ravlt_a3,
      cog_ravlt_a4,
      cog_ravlt_a5,
      cog_ravlt_recog_tp,
      cog_ravlt_recog_tn,
      cog_tmtb_err,
      cog_tmtb_lines
    )
  )

  # Check if any of the scores necessary are in cur_pt_dat, but was not
  # adjusted/rename
  for_main_table <- cur_pt_dat |>
    dplyr::select(
      # Identifiers
      .data$cog_studyid:.data$cog_education,
      # Get all scores
      tidyselect::matches(names(cur_labels))
    ) |>
    # Rename the scores that have not been adjusted. These should be
    # raw_(score)
    dplyr::rename_with(
      .fn = \(x) paste0("raw_", x),
      .cols = tidyselect::matches(paste0("^", names(cur_labels)))
    ) |>
    tidyr::pivot_longer(
      cols = c(
        tidyselect::starts_with("raw_"),
        tidyselect::starts_with("standardized_"),
        tidyselect::starts_with("ss_")
      ),
      names_to = c(".value", "name"),
      names_pattern = c("(raw|standardized|ss)_(.+)"),
      names_transform = list(
        .value = \(x) dplyr::case_match(x, "raw" ~ "Raw", "standardized" ~ "z-score", "ss" ~ "SS")
      )
    ) |>
    dplyr::mutate(
      # Create row groups
      group = dplyr::case_match(
        .data$name,
        c("cog_cdr_global",
          "cog_moca",
          "cog_moca_blind",
          "cog_ticsm") ~ "General Cognition",
        c("cog_tmta_time",
          "cog_otmta_time",
          "cog_otmta_error",
          "cog_nsf_total",
          "cog_nsf_span",
          "cog_nsb_total",
          "cog_nsb_span",
          "cog_digsym") ~ "Attention/Processing",
        c("cog_mint_tot",
          "cog_animal_flu",
          "cog_veg_flu",
          "cog_fl_flu",
          "cog_flc_flu",
          "cog_f_flu",
          "cog_l_flu") ~ "Language",
        "cog_benson_copy" ~ "Visuospatial",
        c("cog_benson_delay",
          "cog_craft_imm_ver",
          "cog_craft_imm_par",
          "cog_craft_delay_verb",
          "cog_craft_delay_par",
          "cog_ravlt_a1_a5_total",
          "cog_ravlt_b1",
          "cog_ravlt_a6",
          "cog_ravlt_a7",
          "cog_ravlt_recog_acc") ~ "Memory",
        c("cog_tmtb_time",
          "cog_otmtb_time",
          "cog_otmtb_error",
          "cog_moca_clock") ~ "Executive Functioning",
        "cog_gds15" ~ "Mood"
      ),
      # Create row labels
      labels = stringr::str_replace_all(.data$name, cur_labels) |>
        factor(levels = unname(cur_labels)),
      .after = .data$name
    ) |>
    # Remove rows with no raw observations
    dplyr::filter(!is.na(.data$Raw)) |>
    dplyr::mutate(
      # Get percentiles. This is done differently for different variables.
      Percentile = dplyr::case_when(
        # For time and number of errors, we want right-tailed probabilities
        .data$name %in% c("cog_tmta_time", "cog_tmtb_time",
                    "cog_otmta_time", "cog_otmtb_time",
                    "cog_otmta_error", "cog_otmtb_error") ~ 1 - pnorm(.data$`z-score`),
        # For other standardized scores, left tailed probabilities:
        .data$name %in% names(for_zscores) #c(unique(male_female$name), unique(ravlt_trials_m_sd$name))
          ~ pnorm(.data$`z-score`),
        # Special cases go here, such as all RAVLT
        .data$name %in% c("cog_flc_flu", "cog_digsym", "cog_ravlt_recog_acc") ~ c(1,1,1,1,2,5,9,16,25,37,50,63,75,84,91,95,98,99,99,99)[pmax(1, .data$SS+1)]/100,
        .default = NA
      ),
      # Get description. Again, this is done differently for different variables.
      Description = dplyr::case_when(
        .data$name == "cog_cdr_global" ~ c("Normal", "Very Mild", "Mild" ,"Moderate", "Severe")[
          findInterval(.data$Raw, c(0, 2.5, 4.5, 9.5, 16, 18.5, Inf))
        ],
        .data$name == "cog_gds15" ~ c("Minimal", "Mild", "Moderate", "Severe")[
          findInterval(.data$Raw, c(0, 5, 9, 12, 16, Inf))
        ],
        .data$name == "cog_moca_clock" & .data$Raw == 3 ~ "Normal",
        .data$name == "cog_moca_clock" & .data$Raw %in% c(0,1,2) ~ "Impaired",
        .default = c("Impaired",
                     "Borderline",
                     "Low Average",
                     "Average",
                     "High Average",
                     "Superior")[findInterval(.data$Percentile,
                                              c(0, 0.03, 0.10, 0.25, 0.76, 0.92, Inf))]
      )
    ) |>
    dplyr::mutate(
      Percentile = .data$Percentile*100
    ) |>
    dplyr::select(
      .data$group,
      .data$labels,
      .data$name,
      .data$Raw,
      .data$`z-score`,
      .data$SS,
      .data$Percentile,
      .data$Description
    ) |>
    dplyr::arrange(.data$labels)

  for_main_table |>
    gt::gt(
      rowname_col = "labels",
      groupname_col = "group"
    ) |>
    gt::cols_hide(.data$name) |>
    gt::fmt_number(
      columns = .data$`z-score`
    ) |>
    gt::fmt_number(
      columns = .data$Raw,
      rows = !stringr::str_detect(.data$labels, "CDR Global"),
      decimals = 0
    ) |>
    gt::tab_stub_indent(
      rows = tidyselect::everything(),
      indent = 5
    ) |>
    gt::tab_spanner(
      label = "Standardized",
      columns = c("z-score", "SS")
    ) |>
    my_gt_plt_bar_pct(
      .data$Percentile,
      scaled = T,
      labels = T,
      height = bar_height
    ) |>
    gt::sub_missing() |>
    gt::cols_align(
      align = "left",
      columns = .data$labels
    ) |>
    gt::tab_style(
      style = gt::cell_text(align = 'center'),
      locations = gt::cells_body(
        columns = .data$Percentile
      )
    ) |>
    gt::tab_style(
      style = gt::cell_fill("red"),
      locations = gt::cells_body(
        columns = .data$Description,
        rows = .data$Description == "Impaired"
      )
    ) |>
    gt::tab_style(
      style = gt::cell_fill("darkorange"),
      locations = gt::cells_body(
        columns = .data$Description,
        rows = .data$Description == "Borderline"
      )
    ) |>
    gt::tab_style(
      style = gt::cell_fill("orange"),
      locations = gt::cells_body(
        columns = .data$Description,
        rows = .data$Description == "Low Average"
      )
    ) |>
    gt::tab_style(
      style = gt::cell_fill("yellow"),
      locations = gt::cells_body(
        columns = .data$Description,
        rows = .data$Description == "Average"
      )
    ) |>
    gt::tab_style(
      style = gt::cell_fill("lightgreen"),
      locations = gt::cells_body(
        columns = .data$Description,
        rows = .data$Description == "High Average"
      )
    ) |>
    gt::tab_style(
      style = gt::cell_fill("green"),
      locations = gt::cells_body(
        columns = .data$Description,
        rows = .data$Description == "Superior"
      )
    ) |>
    gt::tab_options(
      column_labels.font.weight = "bold",
      row_group.font.weight = "bold"
    )
}

