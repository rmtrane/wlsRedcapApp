#' Create NACC T-Cog Neuropsychological Assessment Summary Table
#'
#' This function create a summary table of measures from the NACC T-Cog Neuropsychological
#' Assessment.
#'
#' @param dat Data to use. Must have exactly one row (data from one participant and one visit).
#' @param bar_height In pixels. Height of the percentile bars. Default: 16
#'
#' @returns An object of class `gt::gt_tbl`
#'
#' @examples
#' main_table(
#'   dat = demo_data[1,]
#' )
#'
#' @export
main_table <- function(
    dat,
    bar_height = 16
) {

  stopifnot("Data provided must have exactly one row" = nrow(dat) == 1)

  stopifnot(
    "Data must contain a column labelled 'cog_studyid' giving unique IDs for participants" =
      "cog_studyid" %in% colnames(dat)
  )

  pot_cog_cols <- c(
    "cog_studyid",
    "cog_cdr_sob",
    "cog_tmta_err",
    "cog_tmta_lines",
    "cog_benson_delay",
    "cog_benson_recog",
    "cog_ravlt_a1",
    "cog_ravlt_a2",
    "cog_ravlt_a3",
    "cog_ravlt_a4",
    "cog_ravlt_a5",
    "cog_ravlt_recog_tp",
    "cog_ravlt_recog_tn",
    "cog_tmtb_err",
    "cog_tmtb_lines",
    "cog_cdr_global",
    "cog_moca",
    "cog_moca_blind",
    "cog_ticsm",
    "cog_tmta_time",
    "cog_otmta_time",
    "cog_otmta_error",
    "cog_nsf_total",
    "cog_nsf_span",
    "cog_nsb_total",
    "cog_nsb_span",
    "cog_digsym",
    "cog_mint_tot",
    "cog_animal_flu",
    "cog_veg_flu",
    "cog_fl_flu",
    "cog_flc_flu",
    "cog_f_flu",
    "cog_l_flu",
    "cog_benson_copy",
    "cog_benson_delay",
    "cog_craft_imm_ver",
    "cog_craft_imm_par",
    "cog_craft_delay_verb",
    "cog_craft_delay_par",
    "cog_ravlt_a1_a5_total",
    "cog_ravlt_b1",
    "cog_ravlt_a6",
    "cog_ravlt_a7",
    "cog_ravlt_recog_acc",
    "cog_tmtb_time",
    "cog_otmtb_time",
    "cog_otmtb_error",
    "cog_moca_clock",
    "cog_gds15"
  )

  col_avails <- colnames(dat)

  if (! any(c(pot_cog_cols, paste("raw", pot_cog_cols, sep = "_"), paste("standardized", pot_cog_cols, sep = "_"), paste("ss", pot_cog_cols, sep = "_")) %in% col_avails))
    stop("No columns recognized as cognitive scores.")



  ## Create labels to be used. First, if any of the necessary variables are
  ## missing, set to NA
  vars_needed <- c(
    "cog_cdr_sob",
    "cog_tmta_err",
    "cog_tmta_lines",
    "raw_cog_benson_delay",
    "cog_benson_recog",
    "cog_ravlt_a1",
    "cog_ravlt_a2",
    "cog_ravlt_a3",
    "cog_ravlt_a4",
    "cog_ravlt_a5",
    "cog_ravlt_recog_tp",
    "cog_ravlt_recog_tn",
    "cog_tmtb_err",
    "cog_tmtb_lines",
    "cog_cdr_global",
    "cog_moca",
    "cog_moca_blind",
    "cog_ticsm",
    "cog_tmta_time",
    "cog_otmta_time",
    "cog_otmta_error",
    "cog_nsf_total",
    "cog_nsf_span",
    "cog_nsb_total",
    "cog_nsb_span",
    "cog_digsym",
    "cog_mint_tot",
    "cog_animal_flu",
    "cog_veg_flu",
    "cog_fl_flu",
    "cog_flc_flu",
    "cog_f_flu",
    "cog_l_flu",
    "cog_benson_copy",
    "cog_benson_delay",
    "cog_craft_imm_ver",
    "cog_craft_imm_par",
    "cog_craft_delay_verb",
    "cog_craft_delay_par",
    "cog_ravlt_a1_a5_total",
    "cog_ravlt_b1",
    "cog_ravlt_a6",
    "cog_ravlt_a7",
    "cog_ravlt_recog_acc",
    "cog_tmtb_time",
    "cog_otmtb_time",
    "cog_otmtb_error",
    "cog_moca_clock",
    "cog_gds15"
  )

  dat[vars_needed[!vars_needed %in% colnames(dat)]] <- NA

  cur_labels <- with(
    dat,
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
      cog_tmtb_lines,
      cog_craft_verb_retain = floor(raw_cog_craft_delay_verb / raw_cog_craft_imm_ver * 100),
      cog_craft_par_retain = floor(raw_cog_craft_delay_par / raw_cog_craft_imm_par * 100)
    )
  )

  # Check if any of the scores necessary are in dat, but was not
  # adjusted/renamed
  for_main_table <- dat |>
    dplyr::select(
      # Identifier
      "cog_studyid",
      # Get all scores
      tidyselect::matches(names(cur_labels))
    ) |>
    # Rename the scores that we do not standardize.
    dplyr::rename_with(
      .fn = \(x) paste0("raw_", x),
      .cols = tidyselect::any_of(
        c(
          "cog_cdr_global",
          "cog_moca_blind",
          "cog_moca_clock",
          "cog_ticsm",
          "cog_gds15"
        )
      )
    ) |>
    ## Create long data set with one row per variable, and up to three columns
    ## per variable: raw, standardized, and ss.
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
      .after = "name"
    ) |>
    # Remove rows with no raw observations
    dplyr::filter(!is.na(.data$Raw))


  for_main_table_2 <- for_main_table |>
    dplyr::mutate(
      # Get percentiles. This is done differently for different variables.
      Percentile = get_percentiles(`z-score`, SS, name),
      # Percentile = dplyr::case_when(
      #   # For time and number of errors, we want right-tailed probabilities
      #   .data$name %in% c("cog_tmta_time", "cog_tmtb_time",
      #                     "cog_otmta_time", "cog_otmtb_time",
      #                     "cog_otmta_error", "cog_otmtb_error") ~
      #     1 - pnorm(.data$`z-score`),
      #   # For other standardized scores, left tailed probabilities:
      #   .data$name %in% names(for_zscores) ~ pnorm(.data$`z-score`),
      #   # Percentiles from SS
      #   .data$name %in% c("cog_flc_flu", "cog_digsym", "cog_ravlt_recog_acc") ~
      #     c(1,1,1,1,2,5,9,16,25,37,50,63,75,84,91,95,98,99,99,99)[pmax(1, .data$SS+1)]/100,
      #   .default = NA
      # ),
      # Get description. Again, this is done differently for different variables.
      Description = get_descriptions(Raw, `z-score`, SS, name)
      # Description = dplyr::case_when(
      #   .data$name == "cog_cdr_global" ~ c("Normal", "Very Mild", "Mild" ,"Moderate", "Severe")[
      #     findInterval(.data$Raw, c(0, 2.5, 4.5, 9.5, 16, 18.5, Inf))
      #   ],
      #   .data$name == "cog_gds15" ~ c("Minimal", "Mild", "Moderate", "Severe")[
      #     findInterval(.data$Raw, c(0, 5, 9, 12, 16, Inf))
      #   ],
      #   .data$name == "cog_moca_clock" & .data$Raw == 3 ~ "Normal",
      #   .data$name == "cog_moca_clock" & .data$Raw %in% c(0,1,2) ~ "Impaired",
      #   .default = c("Impaired",
      #                "Borderline",
      #                "Low Average",
      #                "Average",
      #                "High Average",
      #                "Superior")[findInterval(.data$Percentile,
      #                                         c(0, 0.03, 0.10, 0.25, 0.76, 0.92, Inf))]
      # )
    ) |>
    dplyr::mutate(
      across(
        c("Raw", "z-score", "SS", "Percentile"),
        \(x) dplyr::if_else(.data$Raw %in% c(995:999), NA, x)
      ),
      Percentile = .data$Percentile*100,
      Raw_suffix = dplyr::case_match(
        .data$name,
        "cog_moca" ~ "/30",
        "cog_moca_blind" ~ "/22",
        "cog_ticsm" ~ "/50",
        c("cog_tmta_time", "cog_tmtb_time", "cog_otmta_time", "cog_otmtb_time", "cog_moca_clock") ~ "&nbspsec",
        c("cog_nsf_total", "cog_nsb_total") ~ "/14",
        "cog_nsf_span" ~ "/9",
        "cog_nsb_span" ~ "/8",
        "cog_mint_tot" ~ "/32",
        "cog_benson_copy" ~ "/16",
        "cog_benson_delay" ~ "/16",
        c("cog_craft_imm_ver", "cog_craft_delay_verb") ~ "/44",
        c("cog_craft_imm_par", "cog_craft_delay_par") ~ "/25",
        "cog_ravlt_a1_a5_total" ~ "/75",
        c("cog_ravlt_b1", "cog_ravlt_a6", "cog_ravlt_a7", "cog_gds15") ~ "/15",
        .default = ""
      ),
      Raw_suffix = dplyr::if_else(is.na(.data$Raw), "", .data$Raw_suffix)
    ) |>
    dplyr::select(
      "group",
      "labels",
      "name",
      "Raw",
      "Raw_suffix",
      "z-score",
      "SS",
      "Percentile",
      "Description"
    ) |>
    dplyr::arrange("labels")

  for_main_table_2 |>
    gt::gt(
      rowname_col = "labels",
      groupname_col = "group"
    ) |>
    gt::cols_hide("name") |>
    ## Make parts of the table bold
    gt::tab_options(
      table.font.names = "Arial",
      column_labels.font.weight = "bold",
      row_group.font.weight = "bold"
    ) |>
    ## Some formatting
    gt::fmt(
      columns = "Raw",
      rows = .data$name == "cog_cdr_global",
      fns = \(x) sprintf("%.1f", x)
    ) |>
    gt::fmt(
      columns = "Raw_suffix",
      fns = gt::md
    ) |>
    gt::fmt_number(
      columns = "z-score"
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
      #.data$Percentile,
      "Percentile",
      scaled = T,
      labels = T,
      height = bar_height
    ) |>
    # gt::tab_options(
    #   table.font.names = "Arial"
    # ) |>
    gt::sub_missing() |>
    gt::cols_align(
      align = "left",
      columns = c("labels", "Raw_suffix")
    ) |>
    gt::cols_align(
      align = "right",
      columns = "Raw"
    ) |>
    gt::cols_align(
      align = "center",
      columns = "Percentile"
    ) |>
    ## Make Raw and Raw_suffix look pretty
    gt::cols_label(
      "Raw" = "Ra",
      "Raw_suffix" = "w"
    ) |>
    gt::tab_style(
      style = "padding-right:0px; padding-left:0px;",
      locations = gt::cells_column_labels(tidyselect::starts_with("Raw"))
    ) |>
    gt::tab_style(
      style = "padding-right:0px;",
      locations = gt::cells_body(
        columns = "Raw"
      )
    ) |>
    gt::tab_style(
      style = glue::glue("padding-left:0px; color: {rgb(0, 0, 0, 0.3)};"),
      locations = gt::cells_body(
        columns = "Raw_suffix",
        rows = .data$Raw_suffix != "&nbspsec"
      )
    ) |>
    gt::tab_style(
      style = "padding-left:0px;",
      locations = gt::cells_body(
        columns = "Raw_suffix",
        rows = .data$Raw_suffix == "&nbspsec"
      )
    ) |>
    ## Style description column
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
    )
}

