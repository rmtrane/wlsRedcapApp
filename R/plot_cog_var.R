#' Function to Plot Cognitive Variables over Time
#'
#' @param dat Data to use
#' @param var_to_plot variable to plot **without prefix**
#' @param type Type of score to plot. One of "raw", "standardized", or "ss",
#'   depending on `var_to_plot`
#' @param shade_descriptions logical. Default: FALSE. If TRUE, the figure will be shaded
#'   according to the description column of the main table.
#' @param trim default: Inf. Value used to trim scores. Any score with
#'   abs(score) > trim will be removed.
#'
#' @export
plot_cog_var <- function(
    dat,
    var_to_plot = "cog_flc_flu",
    type = c("raw", "standardized", "ss"),
    shade_descriptions = F,
    trim = Inf
) {

  fill_values <- c(
    "Superior" = "green",
    "High Average" = "lightgreen",
    "Average" = "yellow",
    "Low Average" = "orange",
    "Borderline" = "darkorange",
    "Impaired" = "red"
  )

  if (any(stringr::str_detect(var_to_plot, c("time", "error"))))
    fill_values <- rev(fill_values)

  if (is.null(shade_descriptions))
    shade_descriptions <- F

  # If we cannot shade by descriptions, print message and set to F
  if (shade_descriptions) {
    if (!var_to_plot %in% c(names(for_zscores),
                            "cog_flc_flu", "cog_digsym", "cog_ravlt_recog_acc",
                            "cog_cdr_global", "cog_gds15", "cog_moca_clock")) {
      message(glue::glue("No descriptions available for '{var_to_plot}'. shade_descriptions set to FALSE"))
      shade_descriptions <- FALSE
    }

    if (type != "standardized" & var_to_plot %in% c(names(for_zscores))) {
      message(glue::glue("type must be 'standardized' to shade by description for {var_to_plot}. shade_descriptions set to FALSE"))
      shade_descriptions <- FALSE
    }

    if (type != "ss" & var_to_plot %in% c("cog_flc_flu", "cog_digsym", "cog_ravlt_recog_acc")) {
      message(glue::glue("type must be 'ss' to shade by description for {var_to_plot}. shade_descriptions set to FALSE"))
      shade_descriptions <- FALSE
    }

    if (type != "raw" & var_to_plot %in% c("cog_cdr_global", "cog_gds15", "cog_moca_clock")) {
      message(glue::glue("type must be 'raw' to shade by description for {var_to_plot}. shade_descriptions set to FALSE"))
      shade_descriptions <- FALSE
    }
  }

  # y-limits
  if (type == "raw") {
    ylim_min <- 0

    ylim_max <- dplyr::case_match(
      var_to_plot,
      "cog_cdr_global" ~ 18,
      "cog_moca" ~ 30,
      "cog_moca_blind" ~ 22,
      "cog_ticsm" ~ 50,
      c("cog_nsf_total", "cog_nsb_total") ~ 14,
      "cog_nsf_span" ~ 9,
      "cog_nsb_span" ~ 8,
      "cog_mint_tot" ~ 32,
      c("cog_benson_delay", "cog_benson_copy") ~ 16,
      c("cog_craft_imm_ver", "cog_craft_delay_verb") ~ 44,
      c("cog_craft_imm_par", "cog_craft_delay_par") ~ 25,
      "cog_ravlt_a1_a5_total" ~ 75,
      c("cog_ravlt_b1", "cog_ravlt_a6", "cog_ravlt_a7", "cog_gds15") ~ 15,
      .default = NA
    )
  }

  if (paste(type, var_to_plot, sep = "_") %in% colnames(dat))
    var_to_plot <- paste(type, var_to_plot, sep = "_")

  dat <- dat |>
    dplyr::filter(
      abs(.data[[var_to_plot]]) < trim | is.na(.data[[var_to_plot]])
    )

  out_plot <- dat |>
    ggplot2::ggplot(ggplot2::aes(x = as.character(.data$cog_test_date),
                                 y = !!rlang::sym(var_to_plot))) +
    ggplot2::labs(
      x = "Test Date",
      y = dplyr::case_match(
        type,
        "raw" ~ "Raw Scores",
        "standardized" ~ "z-scores",
        "ss" ~ "SS",
        .default = NA
      )
    ) +
    ggplot2::theme_bw()

  if (type == "raw" & !shade_descriptions) {
    out_plot <- out_plot +
      ggplot2::scale_y_continuous(
        limits = c(ylim_min, ylim_max+1)
      )
  }

  if (var_to_plot == "cog_cdr_global" & shade_descriptions) {

    tiles <- tibble::tibble(
      y = seq(0, 18, by = 0.5)
    ) |>
      dplyr::mutate(
        descrs = get_descriptions(.data$y, cog_var = "cog_cdr_global")
      ) |>
      dplyr::summarize(
        ymin = pmax(min(.data$y) - 0.25, 0),
        ymax = max(.data$y) + 0.25,
        .by = "descrs"
      )

    fill_values <- c(
      "Normal" = "green",
      "Very Mild" = "lightgreen",
      "Mild" = "yellow",
      "Moderate" = "orange",
      "Severe" = "red"
    )
  }

  if (var_to_plot == "cog_moca_clock" & shade_descriptions) {

    tiles <- tibble::tibble(
      y = seq(0, 3, by = 0.5)
    ) |>
      dplyr::mutate(
        descrs = get_descriptions(.data$y, cog_var = "cog_moca_clock")
      ) |>
      dplyr::summarize(
        ymin = pmax(min(.data$y) - 0.25, 0),
        ymax = max(.data$y) + 0.25,
        .by = "descrs"
      )

    fill_values <- c(
      "Impaired" = "red",
      "Normal" = "green"
    )
  }

  if (var_to_plot == "cog_gds15" & shade_descriptions) {
    tiles <- tibble::tibble(
      ys = 0:ylim_max
    ) |>
      dplyr::mutate(
        descrs = get_descriptions(raw = .data$ys, cog_var = var_to_plot)
      ) |>
      dplyr::summarise(
        ymin = pmax(min(.data$ys) - 0.5, 0),
        ymax = max(.data$ys) + 0.5,
        .by = "descrs"
      )

    fill_values <- c(
      "Severe" = "deepskyblue",
      "Moderate" = "lightskyblue",
      "Mild" = "lightblue",
      "Minimal" = "white"
    )
  }

  if (type == 'standardized') {

    most_extreme_val <- max(abs(dat[[var_to_plot]]), na.rm = T)*1.025

    y_min <- min(-2.5, -most_extreme_val)
    y_max <- max( 2.5,  most_extreme_val)


    if (shade_descriptions) {

      z_scores_for_plot <- seq(y_min, y_max, by = 0.01)

      tiles <- tibble::tibble(
        ys = seq(y_min, y_max, by = 0.005)
      ) |>
        dplyr::mutate(
          descrs = get_descriptions(raw = NA, z_score = .data$ys, cog_var = stringr::str_remove(var_to_plot, "standardized_"))
        ) |>
        dplyr::summarize(
          ymin = min(.data$ys),
          ymax = max(.data$ys),
          .by = "descrs"
        )
    }


  }

  if (type == "ss" & shade_descriptions) {
    tiles <- tibble::tibble(
      ys = 0:19
    ) |>
      dplyr::mutate(
        descrs = get_descriptions(ss = .data$ys, cog_var = stringr::str_remove(var_to_plot, "ss_"))
      ) |>
      dplyr::summarize(
        ymin = pmax(min(.data$ys) - 0.5, 0),
        ymax = max(.data$ys) + 0.5,
        .by = "descrs"
      )
  }

  if (shade_descriptions) {
    out_plot <- out_plot +
      ggplot2::geom_rect(
        data = tiles,
        inherit.aes = F,
        ggplot2::aes(
          xmin = -Inf,
          xmax = Inf,
          ymin = .data$ymin,
          ymax = .data$ymax,
          fill = .data$descrs
        ),
        alpha = 0.2
      ) +
      ggplot2::geom_hline(
        yintercept = with(tiles, (ymin + lag(ymax))/2)[-1],
        linetype = "dashed",
        alpha = 0.4
      ) +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(0, 0)
      ) +
      ggplot2::scale_fill_manual(
        values = fill_values,
        breaks = names(fill_values)
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(title = NULL)
      ) +
      ggplot2::theme(
        legend.key = ggplot2::element_rect(color = "black", linewidth = 0.2)
      )
  }


  if (length(na.omit(dat[[var_to_plot]])) > 1) {
    out_plot <- out_plot + ggplot2::geom_line(ggplot2::aes(group = .data$cog_studyid))
  }

  out_plot +
    ggplot2::geom_point(na.rm = T)
}
