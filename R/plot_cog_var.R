#' Function to Plot Cognitive Variables over Time
#'
#' @param dat Data to use
#' @param var_to_plot variable to plot **without prefix**
#' @param type Type of score to plot. One of "raw", "standardized", or "ss",
#'   depending on `var_to_plot`
#'
#' @export
plot_cog_var <- function(
    dat,
    # studyid = "102038g",
    var_to_plot = "cog_flc_flu",
    type = c("raw", "standardized", "ss"),
    shade_descriptions = F,
    trim = Inf
) {

  if (is.null(shade_descriptions))
    shade_descriptions <- F

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
      abs(.data[[var_to_plot]]) < trim
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

  #if (var_to_plot == c("cog_ticsm"))
  if (type == "raw") {
    out_plot <- out_plot +
      ggplot2::scale_y_continuous(
        limits = c(ylim_min, ylim_max+1)
      )
  }

  fills <- c("red", "darkorange", "orange", "yellow", "lightgreen", "green")

  if (var_to_plot == "cog_cdr_global" & shade_descriptions) {

    for_tiles <- tibble::tibble(
      y = seq(0, 18, by = 0.5),
      descr = c("Normal", "Very Mild", "Mild" ,"Moderate", "Severe")[
        findInterval(y, c(0, 2.5, 4.5, 9.5, 16, 18.5, Inf))
      ]
    ) |>
      dplyr::summarize(
        ymin = min(y) - 0.25,
        ymax = max(y) + 0.25,
        .by = descr
      ) |>
      dplyr::mutate(
        fill = dplyr::case_match(
          descr,
          "Normal" ~ "green",
          "Very Mild" ~ "lightgreen",
          "Mild" ~ "yellow",
          "Moderate" ~ "orange",
          "Severe" ~ "red"
        )
      )

    out_plot <- out_plot +
      ggplot2::geom_rect(
        data = for_tiles,
        inherit.aes = F,
        ggplot2::aes(
          xmin = -Inf,
          xmax = Inf,
          ymin = ymin,
          ymax = ymax,
          fill = I(fill)
        ),
        alpha = 0.2
      ) +
      ggplot2::scale_y_continuous(limits = c(NA, NA))
  }

  if (type == 'standardized') {

    most_extreme_val <- max(abs(dat[[var_to_plot]]), na.rm = T)*1.025

    y_min <- min(-2.5, -most_extreme_val)
    y_max <- max( 2.5,  most_extreme_val)


    if (shade_descriptions) {

      if (var_to_plot %in% c("standardized_cog_tmta_time", "standardized_cog_tmtb_time",
                             "standardized_cog_otmta_time", "standardized_cog_otmtb_time",
                             "standardized_cog_otmta_error", "standardized_cog_otmtb_error")) {
        fills <- rev(fills)
      }

      z_scores_from_percentiles <- qnorm(c(0.03, 0.10, 0.25, 0.76, 0.92))


      tiles <- tibble::tibble(
        xmin = -Inf,
        xmax = Inf,
        ymin = c(y_min, z_scores_from_percentiles),
        ymax = c(z_scores_from_percentiles, y_max),
        fills = fills
      )

      out_plot <- out_plot +
        ggplot2::geom_rect(
          data = tiles,
          ggplot2::aes(
            xmin = xmin,
            xmax = xmax,
            ymin = ymin,
            ymax = ymax,
            fill = I(fills)
          ),
          alpha = 0.2,
          inherit.aes = F
        )
    }

    out_plot <- out_plot +
      ggplot2::scale_y_continuous(
        limits = c(y_min, y_max),
        expand = ggplot2::expansion(0, 0)
      )
  }

  out_plot +
    ggplot2::geom_point()
}
