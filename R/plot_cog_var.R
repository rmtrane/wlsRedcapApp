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

  if (paste(type, var_to_plot, sep = "_") %in% colnames(dat))
    var_to_plot <- paste(type, var_to_plot, sep = "_")

  dat <- dat |>
    dplyr::filter(
      abs(.data[[var_to_plot]]) < trim
    )

  out_plot <- dat |>
    # dplyr::filter(.data$cog_studyid == studyid) |>
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

  if (var_to_plot == c("cog_ticsm"))
    out_plot <- out_plot + ggplot2::ylim(c(0, NA))

  if (type == 'standardized' & shade_descriptions) {

    most_extreme_val <- max(abs(dat[[var_to_plot]]), na.rm = T)

    y_min <- min(-2.5, -most_extreme_val)
    y_max <- max( 2.5,  most_extreme_val)


    fills <- c("red", "darkorange", "orange", "yellow", "lightgreen", "green")

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
      ) +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(0, 0)
      )

  }

  out_plot +
    ggplot2::geom_point()
}
