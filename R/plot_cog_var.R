#' Function to Plot Cognitive Variables over Time
#'
#' @param dat Data to use
#' @param studyid ID of participant to plot
#' @param var_to_plot variable to plot **without prefix**
#' @param type Type of score to plot. One of "raw", "standardized", or "ss",
#'   depending on `var_to_plot`
#'
#' @export
plot_cog_var <- function(
    dat,
    studyid = "102038g",
    var_to_plot = "cog_flc_flu",
    type = c("raw", "standardized", "ss")
) {

  # raw_or_standard <- "raw"

  if (paste(type, var_to_plot, sep = "_") %in% colnames(dat))
    var_to_plot <- paste(type, var_to_plot, sep = "_")

  dat |>
    dplyr::filter(.data$cog_studyid == studyid) |>
    ggplot2::ggplot(ggplot2::aes(x = as.character(.data$cog_test_date),
                                 y = !!rlang::sym(var_to_plot))) +
      ggplot2::geom_point() +
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
}
