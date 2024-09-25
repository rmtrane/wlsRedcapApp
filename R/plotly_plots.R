#' Create base plotly figure
#'
#' Create a base plot where traces can later be added.
#'
#' @param dat data object to get ranges for axes from
#' @param cog_vars vector of variables to infer ranges from **without prefix**
#' @param fill_values a named vector of length seven with hex color values to use for categories
#'   "Impaired", "Borderline", "Low Average", "Average", "High Average", "Superior", "Very Superior"
#' @param shade_descriptions logical; should ranges for descriptions be shaded?
#' @param fill_alpha opacity value for shaded areas
#' @param source passed to `plotly::plot_ly`
#'
#' @keywords internal
#'
#' @export
base_plot_z_scores <- function(
    dat,
    cog_vars = c(
      "cog_tmta_time",
      "cog_otmta_time",
      "cog_otmta_error",
      "cog_nsf_total",
      "cog_nsf_span",
      "cog_nsb_total",
      "cog_nsb_span"
    ),
    fill_values = setNames(
      RColorBrewer::brewer.pal(n = 7, "RdYlGn"),
      nm = c("Impaired", "Borderline", "Low Average", "Average", "High Average", "Superior", "Very Superior")
    ),
    shade_descriptions = T,
    fill_alpha = 0.2,
    source = "A"
) {
  if (rlang::is_missing(cog_vars) | is.null(cog_vars)) {
      most_extreme_val <- 2.5
  } else {
    if (sum(paste("standardized", cog_vars, sep = "_") %in% colnames(dat)) == 0) {
      most_extreme_val <- 2.5

      # p <- plot_ly(
      #   x = 0,
      #   y = 0,
      #   size = 12,
      #   text = "No values found.",
      #   type = "scatter",
      #   mode = "text"
      # ) |>
      #   layout(
      #     xaxis = F,
      #     yaxis = F
      #   ) |>
      #   config(
      #     staticPlot = T
      #   )

    } else {
      most_extreme_val <- dat |>
        dplyr::summarize(
          dplyr::across(tidyselect::any_of(paste("standardized", cog_vars, sep = "_")), \(x) max(abs(x), na.rm = T))
        ) |> unlist() |> max()
    }
  }

  # fill_values <- c(
  #   "Superior" = "green",
  #   "High Average" = "lightgreen",
  #   "Average" = "yellow",
  #   "Low Average" = "orange",
  #   "Borderline" = "darkorange",
  #   "Impaired" = "red"
  # )

  ## Get min and max values for y-axis
  y_min <- min(-2.5, -most_extreme_val)
  y_max <- max( 2.5,  most_extreme_val)

  z_scores_from_percentiles <- qnorm(c(0.03, 0.10, 0.25, 0.76, 0.92, 0.97))

  tiles <- tibble::tibble(
    ymin = c(y_min, z_scores_from_percentiles),
    ymax = c(z_scores_from_percentiles, y_max),
    descrs = c("Impaired",
               "Borderline",
               "Low Average",
               "Average",
               "High Average",
               "Superior",
               "Very Superior")
  )



  ## Update fill_values with alpha


  # do.call(grDevices::rgb, as.list(c(, alpha = fill_alpha)))

  # tiles <- tibble::tibble(
  #   type = "rect",
  #   line = list(width = 0),
  #   fillcolor = apply(
  #     grDevices::col2rgb(fill_values)/255,
  #     2,
  #     FUN = \(x) {
  #       do.call(grDevices::rgb, c(as.list(x), list(alpha = fill_alpha)))
  #     }
  #   ),
  #   y0 = c(y_min, z_scores_from_percentiles),
  #   y1 = c(z_scores_from_percentiles, y_max),
  #   x0 = date_range(dat$cog_test_date)[1],
  #   x1 = date_range(dat$cog_test_date)[2],
  #   name = c("Impaired",
  #            "Borderline",
  #            "Low Average",
  #            "Average",
  #            "High Average",
  #            "Superior",
  #            "Very Superior")
  # )

  p <- plotly::plot_ly(
    x = date_range(dat$cog_test_date),
    showlegend = FALSE,
    hoverinfo = "none",
    source = source
  ) |>
    plotly::layout(
      xaxis = list(
        title = "Date of Test",
        range = date_range(dat$cog_test_date)
      ),
      yaxis = list(
        title = "z-score",
        range = list(-2.5, 2.5),
        showgrid = F
      ),
      legend = list(
        orientation = "h"
      )
    )


  # p |>
  #   add_segments(
  #     data = head(tiles, -1),
  #     x = ~x0,
  #     xend = ~x1,
  #     y = ~y1,
  #     yend = ~y1,
  #     # linetype = I("dash"),
  #     line = list(
  #       color = "grey",
  #       dash = "dash"
  #     )
  #   ) |>
  #   layout(
  #     shapes = unname(lapply(split(tiles, tiles$name), as.list))
  #   )

  ## Add horizontal lines and fill colors according to descriptions
  for (i in seq_along(fill_values)) { #seq_along(z_scores_from_percentiles)) {
    p <- p |>
      ## horizontal lines
      plotly::add_trace(
        y = z_scores_from_percentiles[min(i, length(z_scores_from_percentiles))],
        type = "scatter",
        color = I(grDevices::rgb(0,0,0,0.3)),
        alpha = I(as.numeric(i != length(fill_values))),
        mode = "lines",
        linetype = I("dashed"),
        line = list(
          width = 1
        ),
        hoverinfo = "none"
      ) |>
      ## fill
      plotly::add_trace(
        y = c(y_min, z_scores_from_percentiles[-length(z_scores_from_percentiles)], y_max)[i],
        type = "scatter",
        mode = "lines",
        fill = "tonexty",
        fillcolor = do.call(grDevices::rgb, as.list(c(grDevices::col2rgb(rev(fill_values)[i])[,1]/255, alpha = fill_alpha))),
        name = rev(names(fill_values))[i],
        visible = shade_descriptions,
        line = list(
          width = 0
        ),
        hoverinfo = "none"
      )

  }

  p

}

#' @keywords internal
date_range <- function(dates) {
  c(lubridate::floor_date(min(dates, na.rm = T) - months(3), unit = "quarter"),
    lubridate::ceiling_date(max(dates, na.rm = T) + months(3), unit = "quarter"))
}
