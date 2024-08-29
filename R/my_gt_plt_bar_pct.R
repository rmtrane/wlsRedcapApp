#' Updated from `gtExtras::gt_plt_bar_pct`
#'
#' This updates the `gtExtras::gt_plt_bar_pct` to handle missing values as
#' desired.
#'
#' @inheritParams gtExtras::gt_plt_bar_pct
#'
#' @keywords internal

my_gt_plt_bar_pct <- function(
    gt_object,
    column,
    height = 16,
    width = 100,
    fill = "purple",
    background = "#e1e1e1",
    scaled = FALSE,
    labels = FALSE,
    label_cutoff = 0.4,
    decimals = 1,
    font_style = "bold",
    font_size = "10px") {

  stopifnot(
    `'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` =
      "gt_tbl" %in% class(gt_object)
  )
  stopifnot(
    `label_cutoff must be a number between 0 and 1` =
      dplyr::between(label_cutoff, 0, 1)
  )

  stopifnot(`\`font_style\` argument must be "bold", "normal", or "italic"` = font_style %in%
    c("bold", "normal", "italic"))

  ## Get underlying data from gt_object
  all_cols <- gtExtras::gt_index(gt_object, column = {{ column }}, as_vector = FALSE)

  data_in <- all_cols |>
    dplyr::select({{ column }}) |>
    dplyr::pull()

  col_name <- all_cols |>
    dplyr::select({{ column }}) |>
    names()

  col_to_widen <- rlang::new_formula(col_name, gt::px(width))

  bar_plt_html <- function(xy) {
    if (length(na.omit(xy)) == 0) {
      max_x <- 0
    } else {
      max_x <- max(suppressWarnings(as.double(xy)), na.rm = TRUE)
    }

    bar <- lapply(data_in, function(x) {
      scaled_value <- if (isFALSE(scaled)) {
        x / max_x * 100
      } else {
        x
      }

      if (is.na(x)) {
        NA # "---" # "<div style='background:transparent;width:0%;height:{height}px;'><span>---</span></div>"
      } else {
        if (labels) {
          label_values <- if (scaled) {
            x
          } else {
            x / max_x * 100
          }
          label <- glue::glue("{round(label_values, decimals)}%")

          if (x < (label_cutoff * max_x)) {
            css_styles <- paste0(
              "background:", fill,
              ";", "width:", scaled_value, "%;", "height:",
              height, "px;", "display:flex;", "align-items:center;",
              "justify-content:center;", "color:", ideal_fgnd_color(background),
              ";", "font-weight:", font_style, ";", "font-size:",
              font_size, ";", "position:relative;"
            )
            span_styles <- paste0(
              "color:", ideal_fgnd_color(background),
              ";", "position:absolute;", "left:0%;", "margin-left:",
              scaled_value * width / 100, "px;", "font-weight:",
              font_style, ";", "font-size:", font_size,
              ";"
            )
            glue::glue("<div style='{css_styles}'>", "<span style='{span_styles}'>{label}</span></div>")
          } else {
            css_styles <- paste0(
              "background:", fill,
              ";", "width:", scaled_value, "%;", "height:",
              height, "px;", "display:flex;", "align-items:center;",
              "justify-content:flex-start;", "position:relative;"
            )
            span_styles <- paste0(
              "color:", ideal_fgnd_color(fill),
              ";", "position:absolute;", "left:0px;",
              "margin-left:5px;", "font-weight:", font_style,
              ";", "font-size:", font_size, ";"
            )
            glue::glue("<div style='{css_styles}'>", "<span style='{span_styles}'>{label}</span></div>")
          }
        } else {
          # if (!is.na(x)) {
          glue::glue("<div style='background:{fill};width:{scaled_value}%;height:{height}px;'></div>")
        }
      }
    })

    chart <- lapply(bar, function(bar) {
      if (is.na(bar)) {
        "&mdash;"
      } else {
        glue::glue("<div style='flex-grow:1;margin-left:8px;background:{background};'>{bar}</div>")
      }
    })
    chart
  }

  data_in

  quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }

  #quiet(
    gt_object |>
      gt::cols_width(col_to_widen) |>
      gt::text_transform(
        locations = gt::cells_body(columns = {{ column }}),
        fn = bar_plt_html
      ) |>
      gt::cols_align(
        align = "left",
        columns = {{ column }}
      )
  #  )
}
