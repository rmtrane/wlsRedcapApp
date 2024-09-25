#' Shiny Module to Display Longitudinal Trends in Table
#'
#' Table version of the plotCogVarModule.
#'
#' @param id string to tie UI to Server
#' @param dat data to use for table. Should only have data on one subject
#' @param fill_values a named vector of length seven with hex color values to use for categories
#'   "Impaired", "Borderline", "Low Average", "Average", "High Average", "Superior", "Very Superior"
#' @param table_font_size Percentage font size to use. Used to scale table
#'
#' @rdname longTableModule
#'
#' @export
longTableUI <- function(id) {
  htmltools::tagList(
    gt::gt_output(shiny::NS(id, "long_table"))
  )
}

#' @rdname longTableModule
#' @export
longTableServer <- function(
    id,
    dat,
    fill_values = setNames(
      RColorBrewer::brewer.pal(n = 7, "RdYlGn"),
      nm = c("Impaired", "Borderline", "Low Average", "Average", "High Average", "Superior", "Very Superior")
    ),
    table_font_size = 100
) {
  shiny::moduleServer(id, function(input, output, session) {
    output$long_table <- gt::render_gt({
      longitudinal_table(dat(), fill_values = fill_values) |>
        gt::tab_options(
          data_row.padding = gt::px(2),
          row_group.padding = gt::px(4),
          table.font.size = gt::pct(table_font_size)
        )
    })
  })
}

longTableApp <- function(dat) {
  ui <- bslib::page_fillable(
    longTableUI("long_table")
  )

  server <- function(input, output, session) {
    longTableServer("long_table", shiny::reactive(dat))
  }

  shiny::shinyApp(ui, server)
}
