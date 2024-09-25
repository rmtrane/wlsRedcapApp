#' Shiny Module to Display NACC T-Cog Neuropsychological Assessment Summary Table
#'
#' @param id An ID string to match module UI and server UI
#' @param dat Data set to use
#' @param fill_values a named vector of length seven with hex color values to use for categories
#'   "Impaired", "Borderline", "Low Average", "Average", "High Average", "Superior", "Very Superior"
#' @param table_font_size Percentage font size to use. Used to scale table
#'
#' @rdname mainTableModule
#'
#' @export

mainTableUI <- function(id) {
  shiny::uiOutput(shiny::NS(id, "mainTable"))
}

#' @rdname mainTableModule
#' @export
mainTableServer <- function(
    id,
    dat,
    fill_values = setNames(
      RColorBrewer::brewer.pal(n = 7, "RdYlGn"),
      nm = c(
        "Impaired", # = "red"
        "Borderline", # = "darkorange",
        "Low Average", # = "orange",
        "Average", # = "yellow"
        "High Average", # = "",
        "Superior", # = "lightgreen",
        "Very Superior" # = "green",
      )
    ),
    table_font_size = 100
) {

  shiny::moduleServer(id, function(input, output, session) {

    # dat_for_table <- shiny::reactive({
    #   dplyr::filter(dat(), .data$cog_studyid == studyid, .data$cog_test_date == date)
    # })

    #shiny::observeEvent(nrow(dat_for_table()), {
    #  if (nrow(dat_for_table()) == 1) {
    shiny::observe({
      if (nrow(dat()) == 1) {
        output$mainTable <- gt::render_gt({
          main_table(
            # dat = dat_for_table(),
            dat = dat(),
            fill_values = fill_values,
            bar_height = 16*table_font_size/100
          ) |>
            gt::cols_width(
              .data$labels ~ gt::px(350),
              .data$Raw ~ gt::px(50),
              .data$`z-score` ~ gt::px(80),
              .data$SS ~ gt::px(40),
              .data$Description ~ gt::px(110)
            )  |>
            gt::tab_options(
              data_row.padding = gt::px(2),
              row_group.padding = gt::px(4),
              table.font.size = gt::pct(table_font_size)
            )
        })
      } else {
        print("Data for main_table must have one row")
      }
    })
  })
}


mainTableApp <- function(dat){# }, studyid = demo_data$cog_studyid[1], date = demo_data$cog_test_date[1]) {
  ui <- shiny::fluidPage(
    mainTableUI("main_table")
  )

  server <- function(input, output, session) {
    mainTableServer("main_table", dat = shiny::reactive(dat)) #, studyid = studyid, date = date)
  }

  shiny::shinyApp(ui, server)
}
