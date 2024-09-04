#' Shiny Module to Plot Scores Across Visits
#'
#' This shiny module is meant for visualizing changes over time of a long list
#' of cognitive measures from the WLS ILIAD study.
#'
#' @param id An ID string to match module UI and module server
#' @param dat Data to use for the plots. Should contain a number of `cog_*`
#'  variables
#'
#' @examples
#' \dontrun{
#'   plotCogVarApp(demo_data[1,])
#' }
#'
#'
#' @rdname plotCogVarModule
#'
#' @export
plotCogVarUI <- function(id) {
  shiny::tagList(
    bslib::layout_column_wrap(
      shiny::selectizeInput(
        shiny::NS(id, "var_to_plot"),
        label = "Choose Variable to Plot",
        choices = NULL
      ),
      shiny::selectizeInput(
        shiny::NS(id, "raw_or_standard"),
        label = "Raw, z-score or SS",
        choices = NULL
      )
    ),
    shiny::plotOutput(shiny::NS(id, "plotCogVar"), width = "100%")# ,
    # shiny::uiOutput(shiny::NS(id, "shade_descriptions"))
  )
}

#' @rdname plotCogVarModule
#' @export
plotCogVarServer <- function(id, dat, trim = Inf) {

  shiny::moduleServer(id, function(input, output, session) {

    # cur_pt_dat <- shiny::reactiveVal()
    var_avail <- shiny::reactiveVal()

    cur_pt_dat <- dat |>
        dplyr::select(
          # Identifiers
          "cog_studyid", # :"cog_education",
          "cog_test_date",
          # Get all scores
          tidyselect::matches(names(cog_vars_labels))
        ) |>
        # Rename the scores that have not been adjusted. These should be
        # raw_(score)
        dplyr::rename_with(
          .fn = \(x) paste0("raw_", x),
          .cols = tidyselect::matches(paste0("^", names(cog_vars_labels)))
        ) |>
        tidyr::pivot_longer(
          cols = c(
            tidyselect::starts_with("raw"),
            tidyselect::starts_with("standardized"),
            tidyselect::starts_with("ss_")
          ),
          names_to = c(".value", "name"),
          names_pattern = c("(raw|standardized|ss)_(.+)")
        ) |>
        dplyr::filter(
          !is.na(.data$raw)
        )


    var_avail(
      unique(cur_pt_dat$name)
    )

    shiny::observeEvent(var_avail(), {
      shiny::updateSelectizeInput(
        session,
        inputId = "var_to_plot",
        choices = setNames(names(cog_vars_labels[var_avail()]),
                           nm = unname(cog_vars_labels[var_avail()]))
      )
    })

    scores_avail <- shiny::reactiveVal()

    shiny::observeEvent(input$var_to_plot, {
      scores_avail(
        cur_pt_dat |>
          dplyr::filter(.data$name == input$var_to_plot) |>
          dplyr::select(tidyselect::any_of(c("raw", "standardized", "ss"))) |>
          dplyr::select(tidyselect::where(\(x) sum(!is.na(x)) > 0)) |>
          names()
      )
    }, ignoreInit = T)

    shiny::observeEvent(scores_avail(), {
      shiny::updateSelectizeInput(
        session,
        inputId = "raw_or_standard",
        choices = setNames(
          stringr::str_to_lower(scores_avail()),
          dplyr::case_match(
            scores_avail(),
            "raw" ~ "Raw",
            "standardized" ~ "z-score",
            "ss" ~ "SS",
            .default = NA
          )
        )
      )
    }, ignoreInit = T)



    shiny::observeEvent(input$raw_or_standard, {
      if (input$raw_or_standard == "standardized") {
        output$shade_descriptions <- shiny::renderUI({
          shiny::checkboxInput(
            inputId = NS(id, "shade_descriptions"),
            label = "Shade according to descriptions?",
            value = FALSE
          )
        })
      } else {
        output$shade_descriptions <- renderUI(markdown(""))
      }
    })


    # observe({
    #   print(any(c(input$var_to_plot, paste(input$raw_or_standard, input$var_to_plot, sep = "_")) %in% colnames(dat)))
    #   print(c(input$var_to_plot, paste(input$raw_or_standard, input$var_to_plot, sep = "_")))
    # })

    observe({
    #   input$raw_or_standard
    #   input$var_to_plot
    #   input$shade_descriptions
    # },
    # {

      print(c(input$var_to_plot, paste(input$raw_or_standard, input$var_to_plot, sep = "_")))

      print(any(c(input$var_to_plot, paste(input$raw_or_standard, input$var_to_plot, sep = "_")) %in% colnames(dat)))

      if (any(c(input$var_to_plot, paste(input$raw_or_standard, input$var_to_plot, sep = "_")) %in% colnames(dat))) {

        output$plotCogVar <- shiny::renderPlot({
          plot_cog_var(
            dat,
            var_to_plot = input$var_to_plot,
            type = input$raw_or_standard,
            shade_descriptions = F, # input$shade_descriptions,
            trim = trim
          ) +
            ggplot2::labs(
              title = cog_vars_labels[input$var_to_plot]
            )
        })
      }
    })

  })
}

#' Shiny App Wrapping the variable plotting module
#'
#' @param dat Data from REDCap to use
#'
#' @export
plotCogVarApp <- function(dat, trim = Inf) {
  ui <- shiny::fluidPage(
    bslib::card(plotCogVarUI("plot_cog_var"))
  )

  server <- function(input, output, session) {
    plotCogVarServer("plot_cog_var", dat, trim)
  }

  shiny::shinyApp(ui, server)
}
