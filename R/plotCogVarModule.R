#' Shiny Module to Plot Scores Across Visits
#'
#' This shiny module is meant for visualizing changes over time of a long list
#' of cognitive measures from the WLS ILIAD study.
#'
#' @param id An ID string to match module UI and module server
#' @param dat Data to use for the plots. Should contain a number of `cog_*`
#'  variables
#'
#' @inheritParams plot_cog_var
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
      shiny::radioButtons(
        inputId = shiny::NS(id, "all_or_one_plot"),
        label = "Show all plots, or choose one?",
        choices = c("Choose one" = "one", "Show all" = "all")
      ),
      shiny::uiOutput(shiny::NS(id, "plot_selection")),
      shiny::selectizeInput(
        shiny::NS(id, "raw_or_standard"),
        label = "Raw, z-score or SS",
        choices = NULL
      ),
      #shiny::uiOutput(shiny::NS(id, "shade_descriptions"))
      shiny::checkboxInput(
        inputId = shiny::NS(id, "shade_descriptions"),
        label = "Shade according to descriptions?",
        value = FALSE
      )
    ),
    shiny::uiOutput(shiny::NS(id, "plotsUI"))
    #shiny::plotOutput(shiny::NS(id, "plotCogVar"), width = "100%")

  )
}

#' @rdname plotCogVarModule
#'
#' @inheritParams plot_cog_var
#'
#' @export
plotCogVarServer <- function(id, dat, trim = Inf) {

  shiny::moduleServer(id, function(input, output, session) {


    var_avail <- shiny::reactiveVal()
    for_var_and_scores_avail <- shiny::reactiveVal()

    for_var_and_scores_avail(
      dat() |>
        dplyr::select(
          # Identifiers
          "cog_studyid",
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
    )


    var_avail(
      unique(for_var_and_scores_avail()$name)
    )

    shiny::observeEvent({
      var_avail()
      input$all_or_one_plot
    }, {
      if (input$all_or_one_plot == "one") {
        output$plot_selection <- shiny::renderUI({
          shiny::selectizeInput(
            inputId = shiny::NS(id, "var_to_plot"),
            label = "Choose Variable to Plot",
            choices = setNames(names(cog_vars_labels[var_avail()]),
                               nm = unname(cog_vars_labels[var_avail()]))
          )
        })
      } else {
        output$plot_selection <- NULL # renderUI()
      }
    })

    scores_avail <- shiny::reactiveVal()

    shiny::observeEvent({
      input$var_to_plot
      for_var_and_scores_avail()
    }, {
      scores_avail(
        for_var_and_scores_avail() |>
          dplyr::filter(.data$name == input$var_to_plot) |>
          dplyr::select(tidyselect::any_of(c("raw", "standardized", "ss"))) |>
          dplyr::select(tidyselect::where(\(x) sum(!is.na(x)) > 0)) |>
          names()
      )
    }, ignoreInit = T)

    shiny::observeEvent({
      input$all_or_one_plot
      scores_avail()
    }, {

      if (input$all_or_one_plot == "one") {
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
      } else {
        shiny::updateSelectizeInput(
          session,
          inputId = "raw_or_standard",
          choices = c(
            "Raw" ="raw",
            "z-score" = "standardized",
            "SS" = "ss"
          )
        )
      }
    }, ignoreInit = T)


    # shiny::observeEvent(input$raw_or_standard, {
    #   if (input$raw_or_standard == "standardized") {
    #     output$shade_descriptions <- shiny::renderUI({
    #       shiny::checkboxInput(
    #         inputId = shiny::NS(id, "shade_descriptions"),
    #         label = "Shade according to descriptions?",
    #         value = FALSE
    #       )
    #     })
    #   } else {
    #     output$shade_descriptions <- shiny::renderUI(shiny::markdown(""))
    #   }
    # })

    output$plotsUI <- shiny::renderUI({
      if (input$all_or_one_plot == "one") {
        htmltools::tagList(
          shiny::plotOutput(shiny::NS(id, paste(input$var_to_plot, "plot", sep = "_")),
                            width = "100%")
        )
      } else {
        if (input$raw_or_standard != "raw") {
          ## Check if score requested exists in data, and has any non-missing values
          tmp_dat <- dat() |>
            dplyr::select(
              tidyselect::any_of(paste(input$raw_or_standard, var_avail(), sep = "_"))
            ) |>
            dplyr::select(
              tidyselect::where(\(x) sum(!is.na(x)) > 0)
            )

          if (ncol(tmp_dat) > 0) {
            to_plot <- stringr::str_remove(colnames(tmp_dat), paste0(input$raw_or_standard, "_"))
          } else {
            to_plot <- NULL
          }

        } else {
          to_plot <- var_avail()
        }

        if (length(to_plot) == 0) {
          return(shiny::markdown(glue::glue("No variables with {c('standardized' = 'z-score', 'ss' = 'SS')[input$raw_or_standard]} values.")))
        } else {
          plot_output_list <- lapply(to_plot, function(x) {
            shiny::plotOutput(shiny::NS(id, paste(x, "plot", sep = "_")), width = "100%")
          })

          base::do.call(htmltools::tagList, plot_output_list)
        }
      }
    })

    shiny::observe({
      # print(c(input$var_to_plot, paste(input$raw_or_standard, input$var_to_plot, sep = "_")))
      # print(any(c(input$var_to_plot, paste(input$raw_or_standard, input$var_to_plot, sep = "_")) %in% colnames(dat())))

      if (input$all_or_one_plot == "one" & any(c(input$var_to_plot, paste(input$raw_or_standard, input$var_to_plot, sep = "_")) %in% colnames(dat()))) {
        output[[paste(input$var_to_plot, "plot", sep = "_")]] <- shiny::renderPlot({
          plot_cog_var(
            dat(),
            var_to_plot = input$var_to_plot,
            type = input$raw_or_standard,
            shade_descriptions = input$shade_descriptions,
            trim = trim
          ) +
            ggplot2::labs(
              title = cog_vars_labels[input$var_to_plot]
            )
        })
      } else {

        print(names(output))

        for (var in var_avail()) {
          local({
            my_var <- var

            # If we're plotting not-raw scores, are there any non-missing values
            # for this variable?
            if (input$raw_or_standard != 'raw') {
              tmp_dat <- dat() |>
                dplyr::select(tidyselect::any_of(paste(input$raw_or_standard, my_var, sep = "_"))) |>
                dplyr::select(
                  tidyselect::where(\(x) sum(!is.na(x)) > 0)
                )

              no_obs <- ncol(tmp_dat) == 0
            }

            if (input$raw_or_standard != "raw" & no_obs) {

              output[[paste(my_var, "plot", sep = "_")]] <- shiny::renderPlot({
                ggplot2::ggplot() +
                  ggplot2::ggtitle(cog_vars_labels[my_var]) +
                  #marquee::geom_marquee(
                  ggplot2::geom_text(
                    ggplot2::aes(
                      x = I(0.5),
                      y = I(0.5),
                      label = paste(c("standardized" = "z-score", "ss" = "SS")[input$raw_or_standard],
                                    "values not available for",
                                    cog_vars_labels[my_var])
                    )
                  ) +
                  ggplot2::theme_minimal() +
                  ggplot2::theme(axis.title = ggplot2::element_blank())
              })
            } else {
              output[[paste(my_var, "plot", sep = "_")]] <- shiny::renderPlot({
                plot_cog_var(
                  dat(),
                  var_to_plot = my_var,
                  type = input$raw_or_standard,
                  shade_descriptions = input$shade_descriptions,
                  trim = trim
                ) +
                  ggplot2::labs(
                    title = cog_vars_labels[my_var]
                  )
              })
            }
          })
        }
      }
    })

    # output$plotCogVar <- shiny::renderUI({
    #   htmltools::tagList(
    #     map(plots(), \(x) shiny::renderPlot(x))
    #   )
    # })

  })
}

#' Shiny App Wrapping the variable plotting module
#'
#' @param dat Data from REDCap to use
#'
#' @inheritParams plot_cog_var
#'
#' @export
plotCogVarApp <- function(dat, trim = Inf) {
  ui <- shiny::fluidPage(
    bslib::card(plotCogVarUI("plot_cog_var"))
  )

  server <- function(input, output, session) {
    plotCogVarServer("plot_cog_var", shiny::reactive(dat), trim)
  }

  shiny::shinyApp(ui, server)
}
