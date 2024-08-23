#' Shiny Module to Plot Scores Across Visits
#'
#' This shiny module is meant for visualizing changes over time of a long list
#' of cognitive measures from the WLS ILIAD study.
#'
#' @param id An ID string to matche module UI and module server
#' @param dat Data to use for the plots. Should contain a number of `cog_*`
#'  variables
#' @param studyid The study id of the participant of interest
#'
#' @examples
#' \dontrun{
#'   plotCogVarApp(demo_data, demo_data$cog_studyid[1])
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
    shiny::plotOutput(shiny::NS(id, "plotCogVar"), width = "100%")
  )
}

#' @rdname plotCogVarModule
#' @export
plotCogVarServer <- function(id, dat, studyid) {

  shiny::moduleServer(id, function(input, output, session) {

    ## Get variables available
    var_labels_for_plot <- c(
      ## General Cognition
      "cog_cdr_global" = "CDR Global",
      "cog_moca" = "MoCA",
      "cog_moca_blind" = "MoCA Blind",
      "cog_ticsm" = "TICS-m",
      ## Attention/Processing Speed
      "cog_tmta_time" = "Trailmaking Part A",
      "cog_otmta_time" = "Oral Trailmaking Part A - Completion Time",
      "cog_otmta_error" = "Oral Trailmaking Part A - Errors",
      "cog_nsf_total" = "Number Span Forward - Total",
      "cog_nsf_span" = "Number Span Forward - Span Length",
      "cog_nsb_total" = "Number Span Backward - Total",
      "cog_nsb_span" = "Number Span Backward - Span Length",
      "cog_digsym" = "WAIS-R Digit Symbol",
      ## Language
      "cog_mint_tot" = "MINT",
      "cog_animal_flu" = "Animal Fluency",
      "cog_veg_flu" = "Vegetable Fluency",
      "cog_fl_flu" = "F+L Words",
      "cog_flc_flu" = "F+L+C Words",
      "cog_f_flu" = "F Words",
      "cog_l_flu" = "L Words",
      ## Visuospatial
      "cog_benson_copy" = "Benson Figure Copy",
      ## Memory
      "cog_benson_delay" = "Benson Delay",
      "cog_craft_imm_ver" = "Craft Immediate - Verbatim",
      "cog_craft_imm_par" = "Craft Immediate - Paraphrase",
      "cog_craft_delay_verb" = "Craft Delay - Verbatim",
      "cog_craft_delay_par" = "Craft Delay - Paraphrase",
      "cog_ravlt_a1_a5_total" = "RAVLT Total Learning",
      "cog_ravlt_b1" = "RAVLT Distractor List",
      "cog_ravlt_a6" = "RAVLT Short Delay",
      "cog_ravlt_a7" = "RAVLT Long Delay",
      "cog_ravlt_recog_acc" = "RAVLT Recognition",
      ## Execute Functioning
      "cog_tmtb_time" = "Trailmaking Part B",
      "cog_moca_clock" = "Clock Drawing Test",
      "cog_otmtb_time" = "Oral Trailmaking Part B - Completion Time",
      "cog_otmtb_error" = "Oral Trailmaking Part B - Errors",
      ## Mood
      "cog_gds15" = "GDS-15 (Depression Symptoms)"
    )

    cur_pt_dat <- shiny::reactiveVal()
    var_avail <- shiny::reactiveVal()

    cur_pt_dat(
      dat() |>
        dplyr::filter(
          .data$cog_studyid == studyid
        ) |>
        dplyr::select(
          # Identifiers
          .data$cog_studyid:.data$cog_education,
          # Get all scores
          tidyselect::matches(names(var_labels_for_plot))
        ) |>
        # Rename the scores that have not been adjusted. These should be
        # raw_(score)
        dplyr::rename_with(
          .fn = \(x) paste0("raw_", x),
          .cols = tidyselect::matches(paste0("^", names(var_labels_for_plot)))
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
      unique(cur_pt_dat()$name)
    )

    shiny::observeEvent(var_avail(), {
      shiny::updateSelectizeInput(
        session,
        inputId = "var_to_plot",
        choices = setNames(names(var_labels_for_plot[var_avail()]),
                           nm = unname(var_labels_for_plot[var_avail()]))
      )
    })

    scores_avail <- shiny::reactiveVal()

    shiny::observeEvent(input$var_to_plot, {
      scores_avail(
        cur_pt_dat() |>
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


    output$plotCogVar <- shiny::renderPlot({
      plot_cog_var(
        dat = dat(),
        studyid = studyid,
        var_to_plot = input$var_to_plot,
        type = input$raw_or_standard
      ) +
        ggplot2::labs(
          title = var_labels_for_plot[input$var_to_plot]
        )
    })

  })
}

#' Shiny App Wrapping the variable plotting module
#'
#' @param dat Data from REDCap to use
#' @param studyid The id of the participant of interest
#'
#' @export
plotCogVarApp <- function(dat, studyid = "102038g") {
  ui <- shiny::fluidPage(
    bslib::card(plotCogVarUI("plot_cog_var"))
  )

  server <- function(input, output, session) {
    plotCogVarServer("plot_cog_var", dat = shiny::reactive(dat), studyid = studyid)
  }

  shiny::shinyApp(ui, server)
}
