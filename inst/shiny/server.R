server <- function(input, output, session) {
  ## Hide 'Print Outs' on startup
  bslib::nav_hide(id = "main_navbar", target = '"Print Outs"')

  ## Prepare data
  redcap_data <- dataPrepServer("dataPrep")

  ## When data is ready, redcap_data will be a tibble. Move to 'Print Outs'
  ## and update options for study ID dropdown.
  shiny::observeEvent(tibble::is_tibble(redcap_data()), {
    bslib::nav_show(
      id = "main_navbar",
      target = '"Print Outs"',
      select = T
    )

    study_id_choices <- unique(redcap_data()$cog_studyid)

    if (exists("dev_mode")) {
      if (dev_mode)
        names(study_id_choices) <- redcap_data() |> count(cog_studyid) |> with(paste0(cog_studyid, " (", n, " visit(s))"))
    }

    shiny::updateSelectizeInput(session, "current_studyid", choices = study_id_choices, server = TRUE)
  }, ignoreInit = T)


  ## Create functional measures table.
  output$functional_measures <- gt::render_gt({
    if (tibble::is_tibble(redcap_data())) {
      if (input$current_studyid %in% redcap_data()$cog_studyid) {
        functional_measures_table(
          redcap_data() |> dplyr::filter(.data$cog_studyid == input$current_studyid)
        )
      }
    }
  })

  ## Create demographics table
  output$demographics_table <- gt::render_gt({
    if (tibble::is_tibble(redcap_data())) {
      if (input$current_studyid %in% redcap_data()$cog_studyid) {
        demographics_table(
          redcap_data() |> dplyr::filter(.data$cog_studyid == input$current_studyid)
        )
      }
    }
  })

  ## Update dropdown menu with visit dates when new study ID selected
  observeEvent(input$current_studyid, {
    shiny::updateSelectInput(
      session,
      inputId = "current_date",
      choices = as.character(unique(dplyr::filter(redcap_data(), .data$cog_studyid == input$current_studyid)$cog_test_date))
    )
  }, ignoreNULL = T, ignoreInit = T)

  ## Color scales
  color_scales <- list(
    setNames(
      RColorBrewer::brewer.pal(n = 7, "RdYlGn"),
      nm = c("Impaired", "Borderline", "Low Average", "Average", "High Average", "Superior", "Very Superior")
    ),
    setNames(
      scales::div_gradient_pal(low = "red", mid = "yellow", high = "green")(0:6/6),
      nm = c("Impaired", "Borderline", "Low Average", "Average", "High Average", "Superior", "Very Superior")
    )
  )

  ## Create plots
  plotCogVarServer(
      "plot_cog_var",
      redcap_data = redcap_data,
      studyid = reactive(input$current_studyid),
      fill_values = reactive(color_scales[[1]])
  )

  observe({
    longTableServer(
      "long_table",
      dat = reactive(dplyr::filter(redcap_data(), .data$cog_studyid == input$current_studyid)),
      fill_values = color_scales[[1]], #[[as.numeric(input$bright_colors) + 1]],
      table_font_size = input$main_table_pct
    )
  }) |>
    bindEvent(
      input$current_studyid,
      # input$bright_colors,
      input$main_table_pct
    )

  shiny::observeEvent({
    input$current_date
    input$main_table_pct
    # input$bright_colors
  }, {
    dat_for_table <- dplyr::filter(redcap_data(), .data$cog_studyid == input$current_studyid, .data$cog_test_date == input$current_date)

    if (nrow(dat_for_table) == 1) {
      mainTableServer(
        "main_table",
        dat = reactive(dat_for_table),
        fill_values = color_scales[[1]], #[[as.numeric(input$bright_colors) + 1]],
        table_font_size = input$main_table_pct
      )
    }
  }, ignoreInit = T)
}
