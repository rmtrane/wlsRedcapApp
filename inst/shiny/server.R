server <- function(input, output, session) {
  bslib::nav_hide(id = "main_navbar", target = '"Print Outs"')
  redcap_data <- dataPrepServer("dataPrep")


  shiny::observeEvent(tibble::is_tibble(redcap_data()), {
    bslib::nav_show(
      id = "main_navbar",
      target = '"Print Outs"',
      select = T
    )

    shiny::updateSelectizeInput(session, "current_studyid", choices = unique(redcap_data()$cog_studyid), server = TRUE)

    output$current_date <- shiny::renderUI({
      shiny::selectizeInput(
        inputId = 'current_date',
        label = "Visit Date",
        choices = NULL
      )
    })
  },
  ignoreInit = T)

  shiny::observeEvent(input$current_studyid, {

    if (input$current_studyid %in% redcap_data()$cog_studyid) {

      shiny::updateSelectizeInput(
        session,
        inputId = "current_date",
        choices = as.character(unique(dplyr::filter(redcap_data(), cog_studyid == input$current_studyid)$cog_test_date))
      )

      output$functional_measures <- gt::render_gt({
        functional_measures_table(
          redcap_data(),
          studyid = input$current_studyid
        )
      })


      cur_pt_dat <- reactiveVal()

      cur_pt_dat(dplyr::filter(redcap_data(), .data$cog_studyid == input$current_studyid))

      plotCogVarServer("plot_cog_var", dat = cur_pt_dat)

      output$demographics_table <- shiny::renderUI({
        gt::render_gt({
          demographics_table(
            redcap_data(),
            studyid = input$current_studyid
          )
        })
      })
    }

  }, ignoreInit = T)

  cur_main_table <- reactiveVal()

  shiny::observeEvent({
    input$current_date
    input$main_table_pct
  }, {
    cur_main_table(
      mainTableServer(
        "main_table",
        # dat = redcap_data,
        dat = reactive(dplyr::filter(redcap_data(), .data$cog_studyid == input$current_studyid, .data$cog_test_date == input$current_date)),
        # studyid = input$current_studyid,
        # date = input$current_date,
        table_font_size = input$main_table_pct
      )
    )
  }, ignoreInit = T)
}
