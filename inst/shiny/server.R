server <- function(input, output, session) {

  redcap_data <- dataPrepServer("dataPrep")


  observeEvent(tibble::is_tibble(redcap_data()), {
    nav_select(
      id = "main_navbar",
      selected = '"Print Outs"'
    )

    updateSelectizeInput(session, "current_studyid", choices = unique(redcap_data()$cog_studyid), server = TRUE)

    output$current_date <- renderUI({
      selectizeInput(
        inputId = 'current_date',
        label = "Visit Date",
        choices = NULL
      )
    })
  },
  ignoreInit = T)

  observeEvent(input$current_studyid, {

    if (input$current_studyid %in% redcap_data()$cog_studyid) {

      updateSelectizeInput(
        session,
        inputId = "current_date",
        choices = as.character(unique(filter(redcap_data(), cog_studyid == input$current_studyid)$cog_test_date))
      )

      output$functional_measures <- gt::render_gt({
        functional_measures_table(
          redcap_data(),
          studyid = input$current_studyid
        )
      })


      plotCogVarServer("plot_cog_var", dat = dplyr::filter(redcap_data(), .data$cog_studyid == input$current_studyid))

      output$demographics_table <- renderUI({
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

  observeEvent({
    input$current_date
    input$main_table_pct
  }, {
    cur_main_table(
      mainTableServer(
        "main_table",
        dat = redcap_data,
        studyid = input$current_studyid,
        date = input$current_date,
        table_font_size = input$main_table_pct
      )
    )
  }, ignoreInit = T)



}
