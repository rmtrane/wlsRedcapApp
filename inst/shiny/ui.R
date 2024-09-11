# library(gt)
# library(gtExtras)
# library(bslib)

ui <- bslib::page_navbar(
  title = "Main App Title",
  id = "main_navbar",
  bslib::nav_panel(
    title = "Data Selection",
    value = "dataPrep",
    bslib::card(
      bslib::card_header("Choose Data to Use"),
      bslib::card_body(
        dataPrepUI("dataPrep")
      )
    )
  ),
  bslib::nav_panel(
    title = '"Print Outs"',
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        width = "300px",
        shiny::selectizeInput(
          inputId = 'current_studyid',
          label = 'Study IDs',
          choices = NULL
        ),
        shiny::uiOutput("demographics_table"),
        gt::gt_output("functional_measures"),
        shiny::sliderInput(
          inputId = 'main_table_pct',
          label = "Main Table Font Size (pct)",
          value = 80,
          min = 1, max = 150
        )
      ),
      bslib::layout_columns(
        col_widths = c(7,5),
        bslib::card(
          id = "main-table",
          full_screen = T,
          bslib::card_header("NACC T-Cog Neuropsychological Assessment Summary Table"),
          bslib::card_body(
            shiny::uiOutput("current_date"),
            mainTableUI("main_table")
          )
        ),
        bslib::card(
          id = "main-plot",
          full_screen = T,
          bslib::card_header("Longitudinal Trends"),
          plotCogVarUI("plot_cog_var")
        )
      )
    )
  )
)
