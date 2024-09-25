# library(gt)
# library(gtExtras)
# library(bslib)

ui <- bslib::page_navbar(
  title = "Main App Title",
  id = "main_navbar",
  bslib::nav_panel(
    title = "Data Selection",
    value = "dataPrep",
    # bslib::layout_columns(
    #   col_widths = c(-3, 6, -3),
    #   max_height = "300px",
      # bslib::card(
      #   width = 300,
      #   bslib::card_header("Choose Data to Use"),
      #   bslib::card_body(

          dataPrepUI("dataPrep")
        # ),
        # fill = T
      # )
#    )
  ),
  bslib::nav_panel(
    title = '"Print Outs"',
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        width = "325px",
        shiny::selectizeInput(
          inputId = 'current_studyid',
          label = 'Study IDs',
          choices = NULL,
          options = list(create = FALSE)
        ),
        gt::gt_output("demographics_table"),
        gt::gt_output("functional_measures"),
        bslib::accordion(
          open = FALSE,
          bslib::accordion_panel(
            title = "Options",
            shiny::sliderInput(
              inputId = 'main_table_pct',
              label = "Main Table Font Size (pct)",
              value = 80,
              min = 1, max = 150
            )#,
            # shiny::checkboxInput(
            #   inputId = "bright_colors",
            #   label = "Use Brighter Colors",
            #   value = FALSE
            # )
          )
        )
      ),
      bslib::layout_columns(
        col_widths = c(6,6),
        bslib::card(
          id = "main-table",
          full_screen = T,
          bslib::card_header("NACC T-Cog Neuropsychological Assessment Summary Table"),
          bslib::card_body(
            shiny::selectInput(
              inputId = 'current_date',
              label = "Visit Date",
              choices = NULL#,
              # options = list(create = FALSE)
            ),
            mainTableUI("main_table")
          )
        ),
        bslib::card(
          id = "main-plot",
          full_screen = T,
          bslib::card_header("Longitudinal Trends"),
          bslib::navset_card_underline(
            bslib::nav_panel(
              title = "Plots",
              plotCogVarUI("plot_cog_var")
            ),
            bslib::nav_panel(
              title = "Table",
              longTableUI("long_table")
            )
          )
        )
      )
    )
  )
)
