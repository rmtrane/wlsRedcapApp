library(tidyverse)
library(gt)
library(gtExtras)

ui <- page_navbar(
  title = "Main App Title",
  id = "main_navbar",
  nav_panel(
    title = "Data Selection",
    value = "dataPrep",
    card(
      card_header("Choose Data to Use"),
      card_body(
        dataPrepUI("dataPrep")
      )
    )
  ),
  nav_panel(
    title = '"Print Outs"',
    page_sidebar(
      sidebar = sidebar(
        width = "300px",
        ## PLACE DATA UI HERE
        selectizeInput(
          inputId = 'current_studyid',
          label = 'Study IDs',
          choices = NULL
        ),
        uiOutput("demographics_table"),
        gt_output("functional_measures"),
        sliderInput(
          inputId = 'main_table_pct',
          label = "Main Table Font Size (pct)",
          value = 80,
          min = 1, max = 150
        )
      ),
      layout_columns(
        col_widths = c(7,5),
        card(
          id = "main-table",
          full_screen = T,
          card_header("NACC T-Cog Neuropsychological Assessment Summary Table"),
          card_body(
            uiOutput("current_date"),
            mainTableUI("main_table")
          )
        ),
        card(
          id = "main-plot",
          full_screen = T,
          card_header("Longitudinal Trends"),
          card_body(
            plotCogVarUI("plot_cog_var")
          )
        )
      )
    )
  )
)
