#' Shiny Module for Preparing Data from the WLS REDCap Project
#'
#' This shiny module presents options to either pull data straight from REDCap
#' or upload a .csv file as downloaded from REDCap. To pull from REDCap, a password
#' is required (contact me [here](mailto:rtrane@wisc.edu)).
#'
#' @param id An ID string to match module UI and module server.
#'
#' @returns A `tibble` with the results of `prep_data()`.
#'
#' @rdname dataPrepModule
#'
#' @export

dataPrepUI <- function(id) {
  shiny::tagList(
    shiny::radioButtons(
      inputId = shiny::NS(id, "data_to_prep"),
      label = "Data Source",
      choices = c(
        "Pull From REDCap" = "redcap",
        "Upload CSV File" = "csv_upload",
        "Demo" = "demo"
      )
    ),
    shiny::uiOutput(shiny::NS(id, "data_upload_or_password")),
    shiny::actionButton(
      inputId = shiny::NS(id, "prep_data"),
      label = "Go"
    )
  )
}

#' @rdname dataPrepModule
#' @export
dataPrepServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    placeholder_text <- dplyr::if_else(
      file.exists(here::here("data-raw/.encrypted-api-token-passwd")) |
        !is.null(getOption("wlsRedcapApp_password")),
      "Local password file found.",
      "Enter Password Here"
    )

    output$data_upload_or_password <- shiny::renderUI({
      out <- shiny::markdown("Click the 'Go' button to continue with demo data.")

      if (input$data_to_prep == "redcap")
        out <- shiny::passwordInput(
          inputId = shiny::NS(id, "password"),
          label = "REDCap API Password",
          value = "",
          placeholder = placeholder_text
        )

      if (input$data_to_prep == 'csv_upload')
        out <- shiny::fileInput(
          inputId = shiny::NS(id, "input_file"),
          label = "Select File",
          accept = ".csv"
        )

      out
    })


    redcap_dat <- shiny::reactiveVal()

    shiny::observeEvent({
      input$prep_data
    }, {
      shinycssloaders::showPageSpinner()

      if (input$data_to_prep == 'redcap') {
        result <- try(safer::decrypt_string(encrypted_api_token, input$password), silent = T)

        if (inherits(result, "try-error")) {

          if (input$password == "") {
            if (!is.null(getOption("wlsRedcapApp_password"))) {
              redcap_dat(
                prep_data(
                  decrypt_password = getOption("wlsRedcapApp_password")
                )
              )
            }

            if (file.exists(here::here("data-raw/.encrypted-api-token-passwd"))) {
              redcap_dat(
                prep_data(
                  decrypt_password = readr::read_file(here::here("data-raw/.encrypted-api-token-passwd"))
                )
              )
            }
          } else {
            shiny::showNotification(
              "Invalid Password",
              duration = 5,
              type = "warning"
            )
          }
        } else {
          redcap_dat(
            prep_data(
              decrypt_password = result
            )
          )
        }
      }

      if (input$data_to_prep == "csv_upload") {
        redcap_dat(
          prep_data(
            file = input$input_file$datapath
          )
        )
      }

      if (input$data_to_prep == "demo") {
        redcap_dat(
          demo_data
        )
      }

      shinycssloaders::hidePageSpinner()
    })

    return(redcap_dat)
  })
}

#' Shiny App wrapping the data prep module
#'
#' @export

dataPrepApp <- function() {
  ui <- bslib::page_sidebar(
    sidebar = shiny::column(
      width = 12,
      dataPrepUI("dataPrep")
    ),
    DT::DTOutput("redcap_dat_DT")
  )

  server <- function(input, output, session) {

    redcap_dat <- dataPrepServer("dataPrep")

    output$redcap_head_dat <- shiny::renderTable({
      head(x = redcap_dat())
    })

    output$redcap_dat_DT <- DT::renderDT({
      redcap_dat()
    })

  }

  shiny::shinyApp(ui, server)
}
