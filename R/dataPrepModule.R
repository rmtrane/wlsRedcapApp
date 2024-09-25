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
    shiny::uiOutput(shiny::NS(id, "prep_data"))
  )
}

#' @rdname dataPrepModule
#' @export
dataPrepServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    redcap_good <- file.exists(here::here("data-raw/.encrypted-api-token-passwd")) |
      !is.null(getOption("wlsRedcapApp_password"))

    placeholder_text <- dplyr::if_else(
      redcap_good,
      "Local password found.",
      "Enter Password Here"
    )

    redcap_password_good <- shiny::reactiveVal(value = FALSE)

    # print(input$file)

    output$prep_data <- shiny::renderUI({

      password_present <- shiny::reactive(
        !is.null(input$password) && input$password != ""
      )

      if (input$data_to_prep == "demo" |
          (input$data_to_prep == "redcap" & redcap_good) |
          (input$data_to_prep == "redcap" & password_present()) |
          (input$data_to_prep == "csv_upload" & !is.null(input$input_file))) {

        shiny::actionButton(
          inputId = shiny::NS(id, "prep_data"),
          label = "Go",
          width = "300px"
        )

      }
    })

    output$data_upload_or_password <- shiny::renderUI({
      out <- shiny::markdown("Click the 'Go' button to continue with demo data.")

      if (input$data_to_prep == "redcap") {
        if (redcap_good) {
          shiny::markdown("Locally saved password found.")
        } else {
          out <- htmltools::tagList(
            shiny::passwordInput(
              inputId = shiny::NS(id, "password"),
              label = "REDCap API Password",
              # value = "",
              placeholder = placeholder_text
            )
          )
        }
      }

      if (input$data_to_prep == 'csv_upload')
        out <- shiny::fileInput(
          inputId = shiny::NS(id, "input_file"),
          label = "Select File",
          accept = ".csv",
          width = "300px"
        )

      out
    })

    redcap_dat <- shiny::reactiveVal()

    shiny::observeEvent({
      input$prep_data
    }, {
      shinycssloaders::showPageSpinner()

      if (input$data_to_prep == 'redcap') {
        result <- try(safer::decrypt_string(string = encrypted_api_token, key = input$password), silent = T)

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
              decrypt_password = input$password #result
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
  ui <- bslib::page_fluid(
      dataPrepUI("dataPrep"),
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
