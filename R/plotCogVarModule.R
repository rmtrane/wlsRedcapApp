#' Shiny Module to Plot Scores Across Visits
#'
#' This shiny module is meant for visualizing changes over time of a long list
#' of cognitive measures from the WLS ILIAD study.
#'
#' @param id An ID string to match module UI and module server
#' @param redcap_data Data to use for the plots. Should contain a number of `cog_*`
#'  variables
#' @param studyid ID of participant; reactive
#' @param fill_values a named vector of length seven with hex color values to use for categories
#'   "Impaired", "Borderline", "Low Average", "Average", "High Average", "Superior", "Very Superior"
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
    # tags$script("
    #   Shiny.addCustomMessageHandler('update-visibility-value', function(data) {
    #     console.log(data.inputName + '. Old value: ' + data.old_value + '. New value:', data.value);
    #     Shiny.setInputValue(data.inputName, data.value);
    #   });
    # "),
    shiny::checkboxInput(
      inputId = shiny::NS(id, "shade_descriptions"),
      label = "Shade according to descriptions?",
      value = T
    ),
    # shiny::actionButton(
    #   inputId = shiny::NS(id, "reset_visiblity"),
    #   label = "Reset legends"
    # ),
    # shiny::uiOutput("visibility"),
    bslib::card_body(
      shiny::uiOutput(shiny::NS(id, "plotsUI")),
      fillable = F
    )
  )
}

#' @rdname plotCogVarModule
#'
#' @export
#'

plotCogVarServer <- function(
    id,
    redcap_data,
    studyid,
    fill_values = setNames(
      RColorBrewer::brewer.pal(n = 7, "RdYlGn"),
      nm = c("Impaired", "Borderline", "Low Average", "Average", "High Average", "Superior", "Very Superior")
    )
) {
  # stopifnot("studyid must be a reactive" = shiny::is.reactive(studyid))

  shiny::moduleServer(id, function(input, output, session) {

    js <- "function(el, x, input){
      el.on('plotly_afterplot', function(eventdata) {
        var out = [];
        // Function to get needed info from traces.
        function getTraceInfo(trace, traceindex) {
          // If trace has a name, set tracename
          if (typeof trace.name !== 'undefined') {
            var tracename = trace.name ;
          } else {
            var tracename = '';
          }

          // If trace has visible attribute, set tracevisible
          if (typeof trace.visible !== 'undefined') {
            var tracevisible = trace.visible ;
          } else {
            var tracevisible = '';
          }

          // If trace has customdata attribute, set name and create
          // input$ns-name_visibility
          if (typeof trace.customdata !== 'undefined') {
            var name = trace.customdata ;

            Shiny.setInputValue(input.ns + '-' + name + '_visibility', tracevisible);
          } else {
            var name = '';
          }

          // Add to out list
          out.push([tracename=tracename, index=traceindex, name = name, visible = tracevisible]);
        }

        // Run function for each trace
        x.data.forEach(getTraceInfo);

        // Create input$input.name
        Shiny.setInputValue(input.name, out);

        // Create input$base_plots_drawn to indicate base plots have been created.
        Shiny.setInputValue(input.ns + '-base_plots_drawn', 1)
      });
    }"

    # fill_values <- reactiveVal(fill_values)

    ## Create UI for plots
    output$plotsUI <- shiny::renderUI({
      ## Initiate list to hold plot UIs
      plot_output_list <- as.list(unique(cog_vars_groups))
      names(plot_output_list) <- paste(unique(cog_vars_groups), "plot", sep = "_")

      ## For entry in list...
      plot_output_list <- lapply(plot_output_list, \(cur_group) {
        ## ... get vector of variables in group
        cur_cog_vars <- names(cog_vars_groups[cog_vars_groups == cur_group])

        ## Return card with a title and plotlyOutput
        bslib::card(
          bslib::card_title(cur_group),
          bslib::card_body(
            plotly::plotlyOutput(shiny::NS(id, paste(cur_group, "plot", sep = "_")), width = "100%")
          )
        )
      })

      ## Return all cards as tagList
      base::do.call(shiny::tagList, plot_output_list)
    })


    ## Reactive value used to trigger drawing of traces when base_plots are created
    base_plots_drawn <- shiny::reactiveVal(value = 0)

    ## When data input changes, create base plots.
    shiny::observe({
      if (nrow(redcap_data()) > 0) {
        print("Creating base plots...")

        ## Create base plot for each group of variables
        for (var_group in unique(cog_vars_groups)) {
          ## Need to use local so that the loop variable is evaluated in the right
          ## namespace
          local({
            my_var_group <- var_group
            cur_vars <- paste("standardized", names(cog_vars_groups[cog_vars_groups == my_var_group]), sep = "_")

            # Are there any non-missing values for this group of variables?
            tmp_dat <- redcap_data() |>
              dplyr::select(
                tidyselect::any_of(cur_vars)
              ) |>
              dplyr::select(
                tidyselect::where(\(x) sum(!is.na(x)) > 0)
              )

            no_obs <- ncol(tmp_dat) == 0

            output[[paste(my_var_group, "plot", sep = "_")]] <- plotly::renderPlotly({
              base_plot_z_scores(
                redcap_data(),
                cog_vars = stringr::str_remove_all(colnames(tmp_dat), "^standardized_"),
                fill_values = fill_values(),
                source = my_var_group
              ) |>
                plotly::layout(
                  showlegend = TRUE,
                  legend = list(
                    traceorder = "normal"
                  ),
                  xaxis = list(
                    title = ""
                  )
                ) |>
                plotly::event_register(event = "plotly_legendclick") |>
                plotly::event_register(event = "plotly_legenddoubleclick") |>
                htmlwidgets::onRender(
                  js,
                  data = list(
                    name = shiny::NS(id, paste(my_var_group, "TraceMapping", sep = "_")),
                    ns = id
                  )
                )
            })
          })
        }
        base_plots_drawn(base_plots_drawn() + 1)
      }
    }) |>
      shiny::bindEvent(
        list(
          redcap_data(),
          fill_values()
        ),
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

    shiny::observe({
      print("Here")
      print(paste("input$base_plots_drawn:", input$base_plots_drawn))
    }) |>
      shiny::bindEvent(
        input$base_plots_drawn
      )


    visibility_defaults <- list(
      "cog_cdr_global" = TRUE,
      "cog_moca" = TRUE,
      "cog_moca_blind" = TRUE,
      "cog_ticsm" = TRUE,
      "cog_tmta_time" = TRUE,
      "cog_otmta_time" = TRUE,
      "cog_otmta_error" = "legendonly",
      "cog_nsf_total" = "legendonly",
      "cog_nsf_span" = TRUE,
      "cog_nsb_total" = "legendonly",
      "cog_nsb_span" = TRUE,
      "cog_digsym" = TRUE,
      "cog_mint_tot" = TRUE,
      "cog_animal_flu" = TRUE,
      "cog_veg_flu" = TRUE,
      "cog_fl_flu" = "legendonly",
      "cog_flc_flu" = TRUE,
      "cog_f_flu" = TRUE,
      "cog_l_flu" = TRUE,
      "cog_benson_copy" = TRUE,
      "cog_benson_delay" = TRUE,
      "cog_craft_imm_ver" = "legendonly",
      "cog_craft_imm_par" = TRUE,
      "cog_craft_delay_verb" = "legendonly",
      "cog_craft_delay_par" = TRUE,
      "cog_ravlt_a1_a5_total" = TRUE,
      "cog_ravlt_b1" = "legendonly",
      "cog_ravlt_a6" = "legendonly",
      "cog_ravlt_a7" = TRUE,
      "cog_ravlt_recog_acc" = TRUE,
      "cog_tmtb_time" = TRUE,
      "cog_otmtb_time" = TRUE,
      "cog_otmtb_error" = TRUE,
      "cog_moca_clock" = TRUE,
      "cog_gds15" = TRUE
    )


    ## Toggle shades
    shiny::observe({
      ## For each group, update visible attribute for shades
      lapply(unique(cog_vars_groups), FUN = \(cur_var_group){

        if (!is.null(input[[paste(cur_var_group, "TraceMapping", sep = "_")]])) {
          traces <- matrix(input[[paste(cur_var_group, "TraceMapping", sep = "_")]], ncol = 4, byrow = TRUE)

          indices <- traces[,2][
            traces[, 1] %in% c(
              "Impaired",
              "Borderline",
              "Low Average",
              "Average",
              "High Average",
              "Superior",
              "Very Superior"
            )
          ]

          plotly::plotlyProxy(paste(cur_var_group, "plot", sep = "_"), session) |>
            plotly::plotlyProxyInvoke(
              method = "restyle",
              list(
                visible = rep(list(input$shade_descriptions), length(indices))
              ),
              indices
            )
        }
      })
    }) |>
      shiny::bindEvent(input$shade_descriptions)

    cog_vars_colors <- setNames(
      rep(RColorBrewer::brewer.pal(n = 12, "Paired")[-11], length.out = length(cog_vars_groups)),
      names(cog_vars_groups)
    )

    shiny::observe({

      # if (!is.null(studyid()) & studyid() != ""){

      print(base_plots_drawn())

      Sys.sleep(0.1)

      if (base_plots_drawn() > 0) {

        # print("Adding traces")

        cur_studyid_dat <- redcap_data() |>
          dplyr::filter(
            .data$cog_studyid == studyid()
          )

        # for (var_group in unique(cog_vars_groups)) {
        lapply(unique(cog_vars_groups), \(cur_var_group){ # })
          cur_vars <- names(cog_vars_groups)[cog_vars_groups == cur_var_group]

          # Remove old trace if any
          if (!is.null(input[[paste(cur_var_group, "TraceMapping", sep = "_")]])) {
            traces <- matrix(input[[paste(cur_var_group, "TraceMapping", sep = "_")]], ncol = 4, byrow = TRUE)
            indices <- as.integer(traces[traces[, 1] %in% c(cog_vars_labels, "no_values"), 2])

            plotly::plotlyProxy(paste(cur_var_group, "plot", sep = "_"), session) |>
              plotly::plotlyProxyInvoke("deleteTraces", indices)
          }

          # Check if
          new_dat <- cur_studyid_dat |>
            dplyr::select(
              "cog_test_date",
              tidyselect::any_of(paste("standardized", cur_vars, sep = "_"))
            ) |>
            dplyr::select(
              "cog_test_date",
              tidyselect::where(\(x) sum(!is.na(x)) > 0)
            )

          any_left <- ncol(new_dat[,-which(colnames(new_dat) == "cog_test_date")])

          # print(any_left)

          if (any_left > 0) {

            print(paste("Adding trace for", cur_var_group))

            new_dat <- new_dat |>
              tidyr::pivot_longer(
                cols = tidyselect::any_of(paste("standardized", cur_vars, sep = "_")),
                names_transform = list(name = \(x) stringr::str_remove(x, "standardized_"))
              ) |>
              dplyr::mutate(
                label = stringr::str_replace_all(.data$name, cog_vars_labels)
              )

            lapply(unique(new_dat$name), \(nm) {
              tmp_dat <- dplyr::filter(new_dat, .data$name == nm)

              if (paste(nm, 'visibility', sep = "_") %in% names(input)) {
                vis <- input[[paste(nm, 'visibility', sep = "_")]]
              } else {
                vis <- visibility_defaults[[nm]]
              }

              new_trace <- list(
                type = "scatter",
                mode = "lines+markers",
                x = as.list(tmp_dat$cog_test_date),
                y = as.list(tmp_dat$value),
                text = I(tmp_dat$label),
                marker = list(color = cog_vars_colors[[nm]], line = list(color = cog_vars_colors[[nm]])),
                name = unique(tmp_dat$label),
                visible = vis,
                customdata = nm,
                hovertemplate = paste0(
                  "%{text}<br>",
                  "z-score: %{y:.2f}<br>",
                  "Date: %{x}",
                  "<extra></extra>"
                )
              )

              plotly::plotlyProxy(paste(cur_var_group, "plot", sep = "_"), session) |>
                plotly::plotlyProxyInvoke(
                  method = "addTraces",
                  new_trace
                )
            })

            plotly::plotlyProxy(paste(cur_var_group, "plot", sep = "_"), session) |>
              plotly::plotlyProxyInvoke(
                method = "reconfig",
                staticPlot = FALSE
              ) |>
              plotly::plotlyProxyInvoke(
                method = "relayout",
                #config = list(staticPlot = TRUE),
                #layout = list(
                list(
                  xaxis = list(
                    range = date_range(new_dat$cog_test_date)
                  ),
                  yaxis = list(
                    title = 'z-score',
                    range = c(min(-2.5, min(new_dat$value, na.rm = T)),
                              max( 2.5, max(new_dat$value, na.rm = T)))*1.02
                  ),
                  showlegend = TRUE
                )
              )
          } else {
            # If no values found
            # print(paste("No values for", cur_var_group))

            plotly::plotlyProxy(paste(cur_var_group, "plot", sep = "_"), session) |>
              plotly::plotlyProxyInvoke(
                method = "relayout",
                list(
                  xaxis = list(range = c(-0.5, 0.5), visible = F),
                  yaxis = list(range = c(-0.5, 0.5), visible = F)
                )
              ) |>
              plotly::plotlyProxyInvoke(
                method = "addTraces",
                list(
                  x = list(0),
                  y = list(0),
                  textfont = list(size = 20),
                  text = list("No Values Found"),
                  type = "scatter",
                  mode = "text",
                  name = "no_values",
                  showlegend = FALSE
                )
              ) |>
              plotly::plotlyProxyInvoke(
                method = "reconfig",
                staticPlot = TRUE
              )
          }
        })
      }
    }) |>
      shiny::bindEvent(
        list(
          studyid(),
          redcap_data(),
          input$base_plots_drawn
        ),
        ignoreInit = T
      )
  })
}


# 322002g
# 115163g

#' Shiny App Wrapping the variable plotting module
#'
#' @param dat Data from REDCap to use
#' @param studyids vector of studyids to include in drop-down menu
#'
#'
#' @export
plotCogVarApp <- function(
    dat,
    studyids = c("143050g", "241123g", "301022s", "322002g", "372034g", "382030s", "391310s", "440250g", "461231g", "498013s", "524005g", "595004s", "635012g")
) {


  ui <- bslib::page_sidebar(
    sidebar = bslib::sidebar(
      shiny::selectizeInput(
        inputId = "studyid",
        label = "Study ID",
        choices = NULL,
        selected = NULL
      ),
      shiny::selectInput(
        inputId = "test_date",
        label = "Date",
        choices = NULL
      ),
      shiny::checkboxInput(
        inputId = "brighter_colors",
        label = "Use Brighter Colors"
      )
    ),
    bslib::layout_columns(
      col_widths = c(6,6),
      bslib::card(
        id = "main-table",
        full_screen = T,
        bslib::card_header("NACC T-Cog Neuropsychological Assessment Summary Table"),
        bslib::card_body(
          mainTableUI("main_table")
          # verbatimTextOutput(NS("plot_cog_var", "click"))
          # verbatimTextOutput(NS("plot_cog_var", "PrintTraceMapping"))
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

  server <- function(input, output, session) {
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

    shiny::updateSelectizeInput(inputId = "studyid", choices = studyids, server = TRUE)
    shiny::observeEvent(input$studyid, {
      #dat |> dplyr::filter(.data$cog_studyid == input$studyid) |> dplyr::pull(cog_test_date) |> print()
      shiny::updateSelectInput(
        inputId = "test_date",
        choices = dat$cog_test_date[dat$cog_studyid == input$studyid]
      )
    })

    plotCogVarServer(
      "plot_cog_var",
      redcap_data = shiny::reactive(dat),
      studyid = shiny::reactive(input$studyid),
      fill_values = shiny::reactive(color_scales[[1]]) #[[as.numeric(input$brighter_colors) + 1]])
    )

    mainTableServer(
      "main_table",
      dat = shiny::reactive(dat |> dplyr::filter(.data$cog_studyid == input$studyid, .data$cog_test_date == input$test_date)),
      table_font_size = 80
    )
  }

  shiny::shinyApp(ui, server)
}



if (FALSE){

  cur_cog_vars = c("cog_tmta_time",
                   "cog_otmta_time",
                   "cog_otmta_error",
                   "cog_nsf_total",
                   "cog_nsf_span",
                   "cog_nsb_total",
                   "cog_nsb_span")

  tmp_dat <- filter(redcap_data, cog_studyid == "143050g")

  p <- base_plot_z_scores(redcap_data, cog_vars = cur_cog_vars)

  for (var in paste("standardized", cur_cog_vars, sep = "_")) {
    p <- p |>
      plotly::add_trace(
        type = "scatter",
        mode = "lines+markers",
        x = tmp_dat$cog_test_date,
        y = tmp_dat[[var]],
        name = cog_vars_labels[str_remove(var, "standardized_")],
        color = I(cog_vars_colors[str_remove(var, "standardized_")]),
        showlegend = TRUE
      )
  }

  p
}
