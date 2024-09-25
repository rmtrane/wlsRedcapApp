#' Prepare data from REDCap
#'
#' This function prepares data downloaded from REDCap to be presented using `main_table()`.
#' It removes records without a valid age before running `add_standardized_scores()`,
#' which adds age groups and standardized scores.
#'
#' @param decrypt_password password to decrypt REDCap API Token so data can be
#'    fetched straight from REDCap.
#' @param file path to file downloaded from REDCap
#' @param nas character vector of strings that should be treated as NA
#'
#' @examples
#' \dontrun{
#' prep_data(
#'   file = "~/Downloads/WisconsinLongitudina_DATA_2024-08-10_1500.csv",
#'   nas = c("NA", "N/A", "-", " ", "", "#REF!", "17/22", "#DIV/0!", "#VALUE!")
#' )
#' }
#'
#' @export

prep_data <- function(
    decrypt_password,
    file, # "~/Downloads/WisconsinLongitudina_DATA_2024-08-10_1500.csv",
    nas = c(
      "NA",
      "N/A",
      "-",
      " ",
      "",
      "#REF!",
      "17/22",
      "#DIV/0!",
      "#VALUE!",
      "0,5",
      "23a",
      "28b"
    )
) {

  if (!missingArg(decrypt_password)) {
    redcap_call <- REDCapR::redcap_read(
      batch_size = 2500L,
      redcap_uri = "https://redcap.medicine.wisc.edu/redcap_v13.7.18/API/",
      token = safer::decrypt_string(encrypted_api_token, decrypt_password),
      fields = c(
        "ptid",
        "cog_studyid",
        "cog_race",
        "cog_education",
        "cog_sex",
        "cog_handedness",
        "cog_test_date",
        "cog_age",
        "cog_cdr_global",
        "cog_moca",
        "cog_moca_blind",
        "cog_ticsm",
        "cog_tmta_time",
        "cog_tmta_err",
        "cog_tmta_lines",
        "cog_otmta_time",
        "cog_otmta_error",
        "cog_nsf_total",
        "cog_nsf_span",
        "cog_nsb_total",
        "cog_nsb_span",
        "cog_digsym",
        "cog_mint_tot",
        "cog_animal_flu",
        "cog_veg_flu",
        "cog_fl_flu",
        "cog_flc_flu",
        "cog_f_flu",
        "cog_l_flu",
        "cog_benson_copy",
        "cog_benson_delay",
        "cog_craft_imm_ver",
        "cog_craft_imm_par",
        "cog_craft_delay_verb",
        "cog_craft_delay_par",
        "cog_ravlt_a1_a5_total",
        "cog_ravlt_a1",
        "cog_ravlt_a2",
        "cog_ravlt_a3",
        "cog_ravlt_a4",
        "cog_ravlt_a5",
        "cog_ravlt_b1",
        "cog_ravlt_a6",
        "cog_ravlt_a7",
        "cog_ravlt_recog_tp",
        "cog_ravlt_recog_tn",
        "cog_ravlt_recog_acc",
        "cog_tmtb_time",
        "cog_tmtb_err",
        "cog_moca_clock",
        "cog_otmtb_time",
        "cog_otmtb_error",
        "cog_tmtb_lines",
        "cog_gds15",
        "cog_fas",
        "cog_cdr_sob",
        "cog_iqcode_inform",
        "cog_iqcode_self",
        "cog_benson_recog"
      )
    )

    raw_dat <- tibble::as_tibble(redcap_call$data) |>
      dplyr::mutate(
        dplyr::across(
          -c(
            .data$ptid,
            .data$cog_studyid,
            .data$cog_race,
            .data$cog_test_date,
            .data$cog_sex,
            .data$cog_handedness,
            .data$redcap_event_name
          ),
          \(x) {
            x[which(x %in% nas)] <- NA

            as.numeric(x)
          }
        )
      )
  }

  if (!missingArg(file))
    raw_dat <- readr::read_csv(
      file,
      na = nas
    )

  if (!exists("raw_dat"))
    message("Either 'decrypt_password' or 'file' must be provided")

  if(!lubridate::is.Date(raw_dat$cog_test_date)) {
    raw_dat <- raw_dat |>
      dplyr::filter(!is.na(.data$cog_age)) |>
      dplyr::mutate(
        cog_test_date = dplyr::case_match(
          .data$cog_test_date,
          "2//27/2023" ~ "2/27/2023",
          "8/12/221"   ~ "8/12/2021",
          "9/6/20223"  ~ "9/6/2023",
          "9/7/202023" ~ "9/7/2023",
          .default = .data$cog_test_date
        ),
        cog_test_date = dplyr::if_else(is.na(.data$cog_test_date), NA, lubridate::as_date(.data$cog_test_date, format = "%m/%d/%Y"))
      )
  }

  if (!"cog_ravlt_recog_acc" %in% colnames(raw_dat)) {
    raw_dat$cog_ravlt_recog_acc <- dplyr::if_else(
      raw_dat$cog_ravlt_recog_tp > 50,
      raw_dat$cog_ravlt_recog_tp,
      floor((raw_dat$cog_ravlt_recog_tp + raw_dat$cog_ravlt_recog_tn)/30*100)
    )
  }

  raw_dat |>
    add_standardized_scores()
}
