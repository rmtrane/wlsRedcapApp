#' Create Row Labels for Main Table
#'
#' The main table of interest uses some of the observed data in the row labels.
#' This function creates the row labels as a named vector for easy use in
#' `stringr::str_replace_all` to create a new column with the desired row labels.
#' All parameters should be "raw" values.
#'
#' @param cog_cdr_sob Raw observed value of CDR Sob
#' @param cog_tmta_err number of errors made on Trail Making Test A
#' @param cog_tmta_lines lines completed on Trail Making Test A
#' @param cog_benson_retained percent retained on Benson
#' @param cog_benson_recog Benson recognized
#' @param cog_ravlt_a1 Rey AVLT Trial 1
#' @param cog_ravlt_a2 Rey AVLT Trial 2
#' @param cog_ravlt_a3 Rey AVLT Trial 3
#' @param cog_ravlt_a4 Rey AVLT Trial 4
#' @param cog_ravlt_a5 Rey AVLT Trial 5
#' @param cog_ravlt_recog_tp Rey AVLT Recognition True Positives
#' @param cog_ravlt_recog_tn Rey AVLT Recognition True Negatives
#' @param cog_tmtb_err number of errors made on Trail Making Test B
#' @param cog_tmtb_lines lines completed on Trail Making Test B
#' @param cog_craft_verb_retain percentage retained from cog_craft_imm_verb to cog_craft_delay_verb
#' @param cog_craft_par_retain percentage retained from cog_craft_imm_par to cog_craft_delay_par
#'
#' @export
var_labels <- function(
    cog_cdr_sob = NA,
    cog_tmta_err = NA,
    cog_tmta_lines = NA,
    cog_benson_retained = NA,
    cog_benson_recog = NA,
    cog_ravlt_a1 = NA,
    cog_ravlt_a2 = NA,
    cog_ravlt_a3 = NA,
    cog_ravlt_a4 = NA,
    cog_ravlt_a5 = NA,
    cog_ravlt_recog_tp = NA,
    cog_ravlt_recog_tn = NA,
    cog_tmtb_err = NA,
    cog_tmtb_lines = NA,
    cog_craft_verb_retain = NA,
    cog_craft_par_retain = NA
) {

  c(
    ## General Cognition
    "cog_cdr_global" = glue::glue("CDR Global ({cog_cdr_sob} SOB)"),
    "cog_moca$" = "MoCA",
    "cog_moca_blind$" = "MoCA Blind",
    "cog_ticsm" = "TICS-m",
    ## Attention/Processing Speed
    "cog_tmta_time" = glue::glue("Trailmaking Part A ({cog_tmta_err} errors; {cog_tmta_lines}/24 CL)"),
    "cog_otmta_time" = "Oral Trailmaking Part A - Completion Time",
    "cog_otmta_error" = "Oral Trailmaking Part A - Errors",
    "cog_nsf_total" = "Number Span Forward - Total",
    "cog_nsf_span" = "Number Span Forward - Span Length",
    "cog_nsb_total" = "Number Span Backward - Total",
    "cog_nsb_span" = "Number Span Backward - Span Length",
    "cog_digsym" = "WAIS-R Digit Symbol",
    ## Language
    "cog_mint_tot" = "MINT",
    "cog_animal_flu" = "Animal Fluency",
    "cog_veg_flu" = "Vegetable Fluency",
    "cog_fl_flu" = "F+L Words",
    "cog_flc_flu" = "F+L+C Words",
    "cog_f_flu" = "F Words",
    "cog_l_flu" = "L Words",
    ## Visuospatial
    "cog_benson_copy" = "Benson Figure Copy",
    ## Memory
    "cog_benson_delay" = glue::glue("Benson Delay ({cog_benson_retained}% retained; Recog = {cog_benson_recog})"),
    "cog_craft_imm_ver" = "Craft Immediate - Verbatim",
    "cog_craft_imm_par" = "Craft Immediate - Paraphrase",
    "cog_craft_delay_verb" = glue::glue("Craft Delay - Verbatim ({cog_craft_verb_retain}% retained)"),
    "cog_craft_delay_par" = glue::glue("Craft Delay - Paraphrase ({cog_craft_par_retain}% retained)"),
    "cog_ravlt_a1_a5_total" = glue::glue("RAVLT Total Learning ({cog_ravlt_a1},{cog_ravlt_a2},{cog_ravlt_a3},{cog_ravlt_a4},{cog_ravlt_a5})"),
    "cog_ravlt_b1" = "RAVLT Distractor List",
    "cog_ravlt_a6" = "RAVLT Short Delay",
    "cog_ravlt_a7" = "RAVLT Long Delay",
    "cog_ravlt_recog_acc" = glue::glue("RAVLT Recognition ({cog_ravlt_recog_tp}, {cog_ravlt_recog_tn})"),
    ## Execute Functioning
    "cog_tmtb_time" = glue::glue("Trailmaking Part B ({cog_tmtb_err} errors; {cog_tmtb_lines}/24 CL)"),
    "cog_moca_clock" = "Clock Drawing Test",
    "cog_otmtb_time" = "Oral Trailmaking Part B - Completion Time",
    "cog_otmtb_error" = "Oral Trailmaking Part B - Errors",
    ## Mood
    "cog_gds15" = "GDS-15 (Depression Symptoms)"
  )
}
