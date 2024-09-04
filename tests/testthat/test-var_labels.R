test_that("var_labels works", {
  var_labels_for_test <- var_labels(
    cog_cdr_sob = 1,
    cog_tmta_err = 2,
    cog_tmta_lines = 3,
    cog_benson_retained = 4,
    cog_benson_recog = 5,
    cog_ravlt_a1 = 1,
    cog_ravlt_a2 = 2,
    cog_ravlt_a3 = 3,
    cog_ravlt_a4 = 4,
    cog_ravlt_a5 = 5,
    cog_ravlt_recog_tp = 20,
    cog_ravlt_recog_tn = 30,
    cog_tmtb_err = 1,
    cog_tmtb_lines = 2,
    cog_craft_verb_retain = 2,
    cog_craft_par_retain = 5
  )

  var_labels_test_results <- c(
    "cog_cdr_global" = "CDR Global (1 SOB)",
    "cog_moca$" = "MoCA",
    "cog_moca_blind$" = "MoCA Blind",
    "cog_ticsm" = "TICS-m",
    "cog_tmta_time" = "Trailmaking Part A (2 errors; 3/24 CL)",
    "cog_otmta_time" = "Oral Trailmaking Part A - Completion Time",
    "cog_otmta_error" = "Oral Trailmaking Part A - Errors",
    "cog_nsf_total" = "Number Span Forward - Total",
    "cog_nsf_span" = "Number Span Forward - Span Length",
    "cog_nsb_total" = "Number Span Backward - Total",
    "cog_nsb_span" = "Number Span Backward - Span Length",
    "cog_digsym" = "WAIS-R Digit Symbol",
    "cog_mint_tot" = "MINT",
    "cog_animal_flu" = "Animal Fluency",
    "cog_veg_flu" = "Vegetable Fluency",
    "cog_fl_flu" = "F+L Words",
    "cog_flc_flu" = "F+L+C Words",
    "cog_f_flu" = "F Words",
    "cog_l_flu" = "L Words",
    "cog_benson_copy" = "Benson Figure Copy",
    "cog_benson_delay" = "Benson Delay (4% retained; Recog = 5)",
    "cog_craft_imm_ver" = "Craft Immediate - Verbatim",
    "cog_craft_imm_par" = "Craft Immediate - Paraphrase",
    "cog_craft_delay_verb" = "Craft Delay - Verbatim (2% retained)",
    "cog_craft_delay_par" = "Craft Delay - Paraphrase (5% retained)",
    "cog_ravlt_a1_a5_total" = "RAVLT Total Learning (1,2,3,4,5)",
    "cog_ravlt_b1" = "RAVLT Distractor List",
    "cog_ravlt_a6" = "RAVLT Short Delay",
    "cog_ravlt_a7" = "RAVLT Long Delay",
    "cog_ravlt_recog_acc" = "RAVLT Recognition (20, 30)",
    "cog_tmtb_time" = "Trailmaking Part B (1 errors; 2/24 CL)",
    "cog_moca_clock" = "Clock Drawing Test",
    "cog_otmtb_time" = "Oral Trailmaking Part B - Completion Time",
    "cog_otmtb_error" = "Oral Trailmaking Part B - Errors",
    "cog_gds15" = "GDS-15 (Depression Symptoms)"
  )

  expect_equal(var_labels_for_test, var_labels_test_results)
})

