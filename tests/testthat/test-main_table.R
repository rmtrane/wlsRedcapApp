test_that("main_table", {
  for_test <- demo_data[5,]

  test_main_table <- main_table(for_test)$`_data`

  expect_equal(
    colnames(test_main_table),
    c("group", "labels", "name", "Raw", "Raw_suffix", "z-score", "SS", "Percentile", "Description")
  )

  expect_true(is.numeric(test_main_table$Percentile))
  expect_true(is.character(test_main_table$Description))

  expect_true(
    all(
      test_main_table$Description %in% c(
        "Impaired",
        "Borderline",
        "Low Average",
        "Average",
        "High Average",
        "Superior",
        "Very Superior",
        "Normal",
        NA,
        "Minimal",
        "Mild",
        "Moderate",
        "Severe",
        "Normal",
        "Very Mild",
        "Mild",
        "Moderate",
        "Severe"
      )
    )
  )


  manual_test_table <- tibble::tibble(
    group = rep(c("General Cognition", "Attention/Processing", "Language",
                  "Memory", "Executive Functioning", "Mood"),
                times = c(3, 6, 6, 4, 2, 1)),
    labels = factor(
      c(
        "CDR Global (2 SOB)",
        "MoCA Blind",
        "TICS-m",
        "Oral Trailmaking Part A - Completion Time",
        "Oral Trailmaking Part A - Errors",
        "Number Span Forward - Total",
        "Number Span Forward - Span Length",
        "Number Span Backward - Total",
        "Number Span Backward - Span Length",
        "Animal Fluency",
        "Vegetable Fluency",
        "F+L Words",
        "F+L+C Words",
        "F Words",
        "L Words",
        "Craft Immediate - Verbatim",
        "Craft Immediate - Paraphrase",
        "Craft Delay - Verbatim (225% retained)",
        "Craft Delay - Paraphrase (87% retained)",
        "Oral Trailmaking Part B - Completion Time",
        "Oral Trailmaking Part B - Errors",
        "GDS-15 (Depression Symptoms)"
      ),
      levels = c(
        "CDR Global (2 SOB)",
        "MoCA",
        "MoCA Blind",
        "TICS-m",
        "Trailmaking Part A (NA errors; NA/24 CL)",
        "Oral Trailmaking Part A - Completion Time",
        "Oral Trailmaking Part A - Errors",
        "Number Span Forward - Total",
        "Number Span Forward - Span Length",
        "Number Span Backward - Total",
        "Number Span Backward - Span Length",
        "WAIS-R Digit Symbol",
        "MINT",
        "Animal Fluency",
        "Vegetable Fluency",
        "F+L Words",
        "F+L+C Words",
        "F Words",
        "L Words",
        "Benson Figure Copy",
        "Benson Delay (NA% retained; Recog = NA)",
        "Craft Immediate - Verbatim",
        "Craft Immediate - Paraphrase",
        "Craft Delay - Verbatim (225% retained)",
        "Craft Delay - Paraphrase (87% retained)",
        "RAVLT Total Learning (NA,NA,NA,NA,NA)",
        "RAVLT Distractor List",
        "RAVLT Short Delay",
        "RAVLT Long Delay",
        "RAVLT Recognition (NA, NA)",
        "Trailmaking Part B (NA errors; NA/24 CL)",
        "Clock Drawing Test",
        "Oral Trailmaking Part B - Completion Time",
        "Oral Trailmaking Part B - Errors",
        "GDS-15 (Depression Symptoms)"
      )
    ),
    name = c(
      "cog_cdr_global",
      "cog_moca_blind",
      "cog_ticsm",
      "cog_otmta_time",
      "cog_otmta_error",
      "cog_nsf_total",
      "cog_nsf_span",
      "cog_nsb_total",
      "cog_nsb_span",
      "cog_animal_flu",
      "cog_veg_flu",
      "cog_fl_flu",
      "cog_flc_flu",
      "cog_f_flu",
      "cog_l_flu",
      "cog_craft_imm_ver",
      "cog_craft_imm_par",
      "cog_craft_delay_verb",
      "cog_craft_delay_par",
      "cog_otmtb_time",
      "cog_otmtb_error",
      "cog_gds15"
    ),
    Raw = c(0.5, 15, 24, 7.07, 0, 7, 6, 6, 4, 15, 7, 24, 9, 14, 22, 8, 8, 18,
            7, 39.5, 2, 4),
    Raw_suffix = c("", "/22", "/50", "&nbspsec", "", "/14", "/9", "/14", "/8",
                   "", "", "", "", "", "",
                   "/44", "/25", "/44", "/25","&nbspsec","","/15"),
    `z-score` = c(NA, NA, NA, 0.19807, 0.04167, 0.09524, 0.07692, 0.28571,
                  -0.07692, -0.17391, -1.68571, 0.08046, NA, 0.39583, 2.36957,
                  -1.26761, -1.06383, 0.57143, -0.86275, 0.35426, -1.92754, NA),
    SS = rep(c(NA, 4, NA), times = c(12, 1, 9)),
    Percentile = c(NA, NA, NA, 57.85039, 51.66178, 53.79371, 53.06576, 61.24515,
                   46.93424, 43.09669, 4.59254, 53.20642, 2, 65.3886, 99.10955,
                   10.24694, 14.37029, 71.61454, 19.41388, 63.84272, 2.69564, NA),
    Description = c(
      "Normal",
      NA,
      NA,
      "Average",
      "Average",
      "Average",
      "Average",
      "Average",
      "Average",
      "Average",
      "Borderline",
      "Average",
      "Impaired",
      "Average",
      "Very Superior",
      "Low Average",
      "Low Average",
      "Average",
      "Low Average",
      "Average",
      "Impaired",
      "Minimal"
    )
  ) # |>
    # dplyr::mutate(
    #   Raw = dplyr::if_else(.data$name == "cog_cdr_global", sprintf("%.1f", .data$Raw), as.character(.data$Raw))
    # )

  expect_equal(
    test_main_table |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, digits = 5))),
    manual_test_table
  )
})
