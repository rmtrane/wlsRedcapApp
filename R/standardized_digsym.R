#' Get standardized WAIS-R Digit Symbol score
#'
#' @param raw_score numeric vector with raw score
#' @param age a numeric vector of same length as `raw_score` with ages of participants
#'
#' @returns A numeric vector of same length as `raw_score` with the standardized scores.
#'
#' @examples
#' standardized_digsym(24, 80)
#'
#' @export

standardized_digsym <- function(
    raw_score,
    age
) {

  ## Checks
  stopifnot(length(raw_score) == length(age))
  stopifnot(is.numeric(raw_score))
  stopifnot(is.numeric(age))


  ## Get age groups
  digsym_age_groups <- get_age_group(age, "digsym")

  ## Get vector of age groups present (no need for calculating SS for age groups
  ## not in data anyway)
  age_groups_present <- unique(digsym_age_groups)

  ## For each participant, find SS for each age group present, regardless of
  ## participants actual age group. (This is faster!)
  all_ss <- lapply(age_groups_present, \(y) {
    digsym_ranges$standardized[
      findInterval(
        raw_score,
        vec = digsym_ranges[[y]]
      )
    ]
  })

  ## Set names of list of all SS. Now, all_ss$A[i] is the SS of participant i
  ## had participant been in age group A. (Note: A will only be in all_ss if
  ## at least one participant is in age group A)
  all_ss <- setNames(all_ss, age_groups_present)

  ## Pull SS for each participant from the age group they belong to
  sapply(1:length(raw_score), \(i) all_ss[[digsym_age_groups[i]]][i])
}


if (FALSE) {
  func_1 <- function() {
    purrr::map(
      age_groups_present,
      \(y) {
        digsym_ranges$standardized[
          findInterval(
            raw_score,
            vec = digsym_ranges[[y]]
          )
        ]
      }
    ) |> rlang::set_names(age_groups_present) |>
      data.frame() |>
      # purrr::list_cbind() |> print()
      dplyr::mutate(
        digsym_age_groups,
        standardized = dplyr::case_match(
          .data$digsym_age_groups,
          'A' ~ A,
          'B' ~ B,
          'C' ~ C,
          'D' ~ D,
          'E' ~ E,
          'F' ~ F,
          .default = NA
        )
      ) |>
      dplyr::pull(.data$standardized)
  }

  func_2 <- function() {
    lapply(age_groups_present, \(y) {
      digsym_ranges$standardized[
        findInterval(
          raw_score,
          vec = digsym_ranges[[y]]
        )
      ]
    })
  }

  microbenchmark::microbenchmark(
    func_1(),
    func_2(),
    times = 10000
  )


}
