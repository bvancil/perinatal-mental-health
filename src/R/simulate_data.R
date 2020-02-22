library("dplyr")
library("lubridate")
library("magrittr")
library("purrr")
library("tibble")
library("tidyr")

possible_caregiver_dates_of_birth <- function() {
  base::seq(
    lubridate::ymd(19800101),
    lubridate::ymd(19991231),
    by = "day"
  )
}

possible_caregiver_ages <- function() {
  # In years
  list(min = 18, max = 40)
}

possible_screening_days <- function(begin_days, end_days) {
  seq(begin_days, end_days)
}

possible_scores <- function() {
  base::seq(0L, 30L)
}

simulate_num_children <- function(num_caregivers, mean_num_children) {
  1L + stats::rpois(num_caregivers, mean_num_children - 1)
}

simulate_caregivers <- function(num_caregivers, mean_num_children) {
  tibble::tibble(
    caregiver_id = base::seq_len(num_caregivers),
    caregiver_birth_date = base::sample(
      possible_caregiver_dates_of_birth(),
      num_caregivers
    ),
    num_children = simulate_num_children(num_caregivers, mean_num_children)
  )
}

simulate_duration_to_birth <- function(num_children, caregiver_ages) {
  stats::runif(num_children, caregiver_ages$min, caregiver_ages$max) %>%
    lubridate::dyears()
}

simulate_children <- function(caregiver_birth_date, num_children, caregiver_ages, mean_screens) {
  tibble::tibble(
    duration_to_birth = simulate_duration_to_birth(num_children, caregiver_ages)
  ) %>%
    dplyr::mutate(
      child_birth_date =
        lubridate::as_date(caregiver_birth_date + duration_to_birth)
    ) %>%
    dplyr::select(-duration_to_birth) %>%
    dplyr::arrange(child_birth_date) %>%
    tibble::rowid_to_column(var = "child_num") %>%
    dplyr::mutate(
      num_screens = 1L + stats::rpois(base::nrow(.), mean_screens - 1)
    ) %>%
    dplyr::select(child_num, child_birth_date, num_screens)
}

simulate_screening_days <- function(size, screen_day_set) {
  base::sample(screen_day_set, size, replace = FALSE)
}

simulate_scores <- function(size, score_set) {
  base::sample(score_set, size, replace = TRUE)
}

simulate_screens <- function(child_birth_date, num_screens, screen_day_set, score_set) {
  tibble::tibble(
    screen_day = simulate_screening_days(num_screens, screen_day_set),
    screen_date = child_birth_date + lubridate::ddays(screen_day)
  ) %>%
    dplyr::arrange(screen_date) %>%
    tibble::rowid_to_column(var = "screen_num") %>%
    dplyr::mutate(
      score = simulate_scores(num_screens, score_set = score_set)
    )
}

simulate_screenings <- function(num_caregivers, mean_num_children, mean_screens, begin_days, end_days) {
  caregivers <- simulate_caregivers(num_caregivers, mean_num_children)

  caregiver_child_pairs <- caregivers %>%
    dplyr::mutate(
      children = purrr::pmap(
        list(
          caregiver_birth_date = caregiver_birth_date,
          num_children = num_children
        ),
        # TODO: Fix birth spacing, which could be non-existant
        simulate_children,
        caregiver_ages = possible_caregiver_ages(),
        mean_screens = mean_screens
      )
    ) %>%
    tidyr::unnest(children) %>%
    tibble::rowid_to_column(var = "child_id")

  caregiver_child_screens <- caregiver_child_pairs %>%
    dplyr::mutate(
      screens = purrr::pmap(
        list(
          child_birth_date = child_birth_date,
          num_screens = num_screens
        ),
        # TODO: Fix birth spacing, which could be non-existant
        simulate_screens,
        screen_day_set = possible_screening_days(begin_days, end_days),
        score_set = possible_scores()
      )
    ) %>%
    tidyr::unnest(screens) %>%
    tibble::rowid_to_column(var = "screen_id")

  caregiver_child_screens %>%
    tsibble::as_tsibble(key = child_id, index = screen_day)
}

sample_children <- function(.data, size) {
  sampled_children <- .data %>%
    dplyr::pull(child_id) %>%
    base::unique() %>%
    base::sample(size, replace = FALSE)

  .data %>%
    dplyr::filter(child_id %in% sampled_children)
}

plot_screens_by <- function(.data, ...) {
  ggplot2::ggplot(.data, ggplot2::aes(screen_day, score)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(vars(...)) +
    ggplot2::theme_minimal()
}

test_sim_screens <- function() {
  sim_screens <- simulate_screenings(5L, 1.3, 5, -14L, 365L)
  print(sim_screens, n = 30L, width = 200L)
}

# test_sim_screens()
