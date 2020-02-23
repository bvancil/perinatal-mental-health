library("dplyr")
# library("lubridate")
library("magrittr")
library("purrr")
library("tibble")
library("tidyr")

mean_score <- function(screens) {
  screens %>%
    dplyr::pull(score) %>%
    base::mean()
}


generate_simple_features <- function(.data) {
  .data %>%
    tidyr::nest(screens = c(screen_id, screen_num, screen_day, screen_date, score)) %>%
    dplyr::mutate(
      feature_mean = purrr::map_dbl(screens, mean_score)
    )
}

test_simple_features <- function() {
  drake::readd(sample_sim_screen_data) %>%
    generate_simple_features() %>%
    dplyr::select(
      -caregiver_id,
      -caregiver_birth_date,
      -num_children,
      -child_num,
      -child_birth_date,
      # -num_screens,
      -screens
    ) %>%
    print()
}

test_simple_features()
