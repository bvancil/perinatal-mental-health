library("broom")
library("dplyr")
# library("lubridate")
library("magrittr")
library("parsnip")
library("purrr")
library("tibble")
library("tidyr")

prepare_features <- function(screens) {
  simple_features <- screens %>%
    # Remove the tsibble-ness (key, index)
    tibble::as_tibble() %>%
    dplyr::select(screen_day, score) %>%
    dplyr::mutate(
      f_mean = score
    ) %>%
    dplyr::select(-screen_day, -score) %>%
    dplyr::summarise_all(base::mean)

  linear_model <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm") %>%
    parsnip::fit(score ~ screen_day, data = screens)

  linear_features <- tibble::tibble(
    f_intercept = linear_model$fit$coefficients[["(Intercept)"]],
    f_slope = linear_model$fit$coefficients[["screen_day"]]
  )

  dplyr::bind_cols(simple_features, linear_features)
}


generate_simple_features <- function(.data) {
  .data %>%
    tidyr::nest(screens = c(screen_id, screen_num, screen_day, screen_date, score)) %>%
    dplyr::mutate(
      features = purrr::map(screens, prepare_features)
    ) %>%
    tidyr::unnest(features)
}

drop_uninteresting_columns <- . %>%
  dplyr::select(
    -caregiver_id,
    -caregiver_birth_date,
    -num_children,
    -child_num,
    -child_birth_date,
    # -num_screens,
    -screens
  )

test_simple_features_1 <- function() {
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

test_simple_features_2 <- function() {
  with_features <-
    drake::readd(sample_sim_screen_data) %>%
    generate_simple_features() %>%
    dplyr::slice(1L)
  print(with_features$screens)
  with_features %>%
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

# test_simple_features_1()
# test_simple_features_2()
