library("broom")
library("recipes")

cluster <- function(data_with_features) {
  # Here, we assume the data_with_features includes the
  # features: f_mean, f_intercept, f_slope

  unsupervised_recipe <- recipes::recipe(data_with_features) %>%
    recipes::update_role(f_mean, f_intercept, f_slope,
                         new_role = "predictor") %>%
    recipes::update_role(recipes::has_role(NA), new_role = "crap") %>%
    recipes::add_role(child_id, caregiver_id, new_role = "id", new_type = "nominal") %>%
    recipes::add_role(child_num, new_role = "sequence", new_type = "ordinal") %>%
    recipes::add_role(caregiver_birth_date, child_birth_date, new_role = "birth date", new_type = "date") %>%
    recipes::update_role(num_children, num_screens, new_role = "spectator") %>%
    recipes::add_role(screens, new_role = "screenings") %>%
    recipes::remove_role(recipes::has_role("crap"), old_role = "crap")

  unsupervised_recipe %>%
    summary() %>%
    print()

  pipeline <- unsupervised_recipe %>%
    recipes::step_rm(caregiver_id, caregiver_birth_date, num_children, child_num, child_birth_date, num_screens, screens) %>%
    recipes::step_center(recipes::all_predictors()) %>%
    recipes::step_scale(recipes::all_predictors())

  print(broom::tidy(pipeline))
  print(summary(pipeline))

  fit_pipeline <- pipeline %>%
    recipes::prep(training = data_with_features)

  print(summary(fit_pipeline))
  print(broom::tidy(fit_pipeline))

  transformed_data <- recipes::bake(fit_pipeline, data_with_features)
}


print(cluster(drake::readd(sample_sim_features_data)))
