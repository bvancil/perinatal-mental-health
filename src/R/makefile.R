library("drake")
library("styler")

base::message("Building project...")

base::source("./src/R/simulate_data.R", encoding = "UTF-8")

plan <- drake::drake_plan(
  style_project = styler::style_dir(
    drake::file_in("./src/R"),
    style = styler::tidyverse_style
  ),
  # Parameters for simulated screening data
  sim_num_caregivers = 1000L,
  sim_mean_num_children = 1.3,
  sim_mean_screens = 5,
  sim_screen_begin_days = -14L,
  sim_screen_end_days = 365L,
  # Simulate screening data
  sim_screen_data = simulate_screenings(
    num_caregivers = sim_num_caregivers,
    mean_num_children = sim_mean_num_children,
    mean_screens = sim_mean_screens,
    begin_days = sim_screen_begin_days,
    end_days = sim_screen_end_days
  ),
  sample_sim_screen_data = sample_children(sim_screen_data, 12L),
  plot_sample_sim_screen_data = plot_screens_by(sample_sim_screen_data, child_id)
)

drake::make(plan)

base::message("Build completed successfully.")
