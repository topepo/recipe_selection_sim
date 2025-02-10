time_start <- proc.time()
library(tidymodels)
library(colino)
library(future.mirai)

# ------------------------------------------------------------------------------

tidymodels_prefer()
conflicted::conflicts_prefer(recipes::update)
plan("mirai_multisession")
options(future.globals.maxSize = 1.0 * 1e9)  ## 1.0 GB

# ------------------------------------------------------------------------------

n_train <- 10^6
n_test <- 10^6
num_extra <- 0
num_repeat <- 1
seed_val <- 8789
grid_size <- 40

file_nm <- glue::glue("res_{num_extra}_{n_train}_{num_repeat}_{seed_val}.RData")

# ------------------------------------------------------------------------------

get_stats <- function(fit, res, te, ...) {
  if (inherits(fit, "workflow")) {
    cls <-
      fit %>%
      extract_fit_parsnip() %>%
      class() %>%
      pluck(1)
  } else {
    cls <- class(fit)[1]
  }

  perf <-
    fit %>%
    augment(sim_te) %>%
    rmse(outcome, .pred) %>%
    pluck(".estimate")

  rs <-
    show_best(res, metric = "rmse", n = 1) %>%
    dplyr::select(-.metric, -.estimator, -n, -std_err, -.config,
                  RMSE_internal = mean) %>%
    relocate(RMSE_internal)

  var_res <- sens_spec(fit, names(te), ...)
  var_res$RMSE_external <- perf
  var_res$class <- gsub("^_", "", cls)
  bind_cols(var_res, rs)
}

sens_spec <- function(fit, cols, ...) {
  num_events <- sum(grepl("predictor_", cols))
  num_nonevents <- sum(grepl("noise_", cols))

  used <-
    fit %>%
    extract_mold() %>%
    pluck("predictors") %>%
    names()

  sens <- sum(grepl("predictor_", used)) / num_events
  if (num_nonevents > 0) {
    spec <- 1 - ( sum(grepl("noise_", used)) / num_nonevents )
  } else {
    spec <- NA_real_
  }
  tibble::tibble(sensitivity = sens, specificity = spec,
                 num_used = length(used), vars = list(used))
}

# ------------------------------------------------------------------------------

set.seed(seed_val)
sim_tr <- sim_regression(n_train, method = "hooker_2004")
sim_te <- sim_regression(n_test,  method = "hooker_2004")

if (num_extra > 0) {
  set.seed(seed_val + 1)
  sim_tr <- sim_tr %>% bind_cols(sim_noise(n_train, num_extra))
  sim_te <- sim_te %>% bind_cols(sim_noise(n_test,  num_extra))
}

num_pred <- ncol(sim_tr) - 1

set.seed(seed_val + 2)
sim_rs <- vfold_cv(sim_tr, repeats = num_repeat)

# ------------------------------------------------------------------------------

sim_rec <-
  recipe(outcome ~ ., data = sim_tr) %>%
  step_select_relief(all_predictors(), outcome = "outcome", top_p = tune()) %>%
  step_normalize(all_predictors())

# ------------------------------------------------------------------------------

ctrl_grid <- control_grid(save_workflow = TRUE)

# ------------------------------------------------------------------------------

mlp_spec <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = 1000,
      activation = tune(), learn_rate = tune()) %>%
  set_engine("brulee", stop_iter = tune()) %>%
  set_mode("regression")

mlp_wflow <- workflow(sim_rec, mlp_spec)

mlp_prm <-
  mlp_wflow %>%
  extract_parameter_set_dials() %>%
  update(
    learn_rate = learn_rate(c(-3, - 1/ 2)),
    top_p = top_p(c(1, num_pred))
  )

set.seed(seed_val + 3)
mlp_res <-
  mlp_wflow %>%
  tune_grid(
    resamples = sim_rs,
    grid = grid_size,
    control = ctrl_grid,
    param_info = mlp_prm
  )

mlp_fit <- fit_best(mlp_res, metric = "rmse")
mlp_stat <- get_stats(mlp_fit, mlp_res, sim_te)

# ------------------------------------------------------------------------------

time_stop <- proc.time()

res <-
  mlp_stat %>%
  mutate(
    seed = seed_val,
    num_extra = num_extra,
    n_train = n_train,
    num_repeat = num_repeat,
    elapsed = time_stop[3] - time_start_[3]
  )

save(res, file = file_nm)

# ------------------------------------------------------------------------------

if (!interactive()) {
  q("no")
}

