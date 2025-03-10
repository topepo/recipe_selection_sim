library(tidymodels)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------
# Get results per sim

res_files <- list.files("files", pattern = "RData$", full.names = TRUE)
r_files <- list.files("files", pattern = "R$", full.names = TRUE)
round(length(res_files) / length(r_files) * 100, 1)

get_res <- function(x) {
  load(x)
  res
}

raw_results <- map_dfr(res_files, get_res)
raw_results %>%
  count(seed)

# ------------------------------------------------------------------------------
# Plot the *selected* top_p during tuning

# raw data
raw_results %>%
  filter(num_extra > 0) %>%
  count(num_extra, top_p, num_repeat, num_train) %>%
  ggplot(aes(num_extra, top_p, size = n)) +
  geom_abline(intercept = 9, lty = 3) +
  geom_hline(col = "green", yintercept = 9) +
  geom_point(position = position_dodge(width = 10), alpha = 1 / 4) +
  facet_grid(num_repeat ~ num_train) +
  coord_obs_pred() +
  scale_size(range = c(1/2, 3)) +
  labs(x = "# Noise Predictors", y = "# Top Predictors")

# averages
raw_results %>%
  summarize(
    mean_top_p = mean(top_p),
    n = length(top_p),
    std_err = sd(top_p) / sqrt(n),
    .by = c(num_extra, num_repeat, num_train)
  ) %>%
  filter(num_extra > 0) %>%
  mutate(`#CV Repeats` = format(num_repeat)) %>%
  ggplot(
    aes(
      num_extra,
      mean_top_p,
      col = `#CV Repeats`,
      pch = `#CV Repeats`
    )
  ) +
  geom_abline(intercept = 9, lty = 3) +
  geom_hline(col = "green", yintercept = 9) +
  geom_point(position = position_dodge(width = 10), cex = 1) +
  geom_errorbar(
    aes(ymin = mean_top_p - 1.65 * std_err,
        ymax = mean_top_p + 1.65 * std_err),
    width = 8, position = position_dodge(width = 10)) +
  facet_wrap(~ num_train) +
  coord_obs_pred() +
  scale_color_brewer(palette = "Paired") +
  labs(x = "#Noise Predictors", y = "#Top Predictors")

# ------------------------------------------------------------------------------
# Difference form estimated RMSE versus large sample

rmse_diffs <-
  raw_results %>%
  mutate(
    rmse_fold = RMSE_test / RMSE_resampled,
    pct_optimism = (RMSE_test - RMSE_resampled) / RMSE_test
  )

# rmse_diff_summary <-
#   rmse_diffs %>%
#   summarize(
#     rmse_fold = mean(rmse_fold),
#     std_err = sd(top_p) / sqrt(length(rmse_fold)),
#     .by = c(num_extra, num_repeat, num_train)
#   )

rmse_diffs %>%
  ggplot(aes(num_extra, rmse_fold, col = num_used, pch = format(num_repeat))) +
  geom_hline(yintercept = 1, col = "green", lty = 2) +
  geom_point(alpha = 1 / 2) +
  facet_grid(~ num_train)

rmse_diffs %>%
  ggplot(aes(num_extra, pct_optimism, col = num_used, pch = format(num_repeat))) +
  geom_hline(yintercept = 0, col = "green", lty = 2) +
  geom_point(alpha = 1 / 2) +
  facet_grid(~ num_train) +
  scale_y_continuous(labels = label_percent())

# ------------------------------------------------------------------------------
# Performance as extra are increased

bl_data <-
  raw_results %>%
  filter(num_extra == 0) %>%
  rename(bl_test = RMSE_test, bl_resampled = RMSE_resampled) %>%
  select(starts_with("bl"), num_train, num_repeat, seed)

ext_diff <-
  raw_results %>%
  full_join(bl_data, by = join_by(seed, num_train, num_repeat)) %>%
  mutate(
    ext_pct = (RMSE_test - bl_test) / bl_test,
    int_pct = (RMSE_resampled - bl_resampled) / bl_resampled
  )

ext_diff_summary <-
  ext_diff %>%
  summarize(
    ext_pct = mean(ext_pct),
    ext_std_err = sd(ext_pct) / sqrt(length(ext_pct)),
    int_pct = mean(int_pct),
    int_std_err = sd(int_pct) / sqrt(length(int_pct)),
    .by = c(num_extra, num_repeat, num_train)
  ) %>%
  mutate(num_repeat = factor(num_repeat))

ext_diff_summary %>%
  mutate(`#CV Repeats` = format(num_repeat)) %>%
  ggplot(aes(num_extra, ext_pct, col = `#CV Repeats`, pch = `#CV Repeats`)) +
  geom_point(alpha = 3 / 4, cex = 2) +
  geom_line(alpha = 1 / 4) +
  facet_wrap(~ num_train) +
  scale_y_continuous(labels = label_percent()) +
  labs(x = "#Noise Predictors", y = "External RMSE Inflation")

ext_diff_summary %>%
  mutate(`#CV Repeats` = format(num_repeat)) %>%
  ggplot(aes(num_extra, int_pct, col = `#CV Repeats`, pch = `#CV Repeats`)) +
  geom_point(alpha = 3 / 4, cex = 2) +
  geom_line(alpha = 1 / 4) +
  facet_wrap(~ num_train) +
  scale_y_continuous(labels = label_percent()) +
  labs(x = "#Noise Predictors", y = "Internal RMSE Inflation")

# ------------------------------------------------------------------------------

raw_results %>%
  filter(num_extra > 0) %>%
  mutate(
    specificity = jitter(specificity),
    sensitivity = jitter(sensitivity),
    `#CV Repeats` = format(num_repeat)
  ) %>%
  ggplot(aes(1 - specificity, sensitivity, col = `#CV Repeats`, pch = `#CV Repeats`)) +
  geom_abline(lty = 3, col = "red") +
  geom_point(cex = 1, alpha = 1 / 2) +
  facet_grid(num_train ~ num_extra) +
  lims(x = c(-.05, 1.05), y = c(-.05, 1.05)) +
  scale_shape_manual(values = c(1, 4))

