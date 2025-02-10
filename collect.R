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

# ------------------------------------------------------------------------------
# accuracy, sens, spec


# ------------------------------------------------------------------------------
# internal versus external


