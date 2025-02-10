
library(tidymodels)
library(glue)
library(stringr)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------

template <- readLines("template.R")

# ------------------------------------------------------------------------------

num_sim <- 20

set.seed(1) # batch of 20 started on 2025-02-10

combinations <-
  crossing(
    num_extra = 10 * c(0, 2, 4, 6, 8, 10),
    num_train = 3:6,
    num_repeat = c(1, 3, 5),
    seed = sample.int(10000, num_sim)
  ) %>%
  mutate(
    file = glue("files/sim_{num_extra}_{num_train}_{num_repeat}_{seed}.R")
  )

new_file <- function(x, template) {
  template <- gsub("NTRAIN", x$num_train, template)
  template <- gsub("REPEATS", x$num_repeat, template)
  template <- gsub("EXTRA", x$num_extra, template)
  template <- gsub("SEED", x$seed, template)
  cat(template, sep = "\n", file = x$file)
  invisible(NULL)
}

for (i in 1:nrow(combinations)) {
  new_file(combinations[i,], template)
}

# ------------------------------------------------------------------------------


existing <- list.files(path = "files", pattern = "*.*RData$")
excluding <- gsub("Data$", "", existing)
excluding <- gsub("^res", "sim", excluding)
src_files <- list.files(path = "files", pattern = "*.*R$")
src_files <- src_files[!(src_files %in% excluding)]

src_files <- sample(src_files)
rda_files <- gsub("R$", "RData", src_files)

# target_list <-
#   paste(rda_files, collapse = " ") %>%
#   str_wrap() %>%
#   str_split("\n")

# target_list <- paste0("\t", target_list[[1]], collapse = " \\ \n")

target_list <- paste0(rda_files, collapse = " ")

target_list <- paste0("all: ", target_list, "\n\n")

instruct <- function(src_file) {
  glue(
    "
{src_file}Data: {src_file}
\t@date '+ %Y-%m-%d %H:%M:%S: + {src_file}'
\t@$(RCMD) BATCH --vanilla {src_file}
\t@date '+ %Y-%m-%d %H:%M:%S: - {src_file}'
\t@sleep 20

"
  )
}

instructions <- map_chr(src_files, instruct)
instructions <- paste0(instructions, collapse = "\n")

header <-
  "SHELL = /bin/bash
R    ?= R
RCMD  =@$(R) CMD
TIMESTAMP = $(shell  date '+%Y-%m-%d-%H-%M')
here=${PWD}/..
"

cat(header, file = "files/makefile", sep = "")

cat(target_list, file = "files/makefile", append = TRUE, sep = "")
cat(instructions, file = "files/makefile", append = TRUE, sep = "")

