Sys.setenv(RENV_CONFIG_ACTIVE = "FALSE")

if (!dir.exists("R-lib")) dir.create("R-lib")
.libPaths(c(normalizePath("R-lib"), .libPaths()))

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

if (!requireNamespace("critstats", quietly = TRUE)) {
  remotes::install_github("professornaite/critstats", force = TRUE, upgrade = "never")
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("MASS", quietly = TRUE)) {
  install.packages("MASS")
}

library(shiny)
library(ggplot2)
library(MASS)
reviz::launch_reviz()
