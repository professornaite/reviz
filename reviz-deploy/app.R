Sys.setenv(RENV_CONFIG_ACTIVE = "FALSE")

if (!dir.exists("R-lib")) dir.create("R-lib")
.libPaths(c(normalizePath("R-lib"), .libPaths()))

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

if (!requireNamespace("critstats", quietly = TRUE)) {
  remotes::install_github("professornaite/critstats", force = TRUE, upgrade = "never")
}

if (!requireNamespace("reviz", quietly = TRUE)) {
  remotes::install_github("professornaite/reviz", force = TRUE, upgrade = "never")
}

library(reviz)
reviz::launch_reviz()
