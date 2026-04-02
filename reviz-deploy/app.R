Sys.setenv(RENV_CONFIG_ACTIVE = "FALSE")

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

if (!requireNamespace("critstats", quietly = TRUE)) {
  remotes::install_github("professornaite/critstats", force = TRUE)
}

if (!requireNamespace("reviz", quietly = TRUE)) {
  remotes::install_github("professornaite/reviz", force = TRUE)
}

library(reviz)

reviz::launch_reviz()
