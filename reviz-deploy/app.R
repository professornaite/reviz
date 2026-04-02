if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

if (!requireNamespace("critstats", quietly = TRUE)) {
  remotes::install_github("professornaite/critstats", force = TRUE)
}

library(reviz)
launch_reviz()
