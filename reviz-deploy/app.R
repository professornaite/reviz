if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

if (!requireNamespace("critstats", quietly = TRUE)) {
  remotes::install_github("professornaite/critstats", force = TRUE)
}

if (!requireNamespace("reviz", quietly = TRUE)) {
  install.packages("../reviz_0.1.0.tar.gz", repos = NULL, type = "source")
}

library(reviz)
reviz::launch_reviz()
