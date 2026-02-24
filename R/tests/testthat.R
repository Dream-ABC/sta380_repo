library(testthat)

# Find project root (directory containing DESCRIPTION or R/)
find_root <- function() {
  w <- getwd()
  for (i in 1:20) {
    if (file.exists(file.path(w, "DESCRIPTION")) || dir.exists(file.path(w, "R")))
      return(w)
    w <- dirname(w)
  }
  getwd()
}

pkg_root <- find_root()
if (requireNamespace("sta380project", quietly = TRUE)) {
  library(sta380project)
  test_check("sta380project")
} else {
  r_dir <- file.path(pkg_root, "R")
  if (dir.exists(r_dir)) {
    for (f in list.files(r_dir, pattern = "\\.R$", full.names = TRUE))
      source(f, local = .GlobalEnv)
  }
  test_dir(file.path(pkg_root, "tests", "testthat"))
}
