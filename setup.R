# One-time setup. Run this file once from the ellmer-practice folder.

pkgs <- c(
  "ellmer",
  "querychat",
  "shiny",
  "bslib",
  "DT",
  "dplyr",
  "readr",
  "ggplot2",
  "plotly",
  "scales",
  "usethis",
  "DBI",
  "duckdb",
  "broom",
  "rsample",
  "yardstick",
  "parsnip",
  "recipes",
  "workflows",
  "glmnet",
  "ranger",
  "rpart.plot",
  "rpart",
  "mgcv",
  "nnet"
)

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing)
}

message("Packages ready: ", paste(pkgs, collapse = ", "))

report_key <- function(name, present) {
  if (present) {
    message(name, " is set (length ", nchar(Sys.getenv(name)), ").")
  } else {
    message(name, " is not set.")
  }
}

has_anthropic <- nzchar(Sys.getenv("ANTHROPIC_API_KEY"))
has_xai <- nzchar(Sys.getenv("XAI_API_KEY"))
report_key("ANTHROPIC_API_KEY", has_anthropic)
report_key("XAI_API_KEY", has_xai)

if (!has_anthropic || !has_xai) {
  message(
    "\nOpening your user .Renviron. Add the missing line(s), save, then restart R:"
  )
  if (!has_xai) {
    message("  XAI_API_KEY=xai-your-key-here")
    message("  Get a key at https://console.x.ai")
  }
  if (!has_anthropic) {
    message("  ANTHROPIC_API_KEY=sk-ant-your-key-here")
    message("  Get a key at https://platform.claude.com/settings/keys")
  }
  message("No quotes. A website subscription is not an API key.")
  usethis::edit_r_environ()
}

if (has_anthropic || has_xai) {
  message("\nOptional: source(\"check-key.R\") to send a tiny test request.")
}

message("\nNext: shiny::runApp()  or click Run App on app.R")
