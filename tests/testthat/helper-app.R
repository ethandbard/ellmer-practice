# Load the app's functions for testing.
#
# This is not a package, so there is no load_all(). Walk up from the test
# directory to the folder holding app.R, then source R/ in dependency order.

app_root <- local({
  dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
  for (i in seq_len(5)) {
    if (file.exists(file.path(dir, "app.R"))) {
      return(dir)
    }
    dir <- dirname(dir)
  }
  stop("Could not locate app.R above ", normalizePath(".", winslash = "/"))
})

source(file.path(app_root, "R", "utils.R"))
source(file.path(app_root, "R", "profile.R"))
source(file.path(app_root, "R", "sql.R"))
source(file.path(app_root, "R", "theme.R"))
source(file.path(app_root, "R", "dashboard-view.R"))
source(file.path(app_root, "R", "plot-recipe.R"))
source(file.path(app_root, "R", "model-tidymodels.R"))
source(file.path(app_root, "R", "model-view.R"))
source(file.path(app_root, "R", "model-recipe.R"))
source(file.path(app_root, "R", "stats-test.R"))
source(file.path(app_root, "R", "unsupervised.R"))
source(file.path(app_root, "R", "providers.R"))
source(file.path(app_root, "R", "tools.R"))

app_path <- function(...) file.path(app_root, ...)

# A throwaway DuckDB holding `df` as the table `data`, for run_sql() tests.
with_test_con <- function(df, code) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "data", df, overwrite = TRUE)
  code(con)
}

# Small deterministic frames used across several test files.
sample_orders <- function() {
  read.csv(app_path("data", "orders.csv"), stringsAsFactors = FALSE) |>
    transform(
      order_date = as.Date(order_date),
      month = as.Date(format(as.Date(order_date), "%Y-%m-01"))
    )
}
