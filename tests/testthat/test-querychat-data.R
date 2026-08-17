# Regression tests for the process-wide-DuckDB staleness bugs:
#
# 1. QueryChat used to be constructed with a data.frame `data_source`,
#    which querychat wraps in a DataFrameSource -- a snapshot frozen at
#    construction time. A session's chat client captures that DataSource
#    object once, when its tools are registered at session start, so a
#    later upload (which reassigns `qc$data_source`) didn't reach an
#    already-running session's `query` tool: it kept answering from the
#    old table while the app's own SQL-driven chart correctly used the
#    new one. Passing the live DBI connection instead builds a DBISource,
#    which re-reads table `data` from the connection on every call --
#    object identity stops mattering because it's a thin proxy, not a copy.
#
# 2. A brand new session (a second tab, or a reload) used to seed its
#    reactiveValues from the literal `starter`/"orders.csv", regardless of
#    what an earlier session had actually loaded into the shared,
#    process-wide table. current_source (app.R) is the fix: it mirrors
#    the live table and every session reads its label/columns/n/preview
#    from there instead of from a hardcoded literal.

test_that("a DBI-backed DataSource re-reads the table after it is replaced", {
  skip_if_not(querychat_supported())
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "data", data.frame(x = 1:3), overwrite = TRUE)
  src <- querychat:::normalize_data_source(con, "data")

  first <- src$get_data()
  expect_equal(nrow(first), 3)
  expect_equal(names(first), "x")

  # Simulate an upload: swap the table contents on the *same* connection,
  # without touching `src`. A DataFrameSource would still show the old
  # rows here; a DBISource must not.
  DBI::dbExecute(con, "CREATE OR REPLACE TABLE data AS SELECT * FROM (VALUES (10, 'a'), (20, 'b')) AS t(y, label)")

  second <- src$get_data()
  expect_equal(nrow(second), 2)
  expect_equal(sort(names(second)), c("label", "y"))
})

test_that("a data.frame-backed DataSource does NOT see a later table swap (documents the bug this replaces)", {
  skip_if_not(querychat_supported())
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "data", data.frame(x = 1:3), overwrite = TRUE)
  stale <- querychat:::normalize_data_source(data.frame(x = 1:3), "data")

  DBI::dbExecute(con, "CREATE OR REPLACE TABLE data AS SELECT * FROM (VALUES (10, 'a'), (20, 'b')) AS t(y, label)")

  # The DataFrameSource never touched `con`, so it can't see the swap --
  # this is the failure mode set_querychat_data(qc, df) used to paper over
  # only for *new* sessions, never for one already running.
  still_old <- stale$get_data()
  expect_equal(names(still_old), "x")
})

test_that("set_querychat_data rebuilds the DataSource from a live connection", {
  skip_if_not(querychat_supported())
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "data", data.frame(x = 1:3), overwrite = TRUE)
  qc <- querychat::QueryChat$new(con, table_name = "data", tools = "query")

  DBI::dbExecute(con, "CREATE OR REPLACE TABLE data AS SELECT * FROM (VALUES (10, 'a'), (20, 'b')) AS t(y, label)")
  expect_true(set_querychat_data(qc, con))

  refreshed <- qc$data_source$get_data()
  expect_equal(sort(names(refreshed)), c("label", "y"))
})
