test_that("is_read_only_sql admits SELECT and WITH ... SELECT", {
  expect_true(is_read_only_sql("SELECT * FROM data"))
  expect_true(is_read_only_sql("select region, sum(revenue) as revenue from data group by 1"))
  expect_true(is_read_only_sql("WITH t AS (SELECT * FROM data) SELECT * FROM t"))
  expect_true(is_read_only_sql("  SELECT 1  "))

  # Absent SQL is not a violation - it means "no query for this pane".
  expect_true(is_read_only_sql(NULL))
  expect_true(is_read_only_sql(""))
})

test_that("is_read_only_sql rejects statements that write", {
  expect_false(is_read_only_sql("DROP TABLE data"))
  expect_false(is_read_only_sql("INSERT INTO data VALUES (1)"))
  expect_false(is_read_only_sql("UPDATE data SET revenue = 0"))
  expect_false(is_read_only_sql("DELETE FROM data"))
  expect_false(is_read_only_sql("CREATE TABLE t AS SELECT 1"))
  expect_false(is_read_only_sql("ATTACH 'x.db'"))
  expect_false(is_read_only_sql("PRAGMA database_list"))
})

test_that("is_read_only_sql rejects a write smuggled after a leading SELECT", {
  expect_false(is_read_only_sql("SELECT 1; DROP TABLE data"))
})

test_that("string literals are not treated as write keywords", {
  # Comments and quotes are stripped before the keyword check, so a value
  # like 'create' is a legal read. A second in-process DuckDB handle is not
  # actually read-only on Windows, so this is the structural gate.
  expect_true(is_read_only_sql("SELECT * FROM data WHERE action = 'create'"))
  expect_true(is_read_only_sql("SELECT * FROM data WHERE note = 'delete'"))

  expect_true(is_read_only_sql("SELECT created_at FROM data"))
  expect_true(is_read_only_sql("SELECT updated_by FROM data"))
})

test_that("run_sql returns a used=FALSE envelope for absent SQL", {
  with_test_con(data.frame(a = 1:3), function(con) {
    res <- run_sql(con, "")
    expect_true(res$ok)
    expect_false(res$used)
    expect_null(res$df)
  })
})

test_that("run_sql executes and reports failures without throwing", {
  with_test_con(data.frame(region = c("W", "E"), revenue = c(10, 20)), function(con) {
    ok <- run_sql(con, "SELECT region, revenue FROM data ORDER BY region")
    expect_true(ok$ok)
    expect_true(ok$used)
    expect_equal(nrow(ok$df), 2)
    expect_equal(ok$df$region, c("E", "W"))

    bad <- run_sql(con, "SELECT nope FROM data")
    expect_false(bad$ok)
    expect_true(bad$used)
    expect_null(bad$df)
    expect_true(nzchar(bad$error))

    blocked <- run_sql(con, "DROP TABLE data")
    expect_false(blocked$ok)
    expect_match(blocked$error, "SELECT")
  })
})

test_that("format_sql breaks clauses onto their own lines", {
  out <- format_sql("SELECT a, b FROM data WHERE a > 1 GROUP BY a ORDER BY b")
  lines <- strsplit(out, "\n", fixed = TRUE)[[1]]

  expect_true(any(grepl("^FROM", lines)))
  expect_true(any(grepl("^WHERE", lines)))
  expect_true(any(grepl("^GROUP BY", lines)))
  expect_true(any(grepl("^ORDER BY", lines)))

  # A multi-column SELECT list is split one column per line.
  expect_true(any(grepl("^  a,", lines)) || any(grepl("^  a$", lines)))
})

test_that("pretty_sql labels the absent case", {
  expect_equal(pretty_sql(""), "(none for this question)")
  expect_equal(pretty_sql(NULL), "(none for this question)")
})

test_that("coerce_result_df parses ISO date strings and drops POSIXt to Date", {
  df <- data.frame(
    d = c("2024-01-01", "2024-02-01", "2024-03-01"),
    ts = as.POSIXct(c("2024-01-01", "2024-02-01", "2024-03-01"), tz = "UTC"),
    label = c("alpha", "beta", "gamma"),
    n = 1:3,
    stringsAsFactors = FALSE
  )
  out <- coerce_result_df(df)

  expect_s3_class(out$d, "Date")
  expect_s3_class(out$ts, "Date")
  expect_type(out$label, "character")
  expect_type(out$n, "integer")
})

test_that("coerce_result_df leaves non-date character columns alone", {
  df <- data.frame(code = c("2024-ab-01", "x", "y"), stringsAsFactors = FALSE)
  expect_type(coerce_result_df(df)$code, "character")
})
