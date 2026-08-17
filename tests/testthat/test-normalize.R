# Characterization tests for the value-normalizing layer.
# These pin CURRENT behaviour so the Phase 1-3 refactor cannot drift silently.

test_that("empty_to_null folds blanks and null-ish words to NULL", {
  expect_null(empty_to_null(NULL))
  expect_null(empty_to_null(character()))
  expect_null(empty_to_null(""))
  expect_null(empty_to_null("   "))

  # The model routinely sends these as literal strings.
  expect_null(empty_to_null("null"))
  expect_null(empty_to_null("NONE"))
  expect_null(empty_to_null("na"))
  expect_null(empty_to_null("n/a"))
  expect_null(empty_to_null("undefined"))

  expect_equal(empty_to_null("revenue"), "revenue")
  expect_equal(empty_to_null("  revenue  "), "revenue")
})

test_that("empty_to_null keeps only the first element", {
  expect_equal(empty_to_null(c("a", "b")), "a")
})

test_that("normalize_key strips case and punctuation", {
  expect_equal(normalize_key("Value Desc"), "valuedesc")
  expect_equal(normalize_key("free_y"), "freey")
  expect_equal(normalize_key("3 months"), "3months")
  expect_null(normalize_key(""))
  expect_null(normalize_key("none"))
})

test_that("is_yes accepts the affirmative spellings a model emits", {
  for (v in c("yes", "TRUE", "y", "1", "on", "show", "labels")) {
    expect_true(is_yes(v), info = v)
  }
  for (v in c("no", "false", "0", "", "off")) {
    expect_false(is_yes(v), info = v)
  }
})

test_that("match_column resolves names case- and punctuation-insensitively", {
  df <- data.frame(order_date = 1, `Unit Price` = 2, revenue = 3, check.names = FALSE)

  expect_equal(match_column("revenue", df), "revenue")
  expect_equal(match_column("REVENUE", df), "revenue")
  expect_equal(match_column("order date", df), "order_date")
  expect_equal(match_column("unit_price", df), "Unit Price")

  expect_null(match_column("nope", df))
  expect_null(match_column("", df))
  expect_null(match_column("revenue", NULL))
})

test_that("r_name backtick-quotes only non-syntactic names", {
  expect_equal(r_name("revenue"), "revenue")
  expect_equal(r_name("order_date"), "order_date")
  expect_equal(r_name("Unit Price"), "`Unit Price`")
  expect_equal(r_name("2024"), "`2024`")
})

test_that("sql_ident double-quotes and escapes embedded quotes", {
  expect_equal(sql_ident("revenue"), '"revenue"')
  expect_equal(sql_ident('we"ird'), '"we""ird"')
})

test_that("n_distinct_safe ignores NA", {
  expect_equal(n_distinct_safe(c(1, 1, 2, NA)), 2)
  expect_equal(n_distinct_safe(c(NA, NA)), 0)
  expect_equal(n_distinct_safe(character()), 0)
})

# --- %or%: absent-or-blank coalescing ---

test_that("%or% falls back for NULL, empty, and blank strings", {
  expect_equal(NULL %or% "fallback", "fallback")
  expect_equal(character() %or% "fallback", "fallback")
  expect_equal("" %or% "fallback", "fallback")
})

test_that("%or% passes through present values", {
  expect_equal("revenue" %or% "fallback", "revenue")
  expect_equal(0 %or% "fallback", 0)
  expect_equal(FALSE %or% "fallback", FALSE)
})

test_that("%or% handles multi-element input without erroring", {
  # `is.character(x) && !nzchar(x)` used to evaluate `&&` against a length-n
  # logical, which is an error in R >= 4.3. A vector is present, so it passes
  # through.
  expect_equal(c("a", "b") %or% "fallback", c("a", "b"))
  expect_equal(c(1, 2, 3) %or% "fallback", c(1, 2, 3))
})

test_that("%or% is deliberately distinct from base R's null-only %||%", {
  # Base treats "" as present; ours treats it as absent. The rename exists so
  # nobody reads one and gets the other.
  expect_equal("" %or% "fallback", "fallback")
  expect_equal(base::`%||%`("", "fallback"), "")
})
