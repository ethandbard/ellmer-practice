test_that("infer_period_from_dates reads the grain off the median gap", {
  expect_equal(infer_period_from_dates(seq(as.Date("2024-01-01"), by = "day", length.out = 30)), "day")
  expect_equal(infer_period_from_dates(seq(as.Date("2024-01-01"), by = "week", length.out = 20)), "week")
  expect_equal(infer_period_from_dates(seq(as.Date("2024-01-01"), by = "month", length.out = 18)), "month")
  expect_equal(infer_period_from_dates(seq(as.Date("2000-01-01"), by = "year", length.out = 10)), "year")
})

test_that("infer_period_from_dates defaults to month when it cannot tell", {
  expect_equal(infer_period_from_dates(as.Date(c("2024-01-01", "2024-02-01"))), "month")
  expect_equal(infer_period_from_dates(character()), "month")
  expect_equal(infer_period_from_dates(c("not", "a", "date")), "month")
})

test_that("infer_period_from_dates treats an integer year column as yearly", {
  expect_equal(infer_period_from_dates(2000:2015), "year")
})

test_that("is_year_number distinguishes years from other integers", {
  expect_true(is_year_number(c(2000L, 2010L, 2024L)))
  expect_false(is_year_number(c(1L, 2L, 3L)))
  expect_false(is_year_number(c(2000.5, 2010.25)))
  expect_false(is_year_number(as.Date("2024-01-01")))
})

test_that("as_clock_date normalizes the shapes a clock column arrives in", {
  expect_equal(as_clock_date(as.Date("2024-03-01")), as.Date("2024-03-01"))
  expect_equal(as_clock_date("2024-03-01"), as.Date("2024-03-01"))
  expect_equal(as_clock_date(as.POSIXct("2024-03-01 12:00", tz = "UTC")), as.Date("2024-03-01"))
  expect_equal(as_clock_date(2024L), as.Date("2024-01-01"))
})

test_that("advance_dates walks forward from the last observed period", {
  out <- advance_dates(as.Date("2024-06-01"), "month", 3)
  expect_equal(out, as.Date(c("2024-07-01", "2024-08-01", "2024-09-01")))

  expect_length(advance_dates(as.Date("2024-06-01"), "week", 2), 2)
  expect_equal(advance_dates(as.Date("2024-06-01"), "year", 1), as.Date("2025-06-01"))
})

test_that("period_freq maps grain to a seasonal frequency", {
  expect_equal(period_freq("month"), 12)
  expect_equal(period_freq("week"), 52)
  expect_equal(period_freq("day"), 7)
  expect_equal(period_freq("year"), 1)
})

test_that("zoo_na_approx interpolates interior gaps and holds the edges", {
  expect_equal(zoo_na_approx(c(1, NA, 3)), c(1, 2, 3))
  # rule = 2: leading and trailing NAs take the nearest observed value.
  expect_equal(zoo_na_approx(c(NA, 2, 4, NA)), c(2, 2, 4, 4))
  expect_equal(zoo_na_approx(c(1, 2, 3)), c(1, 2, 3))
})

test_that("zoo_na_approx falls back to the mean when there is too little signal", {
  expect_equal(zoo_na_approx(c(NA, 5, NA)), c(5, 5, 5))
})

test_that("regularize_series fills missing periods with zero for additive measures", {
  df <- data.frame(
    period = as.Date(c("2024-01-01", "2024-02-01", "2024-04-01")),
    revenue = c(10, 20, 40)
  )
  out <- regularize_series(df, "period", "revenue", "month", fill_zero = TRUE)

  expect_equal(nrow(out), 4)
  expect_equal(out$y, c(10, 20, 0, 40))
})

test_that("regularize_series interpolates instead of zero-filling for averages", {
  df <- data.frame(
    period = as.Date(c("2024-01-01", "2024-02-01", "2024-04-01")),
    rating = c(4, 5, 3)
  )
  out <- regularize_series(df, "period", "rating", "month", fill_zero = FALSE)

  expect_equal(nrow(out), 4)
  expect_equal(out$y[[3]], 4) # midpoint of 5 and 3
})

test_that("regularize_series sums duplicate timestamps", {
  df <- data.frame(
    period = as.Date(c("2024-01-01", "2024-01-01", "2024-02-01")),
    revenue = c(10, 5, 20)
  )
  out <- regularize_series(df, "period", "revenue", "month", fill_zero = TRUE)
  expect_equal(out$y, c(15, 20))
})

test_that("baseline_future repeats the last value, or the last season", {
  y <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)

  expect_equal(baseline_future(y, 3, 12, seasonal = FALSE), c(13, 13, 13))
  # Seasonal naive reaches back one full period.
  expect_equal(baseline_future(y, 2, 12, seasonal = TRUE), c(2, 3))
})

test_that("clip_nonnegative floors forecasts at zero only when history is non-negative", {
  res <- list(
    forecast = data.frame(y = c(-5, 10), lo = c(-20, -1), hi = c(5, 20)),
    series = data.frame(part = c("forecast", "forecast"), y = c(-5, 10), lo = c(-20, -1), hi = c(5, 20))
  )

  clipped <- clip_nonnegative(res, history = c(1, 2, 3))
  expect_equal(clipped$forecast$y, c(0, 10))
  expect_equal(clipped$forecast$lo, c(0, 0))

  # History that legitimately goes negative (profit, temperature) is left alone.
  untouched <- clip_nonnegative(res, history = c(-1, 2, 3))
  expect_equal(untouched$forecast$y, c(-5, 10))
})

test_that("build_forecast_sql aggregates to one row per period", {
  sql <- build_forecast_sql("order_date", "revenue", "month", "SUM")
  expect_match(sql, "date_trunc\\('month'")
  expect_match(sql, "SUM\\(\"revenue\"\\)")
  expect_match(sql, "GROUP BY 1")
  expect_match(sql, "ORDER BY 1")
})
