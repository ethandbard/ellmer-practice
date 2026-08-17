# Golden tests for the automatic opening view.
#
# Golden tests for the automatic opening view.
# default_view() is profile-driven (auto_view). These pin the current choices.

skip_if_no_ggplot2 <- function() {
  skip_if_not_installed("ggplot2")
}

orders_frame <- function() {
  d <- read.csv(app_path("data", "orders.csv"), stringsAsFactors = FALSE)
  d$order_date <- as.Date(d$order_date)
  d$year <- as.integer(format(d$order_date, "%Y"))
  d$month <- as.Date(format(d$order_date, "%Y-%m-01"))
  d
}

test_that("orders opens on monthly revenue by channel", {
  v <- default_view(orders_frame())
  expect_equal(v$spec$geom, "line")
  expect_equal(v$spec$x, "month")
  expect_equal(v$spec$y, "revenue")
  expect_equal(v$spec$color, "channel")
  expect_match(v$sql, "GROUP BY month, channel")
})

test_that("a date column with enough span produces a monthly trend", {
  # food_delivery is not a bundled catalog preset; this is the general path.
  v <- default_view(read.csv(app_path("data", "food_delivery.csv"), stringsAsFactors = FALSE))
  expect_equal(v$spec$geom, "line")
  expect_equal(v$spec$x, "month")
  expect_match(v$sql, "date_trunc\\('month'")
})

test_that("two measures plus a category open as a scatter", {
  v <- default_view(as.data.frame(datasets::iris))
  expect_equal(v$spec$geom, "point")
  expect_true(v$spec$x %in% c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
  expect_true(v$spec$y %in% c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
  expect_false(identical(v$spec$x, v$spec$y))
  expect_true(v$spec$facet == "Species" || v$spec$color == "Species")
})

test_that("mtcars treats cyl as a category, not a measure", {
  mt <- datasets::mtcars
  mt$model <- rownames(mt)
  rownames(mt) <- NULL
  prof <- profile_table(mt)
  expect_equal(prof$role[prof$name == "cyl"], "category")

  v <- default_view(mt)
  expect_true(v$spec$geom %in% c("point", "col"))
})

test_that("Anscombe opens as y vs x, split by dataset", {
  # The quartet exists to show four datasets with near-identical summary stats
  # and completely different shapes. Aggregating x by dataset hid that.
  v <- default_view(read.csv(app_path("data", "anscombe-quartet.csv"), stringsAsFactors = FALSE))

  expect_equal(v$spec$geom, "point")
  expect_equal(v$spec$x, "x")
  expect_equal(v$spec$y, "y")
  expect_true(v$spec$facet == "dataset" || v$spec$color == "dataset")
})

test_that("mpg scatter survives renaming displ", {
  skip_if_no_ggplot2()
  mpg <- as.data.frame(ggplot2::mpg)

  curated <- default_view(mpg)
  expect_equal(curated$spec$geom, "point")
  expect_equal(curated$spec$x, "displ")

  renamed <- mpg
  names(renamed)[names(renamed) == "displ"] <- "engine_litres"
  fallback <- default_view(renamed)
  expect_equal(fallback$spec$geom, "point")
  expect_equal(fallback$spec$x, "engine_litres")
})

test_that("default_view degrades safely on empty and single-column input", {
  empty <- default_view(data.frame())
  expect_true(nzchar(empty$sql))
  expect_false(is.null(empty$spec))

  one <- default_view(data.frame(x = 1:10))
  expect_false(is.null(one$spec$geom))
})

test_that("default_model_spec_for only forecasts on a real calendar", {
  # orders has a genuine month column spanning >40 days.
  expect_equal(default_model_spec_for(orders_frame())$method, "forecast")

  # iris has no clock at all, so it must not claim to forecast.
  expect_equal(default_model_spec_for(as.data.frame(datasets::iris))$method, "regression")

  # An integer `year` column is not a forecast clock.
  yearly <- data.frame(year = 2000:2020, value = runif(21))
  expect_equal(default_model_spec_for(yearly)$method, "regression")
})
