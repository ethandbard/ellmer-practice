# The resolve_* layer maps whatever the model sends onto the app's vocabulary.
# Until Phase 3 puts typed enums on the tool schema, this layer IS the contract,
# so it has to tolerate the casing and spacing an LLM naturally produces.

test_that("resolve_geom accepts canonical names", {
  for (g in c("col", "line", "point", "histogram", "boxplot", "density", "area", "violin", "jitter")) {
    expect_equal(resolve_geom(g), g, info = g)
  }
})

test_that("resolve_geom accepts aliases", {
  expect_equal(resolve_geom("bar"), "col")
  expect_equal(resolve_geom("bars"), "col")
  expect_equal(resolve_geom("scatter"), "point")
  expect_equal(resolve_geom("hist"), "histogram")
  expect_equal(resolve_geom("box"), "boxplot")
  expect_equal(resolve_geom("kde"), "density")
  expect_equal(resolve_geom("violin"), "violin")
  expect_equal(resolve_geom("heatmap"), "tile")
})

test_that("resolve_geom is case-insensitive", {
  # A model that writes "Line" or "Bar Chart" must not silently lose the geom.
  expect_equal(resolve_geom("Line"), "line")
  expect_equal(resolve_geom("LINE"), "line")
  expect_equal(resolve_geom("Bar"), "col")
  expect_equal(resolve_geom("Scatter"), "point")
  expect_equal(resolve_geom("Histogram"), "histogram")
})

test_that("resolve_geom returns NULL (not NA) for auto and for unknown geoms", {
  # NULL means "auto-pick". NA leaks through the is.null() guards downstream and
  # reaches switch(), which errors and renders an error box in the chart pane.
  expect_null(resolve_geom("auto"))
  expect_null(resolve_geom(""))
  expect_null(resolve_geom(NULL))
  expect_null(resolve_geom("sunburst"))
  expect_null(resolve_geom("not_a_geom"))
})

test_that("resolve_sort maps orderings including capitalized ones", {
  expect_equal(resolve_sort("value_desc"), "value_desc")
  expect_equal(resolve_sort("Value Desc"), "value_desc")
  expect_equal(resolve_sort("descending"), "value_desc")
  expect_equal(resolve_sort("Descending"), "value_desc")
  expect_equal(resolve_sort("desc"), "value_desc")
  expect_equal(resolve_sort("alphabetical"), "alpha_asc")
  expect_equal(resolve_sort("A-Z"), "alpha_asc")
  expect_equal(resolve_sort(""), "auto")
  expect_equal(resolve_sort("nonsense"), "auto")
})

test_that("resolve_label_format, y_scale and facet_scales are case-insensitive", {
  expect_equal(resolve_label_format("Dollar"), "dollar")
  expect_equal(resolve_label_format("USD"), "dollar")
  expect_equal(resolve_label_format("Percent"), "percent")
  expect_equal(resolve_label_format(""), "auto")

  expect_equal(resolve_y_scale("Log"), "log")
  expect_equal(resolve_y_scale("log10"), "log")
  expect_equal(resolve_y_scale("Sqrt"), "sqrt")
  expect_equal(resolve_y_scale(""), "linear")

  expect_equal(resolve_facet_scales("Free_Y"), "free_y")
  expect_equal(resolve_facet_scales("free"), "free")
  expect_equal(resolve_facet_scales(""), "fixed")
})

test_that("resolve_x_angle snaps to 0 / 45 / 90", {
  expect_equal(resolve_x_angle(""), 0)
  expect_equal(resolve_x_angle("0"), 0)
  expect_equal(resolve_x_angle("45"), 45)
  expect_equal(resolve_x_angle(45), 45)
  expect_equal(resolve_x_angle("90"), 90)
  expect_equal(resolve_x_angle("30"), 45)
  expect_equal(resolve_x_angle("diagonal"), 45)
  expect_equal(resolve_x_angle("Vertical"), 90)
})

test_that("ylab-only follow-up survives resolve without stealing the y column", {
  df <- data.frame(region = c("East", "West"), revenue = c(10, 20), stringsAsFactors = FALSE)
  current <- modifyList(default_plot_spec(), list(
    geom = "col", x = "region", y = "revenue", from = "aggregate"
  ))
  raw <- merge_plot_spec(current, flatten_plot_args(list(ylab = "Revenue (USD)")))
  out <- resolve_plot_spec(raw, NULL, df)
  expect_equal(out$spec$x, "region")
  expect_equal(out$spec$y, "revenue")
  expect_equal(out$spec$ylab, "Revenue (USD)")
  expect_equal(out$spec$geom, "col")
})

test_that("resolve_plot_spec keeps orientation, smooth, hline, and error bounds", {
  df <- data.frame(x = 1:8, y = 1:8 + 0.5, lo = 1:8, hi = 2:9)
  out <- resolve_plot_spec(
    list(
      geom = "point", x = "x", y = "y", from = "detail",
      smooth = "lm", hline = "mean", vline = "5",
      orientation = "horizontal", ymin = "lo", ymax = "hi"
    ),
    df,
    NULL
  )
  expect_equal(out$spec$smooth, "lm")
  expect_equal(out$spec$hline, "mean")
  expect_equal(out$spec$vline, "5")
  expect_equal(out$spec$orientation, "horizontal")
  expect_equal(out$spec$ymin, "lo")
  expect_equal(out$spec$ymax, "hi")

  rec <- plot_recipe(out$spec, out$df)
  expect_true(rec$ok)
  txt <- deparse_recipe(rec)
  expect_match(txt, "geom_smooth")
  expect_match(txt, "geom_hline")
  expect_match(txt, "coord_flip")
})

test_that("resolve_plot_spec remaps a date on y instead of leaving a dollar clock", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(
    month = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    revenue = c(10, 20, 15)
  )
  out <- resolve_plot_spec(
    list(geom = "col", x = "revenue", y = "month", from = "aggregate"),
    NULL,
    df
  )
  expect_equal(out$spec$x, "month")
  expect_equal(out$spec$y, "revenue")
  expect_equal(out$spec$orientation, "horizontal")
  expect_true(any(grepl("Date was on y", out$notes)))

  styled <- apply_style_to_spec(out$spec, list(number_format = "dollar"))
  rec <- plot_recipe(styled, out$df)
  expect_true(rec$ok)
  txt <- deparse_recipe(rec)
  expect_match(txt, "coord_flip")
  expect_match(txt, "scale_x_date")
  expect_match(txt, "scale_y_continuous")
  expect_false(grepl("scale_y_date", txt, fixed = TRUE))
  expect_s3_class(render_recipe(rec), "ggplot")
})

test_that("resolve_date_breaks maps to ggplot break strings", {
  expect_equal(resolve_date_breaks("1 month"), "1 month")
  expect_equal(resolve_date_breaks("1 Month"), "1 month")
  expect_equal(resolve_date_breaks("monthly"), "1 month")
  expect_equal(resolve_date_breaks("quarter"), "3 months")
  expect_equal(resolve_date_breaks("Year"), "1 year")
  expect_null(resolve_date_breaks("auto"))
  expect_null(resolve_date_breaks(""))
  expect_null(resolve_date_breaks("fortnightly"))
})

# --- model spec vocabulary ---

test_that("resolve_ml_method accepts canonical names and aliases", {
  expect_equal(resolve_ml_method("regression"), "regression")
  expect_equal(resolve_ml_method("classification"), "classification")
  expect_equal(resolve_ml_method("forecast"), "forecast")
  expect_equal(resolve_ml_method("lm"), "regression")
  expect_equal(resolve_ml_method("logistic"), "classification")
  expect_equal(resolve_ml_method("holt"), "forecast")
})

test_that("resolve_ml_method is case-insensitive", {
  # Getting this wrong is the worst failure in the app: "Classification" fell
  # through to the regression branch and silently fit the wrong model family.
  expect_equal(resolve_ml_method("Regression"), "regression")
  expect_equal(resolve_ml_method("Classification"), "classification")
  expect_equal(resolve_ml_method("Forecast"), "forecast")
  expect_equal(resolve_ml_method("LM"), "regression")
})

test_that("resolve_ml_method returns NULL for unknown methods so callers can fail loudly", {
  expect_null(resolve_ml_method("support_vector"))
  expect_null(resolve_ml_method("neural net"))
  expect_null(resolve_ml_method(""))
  expect_null(resolve_ml_method(NULL))
})

test_that("resolve_ml_plot maps plot kinds and returns NULL for auto/unknown", {
  expect_equal(resolve_ml_plot("coef"), "coef")
  expect_equal(resolve_ml_plot("Coef"), "coef")
  expect_equal(resolve_ml_plot("coefficients"), "coef")
  expect_equal(resolve_ml_plot("Residuals"), "residual")
  expect_equal(resolve_ml_plot("actualvspredicted"), "actual_pred")
  expect_equal(resolve_ml_plot("importance"), "importance")
  expect_equal(resolve_ml_plot("tree"), "tree")
  expect_equal(resolve_ml_plot("pdp"), "pdp")
  expect_equal(resolve_ml_plot("partial"), "pdp")
  expect_equal(resolve_ml_plot("path"), "path")
  expect_null(resolve_ml_plot("auto"))
  expect_null(resolve_ml_plot(""))
  expect_null(resolve_ml_plot("sankey"))
})

test_that("resolve_period maps grain including capitalized", {
  expect_equal(resolve_period("month"), "month")
  expect_equal(resolve_period("Month"), "month")
  expect_equal(resolve_period("Monthly"), "month")
  expect_equal(resolve_period("weekly"), "week")
  expect_equal(resolve_period("Annual"), "year")
  expect_equal(resolve_period(""), "auto")
  expect_equal(resolve_period("fortnight"), "auto")
})

test_that("resolve_horizon and resolve_cv_folds clamp to sane ranges", {
  expect_equal(resolve_horizon(""), 3L)
  expect_equal(resolve_horizon("6"), 6L)
  expect_equal(resolve_horizon(6), 6L)
  expect_equal(resolve_horizon("500"), 24L)
  expect_equal(resolve_horizon("0"), 3L)
  expect_equal(resolve_horizon("-2"), 3L)

  expect_equal(resolve_cv_folds(""), 5L)
  expect_equal(resolve_cv_folds("10"), 10L)
  expect_equal(resolve_cv_folds("99"), 10L)
  expect_equal(resolve_cv_folds("none"), 0L)
  expect_equal(resolve_cv_folds("0"), 0L)
  expect_equal(resolve_cv_folds("1"), 0L)
})
