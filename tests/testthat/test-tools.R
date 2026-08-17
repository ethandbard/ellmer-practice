# Phase 3: typed schemas, sticky style, richer tool returns.

test_that("dashboard schema is five fields, with nested plot and style", {
  expect_equal(
    sort(names(dashboard_tool_arguments)),
    c("aggregate_sql", "detail_sql", "plot", "style", "title")
  )
  # Construction must succeed: this is what register_tool() will send
  # to both Anthropic and the OpenAI-compatible xAI path.
  expect_silent(
    ellmer::tool(
      fun = function(title, detail_sql = NULL, aggregate_sql = NULL, plot = NULL, style = NULL) title,
      description = "test",
      arguments = dashboard_tool_arguments
    )
  )
})

test_that("model schema uses enums and integers, not a wall of type_string", {
  expect_true("method" %in% names(model_tool_arguments))
  expect_silent(
    ellmer::tool(
      fun = function(title, method, target, prepare_sql = NULL) title,
      description = "test",
      arguments = model_tool_arguments[c("title", "method", "target", "prepare_sql")]
    )
  )
})

test_that("merge_model_spec keeps the fit when only subgroup is sent", {
  current <- list(
    method = "regression",
    target = "revenue",
    features = "quantity, channel",
    prepare_sql = "",
    plot = "coef",
    subgroup = ""
  )
  merged <- merge_model_spec(current, list(subgroup = "region"))
  expect_equal(merged$method, "regression")
  expect_equal(merged$target, "revenue")
  expect_equal(merged$features, "quantity, channel")
  expect_equal(merged$subgroup, "region")
  expect_equal(merged$plot, "subgroup")
})

test_that("merge_model_spec does not keep a stale subgroup when the target changes", {
  current <- list(
    method = "regression",
    target = "revenue",
    features = "quantity",
    subgroup = "region",
    plot = "subgroup"
  )
  merged <- merge_model_spec(current, list(method = "regression", target = "tip"))
  expect_equal(merged$target, "tip")
  expect_equal(merged$subgroup, "")
  expect_equal(merged$features, "")
})

test_that("style schema exposes a label_size knob the model can call", {
  expect_true("label_size" %in% names(style_spec_type()@properties))
  expect_equal(default_style()$label_size, 3)
  styled <- apply_style_to_spec(default_plot_spec(), list(label_size = 5))
  expect_equal(styled$label_size, 5)
})

test_that("flatten_plot_args does not let ylab partial-match onto y", {
  flat <- flatten_plot_args(list(ylab = "Revenue (USD)", xlab = "Region"))
  expect_null(flat$y)
  expect_null(flat$x)
  expect_equal(flat$ylab, "Revenue (USD)")
  expect_equal(flat$xlab, "Region")
})

test_that("merge_plot_spec keeps mapping when only a lab is sent", {
  current <- modifyList(default_plot_spec(), list(
    geom = "col", x = "region", y = "revenue",
    title = "Revenue by region", xlab = "Region", ylab = "Revenue"
  ))
  incoming <- flatten_plot_args(list(ylab = "Revenue (USD)"))
  merged <- merge_plot_spec(current, incoming)
  expect_equal(merged$geom, "col")
  expect_equal(merged$x, "region")
  expect_equal(merged$y, "revenue")
  expect_equal(merged$xlab, "Region")
  expect_equal(merged$title, "Revenue by region")
  expect_equal(merged$ylab, "Revenue (USD)")
})

test_that("merge_plot_spec resets unsent labs when the mapping changes", {
  current <- modifyList(default_plot_spec(), list(
    geom = "col", x = "region", y = "revenue", ylab = "Revenue (USD)"
  ))
  incoming <- flatten_plot_args(list(geom = "col", x = "channel", y = "quantity"))
  merged <- merge_plot_spec(current, incoming)
  expect_equal(merged$x, "channel")
  expect_equal(merged$y, "quantity")
  expect_equal(merged$ylab, "")
})

test_that("merge_plot_spec treats an empty ylab as a clear", {
  current <- modifyList(default_plot_spec(), list(ylab = "Revenue (USD)", x = "region"))
  incoming <- flatten_plot_args(list(ylab = ""))
  merged <- merge_plot_spec(current, incoming)
  expect_equal(merged$ylab, "")
  expect_equal(merged$x, "region")
})

test_that("reuse_last_queries fills in when both SQL args were omitted", {
  unused <- list(used = FALSE, ok = TRUE, sql = "", df = NULL, error = NULL)
  current <- list(
    detail_df = data.frame(a = 1),
    aggregate_df = data.frame(g = "East", v = 10),
    detail_sql = "SELECT * FROM data",
    aggregate_sql = "SELECT g, SUM(v) AS v FROM data GROUP BY 1"
  )
  out <- reuse_last_queries(unused, unused, current)
  expect_true(out$reused)
  expect_equal(out$agg$df$v, 10)
  expect_match(out$agg$sql, "GROUP BY")

  sent <- list(used = TRUE, ok = TRUE, sql = "SELECT 1", df = data.frame(x = 1), error = NULL)
  out2 <- reuse_last_queries(unused, sent, current)
  expect_false(out2$reused)
})

test_that("merge_style only overwrites fields the model sent", {
  cur <- default_style()
  cur$theme <- "dark"
  cur$palette <- "viridis"
  merged <- merge_style(cur, list(legend_position = "bottom", theme = NULL, palette = ""))
  expect_equal(merged$theme, "dark")
  expect_equal(merged$palette, "viridis")
  expect_equal(merged$legend_position, "bottom")
})

test_that("apply_style_to_spec can turn value labels off", {
  spec <- modifyList(default_plot_spec(), list(labels = TRUE))
  on <- apply_style_to_spec(spec, list(labels = TRUE))
  expect_true(on$labels)
  off <- apply_style_to_spec(on, list(labels = FALSE))
  expect_false(off$labels)
})

test_that("apply_style_to_spec copies sticky appearance onto the plot spec", {
  spec <- default_plot_spec()
  styled <- apply_style_to_spec(spec, list(theme = "dark", number_format = "dollar", x_angle = 45))
  expect_equal(styled$theme, "dark")
  expect_equal(styled$label_format, "dollar")
  expect_equal(styled$x_angle, 45)
})

test_that("apply_style_to_spec does not blank auto-view date breaks, angle, or format", {
  spec <- modifyList(default_plot_spec(), list(
    date_breaks = "1 month",
    x_angle = 45,
    label_format = "dollar"
  ))
  styled <- apply_style_to_spec(spec, default_style())
  expect_equal(styled$date_breaks, "1 month")
  expect_equal(styled$x_angle, 45)
  expect_equal(styled$label_format, "dollar")

  overridden <- apply_style_to_spec(spec, list(
    number_format = "percent",
    x_angle = 90,
    date_breaks = "1 year"
  ))
  expect_equal(overridden$label_format, "percent")
  expect_equal(overridden$x_angle, 90)
  expect_equal(overridden$date_breaks, "1 year")
})

test_that("compact_df_md truncates wide and long frames", {
  df <- as.data.frame(matrix(1:200, nrow = 40, ncol = 5))
  names(df) <- paste0("c", 1:5)
  md <- compact_df_md(df, max_rows = 5, max_cols = 3)
  expect_match(md, "c1")
  expect_match(md, "truncated")
  expect_match(md, "5 of 40 rows")
  expect_match(md, "3 of 5 columns")
})

test_that("set_dashboard return includes the plotted numbers", {
  agg <- list(
    used = TRUE,
    ok = TRUE,
    df = data.frame(region = c("East", "West"), revenue = c(10, 20), stringsAsFactors = FALSE)
  )
  detail <- list(used = FALSE, ok = TRUE, df = NULL)
  spec <- modifyList(default_plot_spec(), list(geom = "col", from = "aggregate", x = "region", y = "revenue"))
  out <- format_dashboard_return("Revenue by region", spec, detail, agg)
  expect_match(out, "Dashboard updated")
  expect_match(out, "East")
  expect_match(out, "20")
  expect_match(out, "Plot: col on aggregate")
})

test_that("a styled recipe deparses the theme that actually ran", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(g = c("East", "West"), v = c(10, 20), stringsAsFactors = FALSE)
  spec <- apply_style_to_spec(
    modifyList(default_plot_spec(), list(geom = "col", x = "g", y = "v", color = "")),
    list(theme = "dark", highlight = "West")
  )
  rec <- plot_recipe(spec, df)
  expect_true(rec$ok)
  txt <- deparse_recipe(rec)
  expect_match(txt, "theme_minimal")
  p <- render_recipe(rec)
  expect_s3_class(p, "ggplot")
  expect_true(".highlight" %in% names(rec$data))
})
