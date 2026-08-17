# Phase 2: the chart pane and the code pane share one recipe.

eval_deparsed <- function(recipe) {
  skip_if_not_installed("ggplot2")
  code <- paste0("p <- ", paste(vapply(recipe$layers, deparse_layer, ""), collapse = " +\n  "))
  env <- rlang::new_environment(parent = rlang::current_env())
  env[[recipe$data_name]] <- recipe$data
  for (nm in names(recipe$bindings)) {
    env[[nm]] <- recipe$bindings[[nm]]
  }
  eval(parse(text = code), envir = env)
}

test_that("printed ggplot is textbook R, not ggplot2:: qualified calls", {
  df <- data.frame(region = c("East", "West"), revenue = c(10, 20), stringsAsFactors = FALSE)
  spec <- modifyList(default_plot_spec(), list(geom = "col", x = "region", y = "revenue", theme = "paper"))
  txt <- deparse_recipe(plot_recipe(spec, df))
  expect_false(grepl("ggplot2::", txt, fixed = TRUE))
  expect_false(grepl("library(", txt, fixed = TRUE))
  expect_match(txt, "geom_col")
  expect_match(txt, "theme_minimal")
})

test_that("deparse_recipe omits the plotly wrapper unless interactive is on", {
  df <- data.frame(region = c("East", "West"), revenue = c(10, 20), stringsAsFactors = FALSE)
  spec <- modifyList(default_plot_spec(), list(geom = "col", x = "region", y = "revenue"))
  rec <- plot_recipe(spec, df)
  expect_true(rec$ok)
  off <- deparse_recipe(rec)
  expect_false(grepl("ggplotly", off, fixed = TRUE))
  expect_false(grepl("library(plotly)", off, fixed = TRUE))
  expect_false(grepl("ggplot2::", off, fixed = TRUE))
  expect_match(off, "ggplot\\(")
  on <- deparse_recipe(rec, interactive = TRUE)
  expect_match(on, "ggplotly")
})

test_that("render and deparse agree on a simple bar chart", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(region = c("East", "West"), revenue = c(10, 20), stringsAsFactors = FALSE)
  spec <- modifyList(default_plot_spec(), list(
    geom = "col",
    x = "region",
    y = "revenue",
    title = "Revenue by region",
    position = "dodge"
  ))
  rec <- plot_recipe(spec, df, data_name = "plot_data")
  expect_true(rec$ok)

  drawn <- render_recipe(rec)
  from_code <- eval_deparsed(rec)

  expect_s3_class(drawn, "ggplot")
  expect_s3_class(from_code, "ggplot")
  expect_equal(nrow(drawn$data), nrow(from_code$data))
  expect_equal(length(drawn$layers), length(from_code$layers))

  # The old print path hard-coded position = "stack" for uncolored bars.
  # The recipe must carry the position that actually ran.
  txt <- deparse_recipe(rec)
  expect_match(txt, 'position = "dodge"')
})

test_that("value labels are formatted and sit beside flipped bars", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(g = letters[1:3], v = c(10.019999, 20, 30))
  upright <- plot_recipe(modifyList(default_plot_spec(), list(
    geom = "col", x = "g", y = "v", labels = TRUE
  )), df)
  expect_match(deparse_recipe(upright), "label_number|label_dollar|label_comma")
  expect_match(deparse_recipe(upright), "vjust = -0.35")

  flipped <- plot_recipe(modifyList(default_plot_spec(), list(
    geom = "col", x = "g", y = "v", labels = TRUE, orientation = "horizontal"
  )), df)
  txt <- deparse_recipe(flipped)
  expect_match(txt, "hjust = -0.15")
  expect_match(txt, "coord_flip")
  expect_false(grepl("vjust = -0.35", txt, fixed = TRUE))
})

test_that("a ylab-only merge still draws the current mapping", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(region = c("East", "West"), revenue = c(10, 20), stringsAsFactors = FALSE)
  current <- modifyList(default_plot_spec(), list(
    geom = "col", x = "region", y = "revenue", title = "Revenue by region"
  ))
  merged <- merge_plot_spec(current, flatten_plot_args(list(ylab = "Revenue (USD)")))
  rec <- plot_recipe(merged, df)
  expect_true(rec$ok)
  txt <- deparse_recipe(rec)
  expect_match(txt, "Revenue \\(USD\\)")
  expect_match(txt, "geom_col")
})

test_that("value labels reserve top headroom sized to label_size, not a fixed clip-prone margin", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(region = c("East", "West"), revenue = c(10, 20), stringsAsFactors = FALSE)

  no_labels <- plot_recipe(modifyList(default_plot_spec(), list(
    geom = "col", x = "region", y = "revenue"
  )), df)
  expect_true(no_labels$ok)
  expect_false(grepl("expansion", deparse_recipe(no_labels)))

  default_size <- plot_recipe(modifyList(default_plot_spec(), list(
    geom = "col", x = "region", y = "revenue", labels = TRUE
  )), df)
  txt_default <- deparse_recipe(default_size)
  expect_match(txt_default, "expansion\\(mult = c\\(0.05, 0.09\\)\\)")
  expect_match(txt_default, "size = 3")

  bigger <- plot_recipe(modifyList(default_plot_spec(), list(
    geom = "col", x = "region", y = "revenue", labels = TRUE, label_size = 6
  )), df)
  txt_bigger <- deparse_recipe(bigger)
  # A bigger label gets more headroom, not the same fixed margin -- this is
  # what makes "make the marks bigger" actually keep them visible.
  expect_match(txt_bigger, "expansion\\(mult = c\\(0.05, 0.12\\)\\)")
  expect_match(txt_bigger, "size = 6")

  p <- render_recipe(bigger)
  expect_s3_class(p, "ggplot")
})

test_that("uncolored col uses the resolved position in both panes", {
  df <- data.frame(g = letters[1:3], v = 1:3, stringsAsFactors = FALSE)
  spec <- modifyList(default_plot_spec(), list(
    geom = "col", x = "g", y = "v", color = "", position = "identity"
  ))
  rec <- plot_recipe(spec, df)
  expect_match(deparse_recipe(rec), 'position = "identity"')
  expect_s3_class(render_recipe(rec), "ggplot")
})

test_that("auto charts go through the same recipe", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(
    month = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    revenue = c(10, 20, 15)
  )
  spec <- modifyList(default_plot_spec(), list(geom = "", auto = TRUE, from = "aggregate"))
  rec <- dashboard_plot_recipe(spec, NULL, df)
  expect_true(rec$ok)
  expect_match(deparse_recipe(rec), "ggplot\\(")
  expect_s3_class(render_recipe(rec), "ggplot")
  expect_match(deparse_recipe(rec), "# auto")
})

test_that("model code prints the tidymodels fit that ran, not MASS::stepAIC", {
  res <- list(
    ok = TRUE,
    method = "regression",
    engine = "lasso",
    penalty = 0.02,
    fit_label = "Lasso · revenue ~ quantity",
    prepare_sql = "",
    title = "Demo",
    target = "revenue",
    features = "quantity",
    interactions = character(),
    auto_select = TRUE,
    notes = "Lasso (mixture = 1) with lambda = 0.02.",
    plot = "actual_pred",
    split = "random 80/20",
    scores = data.frame(actual = 1:5, fitted = 1:5 + 0.1, residual = rep(-0.1, 5)),
    coefs = data.frame(term = "quantity", estimate = 1, std.error = 0.1, conf.low = 0.8, conf.high = 1.2),
    cv = NULL,
    subgroup_table = NULL,
    series = NULL
  )
  txt <- model_code_chunk(res)
  expect_false(grepl("fit <- MASS::stepAIC", txt, fixed = TRUE))
  expect_false(grepl("stepwise_select", txt, fixed = TRUE))
  expect_match(txt, "linear_reg")
  expect_match(txt, "glmnet")
  expect_match(txt, "fit\\(wf")
  expect_false(grepl("ggplot2::", txt, fixed = TRUE))
  expect_false(grepl("ggplotly", txt, fixed = TRUE))
  expect_match(model_code_chunk(res, interactive = TRUE), "ggplotly")
})

test_that("model plot recipe renders a coefficient chart", {
  skip_if_not_installed("ggplot2")
  res <- list(
    ok = TRUE,
    method = "regression",
    plot = "coef",
    title = "Demo",
    fit_label = "Linear model",
    coefs = data.frame(
      term = c("(Intercept)", "quantity"),
      estimate = c(1, 2),
      std.error = c(0.1, 0.2),
      conf.low = c(0.8, 1.6),
      conf.high = c(1.2, 2.4)
    )
  )
  rec <- model_plot_recipe(res)
  expect_true(rec$ok)
  p <- render_recipe(rec)
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 1)
  expect_equal(p$data$term, "quantity")
})
