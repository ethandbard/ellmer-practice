# Teaching plots promised in Phase 5: drawn trees, partial dependence,
# glmnet path, GAM s() terms, and a code pane that matches the engine.

test_that("measure_fn_for follows the profile (AVG for ratings, SUM for revenue)", {
  expect_equal(measure_fn_for("rating", c(1, 2, 3, 4, 5)), "AVG")
  expect_equal(measure_fn_for("revenue", c(10, 20, 50, 200, 1000)), "SUM")
})

test_that("default plots for tree and gam are the teaching charts", {
  expect_equal(default_plot_for_method("tree"), "tree")
  expect_equal(default_plot_for_method("gam"), "pdp")
  expect_equal(default_plot_for_method("forest"), "importance")
})

test_that("supervised_fit_lines prints the engine that ran", {
  base <- list(
    method = "regression",
    target = "y",
    features = "x",
    interactions = character(),
    penalty = 0.02,
    positive = NULL,
    fit_label = "Decision tree (rpart) · y ~ x"
  )
  tree_txt <- paste(supervised_fit_lines(modifyList(base, list(engine = "tree"))), collapse = "\n")
  expect_match(tree_txt, "decision_tree")
  expect_false(grepl("linear_reg\\(\\)", tree_txt))

  gam_txt <- paste(supervised_fit_lines(modifyList(base, list(
    engine = "gam",
    fit_label = "GAM (mgcv) · y ~ s(x)"
  ))), collapse = "\n")
  expect_match(gam_txt, "gen_additive_mod")
  expect_match(gam_txt, "s\\(")

  lasso_txt <- paste(supervised_fit_lines(modifyList(base, list(engine = "lasso"))), collapse = "\n")
  expect_match(lasso_txt, "mixture = 1")
  expect_match(lasso_txt, "glmnet")
})

test_that("decision tree default plot is a drawn tree", {
  skip_if_not_installed("rpart")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("workflows")
  skip_if_not_installed("ggplot2")
  set.seed(1)
  df <- data.frame(
    x = rep(1:20, 5),
    g = rep(c("a", "b"), 50),
    y = rep(1:20, 5) + ifelse(rep(c("a", "b"), 50) == "a", 8, 0) + rnorm(100, 0, 0.4)
  )
  res <- fit_supervised(
    df,
    method = "tree",
    target = "y",
    features = c("x", "g"),
    title = "tree",
    plot = "tree",
    prepare_sql = "",
    cv_folds = 0L
  )
  expect_true(res$ok)
  expect_equal(res$engine, "tree")
  expect_equal(res$plot, "tree")
  expect_true(!is.null(res$tree_nodes) && nrow(res$tree_nodes) >= 1)
  rec <- model_plot_recipe(res)
  expect_true(rec$ok)
  txt <- deparse_recipe(rec)
  expect_match(txt, "geom_label")
  expect_match(txt, "geom_segment")
  expect_s3_class(render_model_plot(res), "ggplot")
})

test_that("GAM wraps numeric predictors in s() and draws partial dependence", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("workflows")
  skip_if_not_installed("ggplot2")
  set.seed(1)
  x <- seq(0, 2 * pi, length.out = 80)
  df <- data.frame(x = x, z = rnorm(80), y = sin(x) + rnorm(80, 0, 0.15))
  res <- fit_supervised(
    df,
    method = "gam",
    target = "y",
    features = c("x", "z"),
    title = "gam",
    plot = "pdp",
    prepare_sql = "",
    cv_folds = 0L
  )
  expect_true(res$ok)
  expect_equal(res$engine, "gam")
  expect_match(res$fit_label, "s\\(")
  expect_true(!is.null(res$pdp) && nrow(res$pdp) >= 5)
  rec <- model_plot_recipe(res)
  expect_true(rec$ok)
  expect_match(deparse_recipe(rec), "partial dependence|yhat")
  expect_s3_class(render_model_plot(res), "ggplot")
})

test_that("lasso path plot has many lambdas", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("workflows")
  skip_if_not_installed("ggplot2")
  set.seed(1)
  n <- 60
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    y = rnorm(n)
  )
  df$y <- 2 * df$x1 + rnorm(n, 0, 0.3)
  res <- fit_supervised(
    df,
    method = "regression",
    target = "y",
    features = c("x1", "x2", "x3"),
    title = "path",
    plot = "path",
    prepare_sql = "",
    cv_folds = 0L,
    auto_select = TRUE
  )
  expect_true(res$ok)
  expect_equal(res$engine, "lasso")
  expect_true(!is.null(res$path) && nrow(res$path) > 3)
  expect_gt(n_distinct_safe(res$path$lambda), 3)
  rec <- model_plot_recipe(res)
  expect_true(rec$ok)
  expect_match(deparse_recipe(rec), "log\\(lambda\\)")
  expect_s3_class(render_model_plot(res), "ggplot")
})

test_that("classification threshold is stored and used for predicted labels", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("workflows")
  set.seed(1)
  df <- data.frame(
    x = c(rnorm(40, 0), rnorm(40, 2)),
    y = c(rep("no", 40), rep("yes", 40)),
    stringsAsFactors = FALSE
  )
  res <- fit_supervised(
    df,
    method = "classification",
    target = "y",
    features = "x",
    title = "cls",
    plot = "confusion",
    prepare_sql = "",
    cv_folds = 0L,
    threshold = 0.7
  )
  expect_true(res$ok)
  expect_equal(res$threshold, 0.7)
  expect_true("predicted_label" %in% names(res$scores))
  rec <- model_plot_recipe(res)
  expect_true(rec$ok)
  expect_match(deparse_recipe(rec), "geom_tile")
})
