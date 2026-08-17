test_that("violin is a real geom, not a silent boxplot", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(g = rep(c("a", "b"), 20), y = rnorm(40))
  spec <- modifyList(default_plot_spec(), list(geom = "violin", x = "g", y = "y", color = ""))
  rec <- plot_recipe(spec, df)
  expect_true(rec$ok)
  txt <- deparse_recipe(rec)
  expect_match(txt, "geom_violin")
  expect_false(grepl("geom_boxplot", txt, fixed = TRUE))
})

test_that("smooth and hline land in the recipe", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = 1:20, y = 1:20 + rnorm(20))
  spec <- modifyList(default_plot_spec(), list(
    geom = "point", x = "x", y = "y", color = "", smooth = "lm", hline = "mean"
  ))
  rec <- plot_recipe(spec, df)
  expect_true(rec$ok)
  txt <- deparse_recipe(rec)
  expect_match(txt, "geom_smooth")
  expect_match(txt, "geom_hline")
})

test_that("orientation=vertical does not coord_flip crowded bars", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(g = letters[1:8], v = 1:8)
  spec <- modifyList(default_plot_spec(), list(
    geom = "col", x = "g", y = "v", color = "", orientation = "vertical"
  ))
  txt <- deparse_recipe(plot_recipe(spec, df))
  expect_false(grepl("coord_flip", txt, fixed = TRUE))
})

test_that("a colored violin is not pinned to a constant fill", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(g = rep(c("a", "b"), 20), y = rnorm(40))
  spec <- modifyList(default_plot_spec(), list(
    geom = "violin", x = "g", y = "y", color = "g"
  ))
  rec <- plot_recipe(spec, df)
  expect_true(rec$ok)
  txt <- deparse_recipe(rec)
  expect_match(txt, "geom_violin")
  expect_false(grepl("geom_violin\\([^\\n]*fill", txt))
  expect_s3_class(render_recipe(rec), "ggplot")
})

test_that("flip plus zoom uses one coord_flip, not a second cartesian", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(g = letters[1:4], v = 1:4)
  spec <- modifyList(default_plot_spec(), list(
    geom = "col", x = "g", y = "v", color = "",
    orientation = "horizontal", xlim = "", ylim = "0, 10"
  ))
  txt <- deparse_recipe(plot_recipe(spec, df))
  expect_match(txt, "coord_flip")
  expect_false(grepl("coord_cartesian", txt, fixed = TRUE))
})

test_that("palettes stretch instead of erroring on extra levels", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(g = letters[1:9], v = 1:9, stringsAsFactors = FALSE)
  spec <- modifyList(default_plot_spec(), list(
    geom = "col", x = "g", y = "v", color = "g", palette = "okabe_ito"
  ))
  rec <- plot_recipe(spec, df)
  expect_true(rec$ok)
  p <- render_recipe(rec)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))

  seq_spec <- modifyList(spec, list(palette = "sequential"))
  seq_rec <- plot_recipe(seq_spec, df)
  expect_true(seq_rec$ok)
  expect_match(deparse_recipe(seq_rec), "scale_fill_manual")
  expect_silent(ggplot2::ggplot_build(render_recipe(seq_rec)))
})

test_that("rotated ticks do not hardcode paper ink", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(g = letters[1:4], v = 1:4)
  spec <- modifyList(default_plot_spec(), list(
    geom = "col", x = "g", y = "v", color = "", theme = "dark", x_angle = 45
  ))
  txt <- deparse_recipe(plot_recipe(spec, df))
  expect_match(txt, "angle = 45")
  expect_false(grepl("#2a2622", txt, fixed = TRUE))
})

test_that("Welch t-test returns a p-value and Cohen's d", {
  set.seed(1)
  df <- data.frame(
    group = rep(c("A", "B"), each = 30),
    y = c(rnorm(30, 0), rnorm(30, 1))
  )
  res <- fit_stat_test(df, list(target = "y", features = "group", title = "t", plot = "test"))
  expect_true(res$ok)
  expect_equal(res$engine, "welch_t")
  expect_true(is.finite(res$metrics$p.value))
  expect_true(is.finite(res$metrics$cohen_d))
})

test_that("PCA returns a scree table", {
  set.seed(1)
  df <- as.data.frame(matrix(rnorm(80), ncol = 4))
  names(df) <- paste0("v", 1:4)
  res <- fit_pca(df, list(title = "pca", plot = "scree", features = ""))
  expect_true(res$ok)
  expect_equal(nrow(res$scree), 4)
  expect_s3_class(render_model_plot(res), "ggplot")
})
