# Model-page recipe: the printed R is the call that ran, and the chart
# is a plot_recipe so it cannot drift from the code pane.

supervised_fit_lines <- function(res) {
  rhs <- paste(c(vapply(res$features, r_name, ""), res$interactions), collapse = " + ")
  y <- if (identical(res$method, "classification")) ".class" else r_name(res$target)
  form <- paste(y, "~", rhs)
  engine <- res$engine %or% if (identical(res$method, "classification")) "glm" else "lm"
  class_mode <- identical(res$method, "classification") || identical(engine, "multinom")
  mix <- switch(engine, ridge = 0, elastic = 0.5, lasso = 1, NULL)
  spec_line <- if (!is.null(mix) && class_mode) {
    paste0(
      "spec <- parsnip::logistic_reg(penalty = ", res$penalty %or% 0.01,
      ", mixture = ", mix, ") |> parsnip::set_engine(\"glmnet\")"
    )
  } else if (!is.null(mix)) {
    paste0(
      "spec <- parsnip::linear_reg(penalty = ", res$penalty %or% 0.01,
      ", mixture = ", mix, ") |> parsnip::set_engine(\"glmnet\")"
    )
  } else if (identical(engine, "poisson")) {
    "spec <- parsnip::poisson_reg() |> parsnip::set_engine(\"glm\")"
  } else if (identical(engine, "tree")) {
    paste0(
      "spec <- parsnip::decision_tree(mode = \"",
      if (class_mode) "classification" else "regression",
      "\", tree_depth = 4) |> parsnip::set_engine(\"rpart\")"
    )
  } else if (identical(engine, "forest")) {
    paste0(
      "spec <- parsnip::rand_forest(mode = \"",
      if (class_mode) "classification" else "regression",
      "\", trees = 200) |> parsnip::set_engine(\"ranger\", importance = \"impurity\")"
    )
  } else if (identical(engine, "gam")) {
    "spec <- parsnip::gen_additive_mod(mode = \"regression\") |> parsnip::set_engine(\"mgcv\")"
  } else if (identical(engine, "multinom")) {
    "spec <- parsnip::multinom_reg() |> parsnip::set_engine(\"nnet\")"
  } else if (class_mode) {
    "spec <- parsnip::logistic_reg() |> parsnip::set_engine(\"glm\")"
  } else {
    "spec <- parsnip::linear_reg() |> parsnip::set_engine(\"lm\")"
  }
  lines <- c(
    "frame <- data",
    "# 80/20 split → train, test"
  )
  if (identical(res$method, "classification") && !is.null(res$positive)) {
    lines <- c(
      lines,
      paste0(
        "train$.class <- factor(ifelse(train[[", quote_str(res$target), "]] == ",
        quote_str(res$positive), ", ", quote_str(res$positive), ", ",
        quote_str(res$negative %or% "other"), "))"
      )
    )
  }
  if (!is.null(mix)) {
    lines <- c(
      lines,
      paste0(
        "# ", engine, " (mixture = ", mix,
        "): shrinkage instead of stepwise AIC. lambda is cv.glmnet's lambda.1se."
      )
    )
  }
  rec_form <- form
  model_form <- form
  if (identical(engine, "gam")) {
    lines <- c(
      lines,
      "# Numeric predictors are wrapped in s() so mgcv fits smooths, not a straight line."
    )
    if (nzchar(res$fit_label %or% "") && grepl("s\\(", res$fit_label)) {
      rhs_s <- sub("^.*~\\s*", "", res$fit_label)
      model_form <- paste(y, "~", rhs_s)
    }
  }
  if (identical(engine, "tree")) {
    lines <- c(
      lines,
      "# tree_depth = 4 keeps the preview readable. For a publication drawing:",
      "# rpart.plot::rpart.plot(workflows::extract_fit_engine(fit))"
    )
  }
  dummy_line <- if (engine %in% c("tree", "forest", "gam")) {
    NULL
  } else {
    "  recipes::step_dummy(recipes::all_nominal_predictors()) |>"
  }
  add_model <- if (identical(engine, "gam")) {
    paste0(
      "wf <- workflows::workflow() |> workflows::add_recipe(rec) |> ",
      "workflows::add_model(spec, formula = ", model_form, ")"
    )
  } else {
    "wf <- workflows::workflow() |> workflows::add_recipe(rec) |> workflows::add_model(spec)"
  }
  c(
    lines,
    paste0("rec <- recipes::recipe(", rec_form, ", data = train) |>"),
    dummy_line,
    "  recipes::step_zv(recipes::all_predictors())",
    if (!is.null(mix)) {
      "rec <- rec |> recipes::step_normalize(recipes::all_numeric_predictors())"
    },
    spec_line,
    add_model,
    "fit <- workflows::fit(wf, data = train)",
    if (class_mode && !identical(engine, "multinom")) {
      "pred <- predict(fit, new_data = test, type = \"prob\")"
    } else {
      "pred <- predict(fit, new_data = test)"
    },
    if (engine %in% c("tree", "forest")) {
      "workflows::extract_fit_engine(fit)$variable.importance"
    } else if (!is.null(mix)) {
      paste0("broom::tidy(workflows::extract_fit_parsnip(fit), penalty = ", res$penalty %or% 0.01, ")")
    } else {
      "broom::tidy(workflows::extract_fit_parsnip(fit), conf.int = TRUE)"
    }
  )
}

model_plot_recipe <- function(res, style = NULL) {
  if (is.null(res) || !isTRUE(res$ok)) {
    return(failed_recipe(res$error %or% "Ask a modeling question"))
  }
  style <- merge_style(default_style(), style)
  theme_name <- style$theme %or% "paper"
  base_size <- if (is.numeric(style$base_size)) style$base_size[[1]] else 13
  accent <- empty_to_null(style$accent) %or% accent_ink
  plot <- res$plot %or% default_plot_for_method(res$method)

  if (identical(plot, "forecast") && !is.null(res$series)) {
    df <- res$series
    df$part <- factor(df$part, levels = c("history", "holdout", "forecast"))
    ribbon <- df[!is.na(df$lo) & !is.na(df$hi), , drop = FALSE]
    layers <- list(
      rlang::expr(ggplot2::ggplot(
        series,
        ggplot2::aes(x = period, y = y, colour = part, group = part)
      )),
      rlang::expr(ggplot2::geom_line(linewidth = 1)),
      rlang::expr(ggplot2::geom_point(size = 2))
    )
    if (nrow(ribbon)) {
      layers <- c(layers, list(rlang::expr(
        ggplot2::geom_ribbon(
          data = ribbon,
          ggplot2::aes(x = period, ymin = lo, ymax = hi),
          inherit.aes = FALSE,
          fill = !!accent,
          alpha = 0.12
        )
      )))
    }
    layers <- c(layers, list(
      rlang::expr(ggplot2::scale_color_manual(
        values = c(history = !!accent, holdout = "#8a5a32", forecast = "#1b3a4b"),
        drop = FALSE
      )),
      rlang::expr(ggplot2::labs(
        title = !!res$title,
        subtitle = !!res$fit_label,
        x = NULL,
        y = !!res$target,
        colour = NULL
      )),
      rlang::expr(theme_preview(!!theme_name, !!base_size))
    ))
    return(new_plot_recipe(
      data = df,
      data_name = "series",
      layers = layers,
      bindings = list(ribbon = ribbon),
      header = c(paste0("# ", res$fit_label), "# series: history + holdout + forecast")
    ))
  }

  if (identical(plot, "coef") && !is.null(res$coefs) && nrow(res$coefs)) {
    coefs <- res$coefs
    if (nrow(coefs) > 1) {
      coefs <- coefs[coefs$term != "(Intercept)", , drop = FALSE]
    }
    if (!nrow(coefs)) {
      coefs <- res$coefs
    }
    if (nrow(coefs) > 16) {
      coefs <- coefs[order(-abs(coefs$estimate)), , drop = FALSE]
      coefs <- coefs[seq_len(16), , drop = FALSE]
    }
    ylab <- if (identical(res$method, "classification")) "log-odds" else "estimate"
    layers <- list(
      rlang::expr(ggplot2::ggplot(
        coefs,
        ggplot2::aes(x = stats::reorder(term, estimate), y = estimate)
      )),
      rlang::expr(ggplot2::geom_col(fill = !!accent)),
      rlang::expr(ggplot2::coord_flip())
    )
    if (all(c("conf.low", "conf.high") %in% names(coefs))) {
      layers <- c(layers, list(rlang::expr(
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = conf.low, ymax = conf.high),
          width = 0.2,
          color = "#1c1917"
        )
      )))
    } else if ("std.error" %in% names(coefs)) {
      layers <- c(layers, list(rlang::expr(
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = estimate - 1.96 * std.error,
            ymax = estimate + 1.96 * std.error
          ),
          width = 0.2,
          color = "#1c1917"
        )
      )))
    }
    layers <- c(layers, list(
      rlang::expr(ggplot2::labs(
        title = !!res$title,
        subtitle = "Coefficient estimates, 95% CI (intercept omitted when others exist)",
        x = NULL,
        y = !!ylab
      )),
      rlang::expr(theme_preview(!!theme_name, !!base_size))
    ))
    return(new_plot_recipe(
      data = coefs,
      data_name = "coefs",
      layers = layers,
      header = c(paste0("# ", res$fit_label), "# coefs: broom-style term table")
    ))
  }

  if (identical(plot, "cv") && !is.null(res$cv) && isTRUE(res$cv$ok) && nrow(res$cv$folds)) {
    cv <- res$cv
    folds <- cv$folds
    folds$fold <- factor(folds$fold)
    metric <- cv$metric_name
    layers <- list(
      rlang::expr(ggplot2::ggplot(cv_folds, ggplot2::aes(x = fold, y = !!rlang::sym(metric)))),
      rlang::expr(ggplot2::geom_hline(yintercept = !!cv$mean, linetype = "dashed", color = "#8a5a32")),
      rlang::expr(ggplot2::geom_col(fill = !!accent)),
      rlang::expr(ggplot2::labs(
        title = !!res$title,
        subtitle = !!paste0(
          cv$k, "-fold CV · mean ", metric, " ", fmt_num(cv$mean), " ± ", fmt_num(cv$sd),
          " (dashed = mean)"
        ),
        x = "fold",
        y = !!metric
      )),
      rlang::expr(theme_preview(!!theme_name, !!base_size))
    )
    return(new_plot_recipe(
      data = folds,
      data_name = "cv_folds",
      layers = layers,
      header = c(paste0("# ", res$fit_label), paste0("# ", cv$k, "-fold CV on the training rows"))
    ))
  }

  if (identical(plot, "subgroup") && !is.null(res$subgroup_table) && nrow(res$subgroup_table)) {
    sg <- res$subgroup_table
    metric_col <- if (identical(res$method, "classification")) "accuracy" else "rmse"
    grp <- res$subgroup
    layers <- list(
      rlang::expr(ggplot2::ggplot(
        subgroup_table,
        ggplot2::aes(x = stats::reorder(!!rlang::sym(grp), !!rlang::sym(metric_col)), y = !!rlang::sym(metric_col))
      )),
      rlang::expr(ggplot2::geom_col(fill = !!accent)),
      rlang::expr(ggplot2::coord_flip()),
      rlang::expr(ggplot2::labs(
        title = !!res$title,
        subtitle = !!paste("Holdout", metric_col, "by", grp),
        x = NULL,
        y = !!metric_col
      )),
      rlang::expr(theme_preview(!!theme_name, !!base_size))
    )
    return(new_plot_recipe(
      data = sg,
      data_name = "subgroup_table",
      layers = layers,
      header = c(paste0("# ", res$fit_label), paste0("# holdout ", metric_col, " by ", grp))
    ))
  }

  if (identical(plot, "residual") && !is.null(res$scores) && nrow(res$scores)) {
    sc <- res$scores
    xcol <- if ("fitted" %in% names(sc)) "fitted" else names(sc)[[1]]
    layers <- list(
      rlang::expr(ggplot2::ggplot(scores, ggplot2::aes(x = !!rlang::sym(xcol), y = residual))),
      rlang::expr(ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#8a5a32")),
      rlang::expr(ggplot2::geom_point(alpha = 0.75, color = !!accent, size = 2.2)),
      rlang::expr(ggplot2::labs(
        title = !!res$title,
        subtitle = "Residuals on the holdout",
        x = !!xcol,
        y = "residual"
      )),
      rlang::expr(theme_preview(!!theme_name, !!base_size))
    )
    return(new_plot_recipe(
      data = sc,
      data_name = "scores",
      layers = layers,
      header = c(paste0("# ", res$fit_label), "# scores: holdout actual / fitted / residual")
    ))
  }

  if (identical(plot, "roc") && !is.null(res$scores) && "fitted" %in% names(res$scores)) {
    sc <- res$scores
    sc$truth <- factor(sc$actual, levels = c(0, 1))
    roc <- tryCatch(yardstick::roc_curve(sc, truth, fitted), error = function(...) NULL)
    if (!is.null(roc) && nrow(roc)) {
      layers <- list(
        rlang::expr(ggplot2::ggplot(roc, ggplot2::aes(x = 1 - specificity, y = sensitivity))),
        rlang::expr(ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#8a5a32")),
        rlang::expr(ggplot2::geom_line(linewidth = 1, color = !!accent)),
        rlang::expr(ggplot2::coord_equal()),
        rlang::expr(ggplot2::labs(title = !!res$title, subtitle = "ROC on the holdout", x = "False positive rate", y = "True positive rate")),
        rlang::expr(theme_preview(!!theme_name, !!base_size))
      )
      return(new_plot_recipe(data = as.data.frame(roc), data_name = "roc", layers = layers,
        header = c(paste0("# ", res$fit_label), "# yardstick::roc_curve")))
    }
  }

  if (identical(plot, "pr") && !is.null(res$scores) && "fitted" %in% names(res$scores)) {
    sc <- res$scores
    sc$truth <- factor(sc$actual, levels = c(0, 1))
    pr <- tryCatch(yardstick::pr_curve(sc, truth, fitted), error = function(...) NULL)
    if (!is.null(pr) && nrow(pr)) {
      layers <- list(
        rlang::expr(ggplot2::ggplot(pr, ggplot2::aes(x = recall, y = precision))),
        rlang::expr(ggplot2::geom_line(linewidth = 1, color = !!accent)),
        rlang::expr(ggplot2::labs(title = !!res$title, subtitle = "Precision–recall on the holdout", x = "recall", y = "precision")),
        rlang::expr(theme_preview(!!theme_name, !!base_size))
      )
      return(new_plot_recipe(data = as.data.frame(pr), data_name = "pr", layers = layers,
        header = c(paste0("# ", res$fit_label), "# yardstick::pr_curve")))
    }
  }

  if (identical(plot, "confusion") && !is.null(res$scores) && "actual_label" %in% names(res$scores)) {
    sc <- res$scores
    th <- res$threshold %or% 0.5
    pred_lab <- if ("predicted_label" %in% names(sc)) {
      sc$predicted_label
    } else if ("fitted" %in% names(sc) && is.numeric(sc$fitted) && n_distinct_safe(sc$actual_label) == 2) {
      levs <- unique(sc$actual_label)
      ifelse(sc$fitted >= th, res$positive %or% levs[[2]], res$negative %or% levs[[1]])
    } else {
      sc$actual_label
    }
    tab <- as.data.frame(table(actual = sc$actual_label, predicted = pred_lab), stringsAsFactors = FALSE)
    names(tab)[3] <- "n"
    layers <- list(
      rlang::expr(ggplot2::ggplot(cm, ggplot2::aes(x = predicted, y = actual, fill = n))),
      rlang::expr(ggplot2::geom_tile()),
      rlang::expr(ggplot2::geom_text(ggplot2::aes(label = n), color = "white")),
      rlang::expr(ggplot2::labs(title = !!res$title, subtitle = "Confusion matrix (holdout)", x = "predicted", y = "actual")),
      rlang::expr(theme_preview(!!theme_name, !!base_size))
    )
    return(new_plot_recipe(data = tab, data_name = "cm", layers = layers,
      header = c(paste0("# ", res$fit_label), "# confusion matrix")))
  }

  if (identical(plot, "calibration") && !is.null(res$scores) && is.numeric(res$scores$fitted)) {
    sc <- res$scores
    br <- cut(sc$fitted, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)
    cal <- data.frame(
      pred = as.numeric(tapply(sc$fitted, br, mean)),
      obs = as.numeric(tapply(sc$actual, br, mean))
    )
    cal <- cal[is.finite(cal$pred) & is.finite(cal$obs), , drop = FALSE]
    layers <- list(
      rlang::expr(ggplot2::ggplot(cal, ggplot2::aes(x = pred, y = obs))),
      rlang::expr(ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#8a5a32")),
      rlang::expr(ggplot2::geom_point(size = 2.6, color = !!accent)),
      rlang::expr(ggplot2::geom_line(color = !!accent)),
      rlang::expr(ggplot2::labs(title = !!res$title, subtitle = "Calibration (binned holdout)", x = "mean predicted", y = "mean observed")),
      rlang::expr(theme_preview(!!theme_name, !!base_size))
    )
    return(new_plot_recipe(data = cal, data_name = "cal", layers = layers,
      header = c(paste0("# ", res$fit_label), "# calibration")))
  }

  if (identical(plot, "scree") && !is.null(res$scree)) {
    layers <- list(
      rlang::expr(ggplot2::ggplot(scree, ggplot2::aes(x = term, y = estimate))),
      rlang::expr(ggplot2::geom_col(fill = !!accent)),
      rlang::expr(ggplot2::labs(title = !!res$title, subtitle = "Variance explained", x = NULL, y = "share")),
      rlang::expr(theme_preview(!!theme_name, !!base_size))
    )
    return(new_plot_recipe(data = res$scree, data_name = "scree", layers = layers,
      header = c(paste0("# ", res$fit_label), "# PCA scree")))
  }

  if (identical(plot, "biplot") && !is.null(res$scores) && "PC1" %in% names(res$scores)) {
    layers <- list(
      rlang::expr(ggplot2::ggplot(scores, ggplot2::aes(x = PC1, y = PC2))),
      rlang::expr(ggplot2::geom_point(alpha = 0.7, color = !!accent)),
      rlang::expr(ggplot2::labs(title = !!res$title, subtitle = "Scores on PC1–PC2")),
      rlang::expr(theme_preview(!!theme_name, !!base_size))
    )
    return(new_plot_recipe(data = res$scores, data_name = "scores", layers = layers,
      header = c(paste0("# ", res$fit_label), "# PCA biplot (scores)")))
  }

  if (identical(plot, "cluster") && !is.null(res$scores) && "cluster" %in% names(res$scores)) {
    layers <- list(
      rlang::expr(ggplot2::ggplot(scores, ggplot2::aes(x = PC1, y = PC2, colour = cluster))),
      rlang::expr(ggplot2::geom_point(alpha = 0.8, size = 2.2)),
      rlang::expr(ggplot2::labs(title = !!res$title, subtitle = "k-means on the first two PCs")),
      rlang::expr(theme_preview(!!theme_name, !!base_size))
    )
    return(new_plot_recipe(data = res$scores, data_name = "scores", layers = layers,
      header = c(paste0("# ", res$fit_label), "# k-means clusters")))
  }

  if (identical(plot, "elbow") && !is.null(res$elbow)) {
    layers <- list(
      rlang::expr(ggplot2::ggplot(elbow, ggplot2::aes(x = k, y = tot.withinss))),
      rlang::expr(ggplot2::geom_line(color = !!accent)),
      rlang::expr(ggplot2::geom_point(size = 2.4, color = !!accent)),
      rlang::expr(ggplot2::labs(title = !!res$title, subtitle = "Elbow: total within-cluster SS", x = "k", y = "tot.withinss")),
      rlang::expr(theme_preview(!!theme_name, !!base_size))
    )
    return(new_plot_recipe(data = res$elbow, data_name = "elbow", layers = layers,
      header = c(paste0("# ", res$fit_label), "# k-means elbow")))
  }

  if (identical(plot, "heatmap") && !is.null(res$scores) && all(c("x", "y") %in% names(res$scores))) {
    fill_col <- if ("cor" %in% names(res$scores)) "cor" else if ("residual" %in% names(res$scores)) "residual" else names(res$scores)[[3]]
    layers <- list(
      rlang::expr(ggplot2::ggplot(grid, ggplot2::aes(x = x, y = y, fill = !!rlang::sym(fill_col)))),
      rlang::expr(ggplot2::geom_tile()),
      rlang::expr(ggplot2::scale_fill_gradient2(low = "#8a5a32", mid = "#f6f4ef", high = "#1b3a4b")),
      rlang::expr(ggplot2::labs(title = !!res$title, subtitle = "Pairwise complete observations")),
      rlang::expr(theme_preview(!!theme_name, !!base_size))
    )
    return(new_plot_recipe(data = res$scores, data_name = "grid", layers = layers,
      header = c(paste0("# ", res$fit_label), "# correlation / residual heatmap")))
  }

  if (identical(plot, "stl") && !is.null(res$stl)) {
    stl <- res$stl
    stl_long <- rbind(
      data.frame(period = stl$period, component = "trend", value = stl$trend),
      data.frame(period = stl$period, component = "seasonal", value = stl$seasonal),
      data.frame(period = stl$period, component = "remainder", value = stl$remainder)
    )
    layers <- list(
      rlang::expr(ggplot2::ggplot(stl_long, ggplot2::aes(x = period, y = value))),
      rlang::expr(ggplot2::geom_line(color = !!accent)),
      rlang::expr(ggplot2::facet_wrap(~component, scales = "free_y", ncol = 1)),
      rlang::expr(ggplot2::labs(title = !!res$title, subtitle = "STL trend / seasonal / remainder", x = NULL, y = NULL)),
      rlang::expr(theme_preview(!!theme_name, !!base_size))
    )
    return(new_plot_recipe(data = as.data.frame(stl_long), data_name = "stl_long", layers = layers,
      header = c(paste0("# ", res$fit_label), "# stats::stl")))
  }

  if (identical(plot, "tree") && !is.null(res$tree_nodes) && nrow(res$tree_nodes)) {
    nodes <- res$tree_nodes
    edges <- res$tree_edges
    if (is.null(edges)) {
      edges <- data.frame(x = numeric(), y = numeric(), xend = numeric(), yend = numeric())
    }
    layers <- list(
      rlang::expr(ggplot2::ggplot(nodes, ggplot2::aes(x = x, y = y))),
      rlang::expr(ggplot2::geom_segment(
        data = edges,
        ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
        inherit.aes = FALSE,
        color = "#8a5a32"
      )),
      rlang::expr(ggplot2::geom_label(
        ggplot2::aes(label = label, fill = kind),
        size = 3,
        linewidth = 0.2
      )),
      rlang::expr(ggplot2::scale_fill_manual(
        values = c(split = "#f6f4ef", leaf = !!accent),
        guide = "none"
      )),
      rlang::expr(theme_preview(!!theme_name, !!base_size)),
      rlang::expr(ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      )),
      rlang::expr(ggplot2::labs(
        title = !!res$title,
        subtitle = "Decision tree (rpart); leaves show the prediction"
      )),
      rlang::expr(ggplot2::coord_cartesian(clip = "off"))
    )
    return(new_plot_recipe(
      data = nodes,
      data_name = "nodes",
      layers = layers,
      bindings = list(edges = edges),
      header = c(paste0("# ", res$fit_label), "# tree layout from rpart node ids")
    ))
  }

  if (identical(plot, "pdp") && !is.null(res$pdp) && nrow(res$pdp)) {
    pdp <- res$pdp
    feat <- pdp$feature[[1]]
    ylab <- if (identical(res$method, "classification")) {
      "mean predicted P(positive)"
    } else {
      "mean predicted"
    }
    layers <- if (is.numeric(pdp$x)) {
      list(
        rlang::expr(ggplot2::ggplot(pdp, ggplot2::aes(x = x, y = yhat))),
        rlang::expr(ggplot2::geom_line(linewidth = 1, color = !!accent)),
        rlang::expr(ggplot2::geom_point(size = 2.2, color = !!accent))
      )
    } else {
      list(
        rlang::expr(ggplot2::ggplot(pdp, ggplot2::aes(x = stats::reorder(x, yhat), y = yhat))),
        rlang::expr(ggplot2::geom_col(fill = !!accent)),
        rlang::expr(ggplot2::coord_flip())
      )
    }
    layers <- c(layers, list(
      rlang::expr(ggplot2::labs(
        title = !!res$title,
        subtitle = !!paste0(
          "Partial dependence of ", feat,
          " (others held at training median / mode)"
        ),
        x = feat,
        y = ylab
      )),
      rlang::expr(theme_preview(!!theme_name, !!base_size))
    ))
    return(new_plot_recipe(
      data = pdp,
      data_name = "pdp",
      layers = layers,
      header = c(paste0("# ", res$fit_label), paste0("# partial dependence: ", feat))
    ))
  }

  if (identical(plot, "path") && !is.null(res$path) && nrow(res$path)) {
    path <- res$path
    vline <- if (is.finite(res$penalty %or% NA_real_) && (res$penalty %or% 0) > 0) {
      list(rlang::expr(ggplot2::geom_vline(
        xintercept = log(!!res$penalty),
        linetype = "dashed",
        color = "#8a5a32"
      )))
    } else {
      list()
    }
    layers <- c(
      list(
        rlang::expr(ggplot2::ggplot(
          path,
          ggplot2::aes(x = log(lambda), y = estimate, group = term, colour = term)
        )),
        rlang::expr(ggplot2::geom_line(linewidth = 0.7))
      ),
      vline,
      list(
        rlang::expr(ggplot2::labs(
          title = !!res$title,
          subtitle = "glmnet coefficient path; dashed = lambda.1se",
          x = "log(lambda)",
          y = "estimate",
          colour = NULL
        )),
        rlang::expr(theme_preview(!!theme_name, !!base_size))
      )
    )
    return(new_plot_recipe(
      data = path,
      data_name = "path",
      layers = layers,
      header = c(paste0("# ", res$fit_label), "# glmnet regularization path")
    ))
  }

  if (identical(plot, "importance") && !is.null(res$coefs) && nrow(res$coefs)) {
    imp <- res$coefs
    if (!"estimate" %in% names(imp) && "importance" %in% names(imp)) {
      imp$estimate <- imp$importance
    }
    layers <- list(
      rlang::expr(ggplot2::ggplot(imp, ggplot2::aes(x = stats::reorder(term, estimate), y = estimate))),
      rlang::expr(ggplot2::geom_col(fill = !!accent)),
      rlang::expr(ggplot2::coord_flip()),
      rlang::expr(ggplot2::labs(title = !!res$title, subtitle = "Variable importance", x = NULL, y = "importance")),
      rlang::expr(theme_preview(!!theme_name, !!base_size))
    )
    return(new_plot_recipe(data = imp, data_name = "imp", layers = layers,
      header = c(paste0("# ", res$fit_label), "# variable importance")))
  }

  if (!is.null(res$scores) && nrow(res$scores)) {
    sc <- res$scores
    if (identical(res$method, "classification") && "actual_label" %in% names(sc)) {
      layers <- list(
        rlang::expr(ggplot2::ggplot(scores, ggplot2::aes(x = actual_label, y = fitted))),
        rlang::expr(ggplot2::geom_boxplot(fill = "#d9d0c4", color = !!accent)),
        rlang::expr(ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", color = "#8a5a32")),
        rlang::expr(ggplot2::labs(
          title = !!res$title,
          subtitle = !!paste0(
          "Predicted probability on the holdout (",
          res$threshold %or% 0.5, " threshold)"
        ),
          x = !!res$target,
          y = "P(positive)"
        )),
        rlang::expr(theme_preview(!!theme_name, !!base_size))
      )
      return(new_plot_recipe(
        data = sc,
        data_name = "scores",
        layers = layers,
        header = c(paste0("# ", res$fit_label), "# holdout predicted probability by class")
      ))
    }
    layers <- list(
      rlang::expr(ggplot2::ggplot(scores, ggplot2::aes(x = fitted, y = actual))),
      rlang::expr(ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#8a5a32")),
      rlang::expr(ggplot2::geom_point(alpha = 0.75, color = !!accent, size = 2.2)),
      rlang::expr(ggplot2::labs(
        title = !!res$title,
        subtitle = !!paste("Holdout actual vs predicted ·", res$split),
        x = "predicted",
        y = "actual"
      )),
      rlang::expr(theme_preview(!!theme_name, !!base_size))
    )
    return(new_plot_recipe(
      data = sc,
      data_name = "scores",
      layers = layers,
      header = c(paste0("# ", res$fit_label), "# scores: holdout actual vs predicted")
    ))
  }

  failed_recipe("Model updated; nothing to draw")
}

render_model_plot <- function(res, style = NULL) {
  render_recipe(model_plot_recipe(res, style = style))
}

# The Model page always shows two panels: estimates on top, a diagnostic
# chart on the bottom. Both read from the same fitted `res` -- res$scores /
# res$coefs / etc. are populated at fit time regardless of which plot the
# user originally asked for, so picking a different `plot` key per panel
# here is just a render-time choice, not a refit.
estimates_plot_recipe <- function(res, style = NULL) {
  # tree and forest reuse the term/estimate column names for their
  # variable-importance table, but it's not a coefficient with a standard
  # error or CI -- routing it through the "coef" renderer would mislabel it
  # "Coefficient estimates, 95% CI" (and for forest, duplicate the bottom
  # panel's own "importance" chart verbatim). Leave the top panel blank.
  has_coefs <- !is.null(res$coefs) && nrow(res$coefs) &&
    all(c("term", "estimate") %in% names(res$coefs)) &&
    !res$method %in% c("forest", "tree")
  if (is.null(res) || !isTRUE(res$ok) || !has_coefs) {
    return(failed_recipe("No coefficient estimates for this model"))
  }
  res$plot <- "coef"
  model_plot_recipe(res, style = style)
}

# What the bottom panel defaults to when the user hasn't named a plot.
# Coefficients already live in the top panel, so a bottom default of "coef"
# would just repeat it -- point classification/regression at a genuinely
# different view instead. Every other method's default (forecast, tree,
# importance, scree, cluster, heatmap, test, ...) has no coefficient
# equivalent up top, so it passes through unchanged.
secondary_plot_key <- function(res) {
  plot <- res$plot
  if (identical(plot, "coef")) {
    if (identical(res$method, "classification")) {
      return("confusion")
    }
    return("actual_pred")
  }
  plot
}

diagnostic_plot_recipe <- function(res, style = NULL) {
  if (is.null(res) || !isTRUE(res$ok)) {
    return(failed_recipe(res$error %or% "Ask a modeling question"))
  }
  res$plot <- secondary_plot_key(res)
  model_plot_recipe(res, style = style)
}

model_code_chunk <- function(res, interactive = FALSE) {
  if (!isTRUE(res$ok)) {
    return(res$code_text %or% "# Could not fit.")
  }
  rec <- model_plot_recipe(res)
  if (identical(res$method, "forecast")) {
    y <- r_name(res$target)
    fit_lines <- c(
      "series <- # one row per period, from the SQL above",
      paste0("y <- ts(series$", y, ", frequency = ", period_freq(res$period), ")"),
      if (identical(res$engine, "holtwinters")) {
        "fit <- stats::HoltWinters(y)"
      } else if (identical(res$engine, "holt")) {
        "fit <- stats::HoltWinters(y, gamma = FALSE)"
      } else if (identical(res$engine, "baseline")) {
        "# Holdout winner was last-value or seasonal-naive — no extra fit."
      } else {
        c(
          "t <- seq_along(y)",
          paste0("fit <- stats::lm(", y, " ~ t, data = data.frame(", y, " = as.numeric(y), t = t))")
        )
      },
      if (!identical(res$engine, "baseline")) {
        paste0("fc <- stats::predict(fit, n.ahead = ", res$horizon, ", prediction.interval = TRUE)")
      },
      "",
      "# Holdout: last periods vs a last-value / seasonal-naive baseline.",
      paste0("# RMSE ", fmt_num(res$metrics$rmse), "  vs baseline ", fmt_num(res$metrics$baseline_rmse))
    )
  } else {
    fit_lines <- supervised_fit_lines(res)
    if (!is.null(res$cv) && isTRUE(res$cv$ok)) {
      fit_lines <- c(
        fit_lines,
        "",
        paste0("# ", res$cv$k, "-fold CV on train  ·  ",
               res$cv$metric_name, " ", fmt_num(res$cv$mean), " ± ", fmt_num(res$cv$sd))
      )
    }
    if (!is.null(res$subgroup_table) && nrow(res$subgroup_table)) {
      fit_lines <- c(
        fit_lines,
        "",
        paste0("scores |> summarise(..., .by = ", res$subgroup, ")  # holdout by group")
      )
    }
  }

  plot_txt <- if (isTRUE(rec$ok)) {
    pretty_r_code(paste(vapply(rec$layers, deparse_layer, ""), collapse = " +\n  "))
  } else {
    paste0("# ", rec$error)
  }
  if (isTRUE(interactive) && isTRUE(rec$ok)) {
    plot_txt <- paste0("p <- ", plot_txt)
  }

  pretty_r_code(paste(
    c(
      if (nzchar(res$fit_label %or% "")) paste0("# ", res$fit_label),
      if (nzchar(empty_to_null(res$prepare_sql) %or% "") &&
          !identical(trimws(res$prepare_sql), "SELECT * FROM data")) {
        paste0("# ", gsub("\\s+", " ", res$prepare_sql))
      },
      if (nzchar(res$fit_label %or% "")) "",
      pretty_r_code(fit_lines),
      "",
      plot_txt,
      if (isTRUE(interactive)) c("", "ggplotly(p)")
    ),
    collapse = "\n"
  ))
}
