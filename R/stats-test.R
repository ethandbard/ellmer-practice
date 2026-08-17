# method = "test": "is this difference real?"

cohen_d <- function(a, b) {
  a <- a[is.finite(a)]
  b <- b[is.finite(b)]
  if (length(a) < 2 || length(b) < 2) {
    return(NA_real_)
  }
  sp <- sqrt(((length(a) - 1) * stats::var(a) + (length(b) - 1) * stats::var(b)) /
    (length(a) + length(b) - 2))
  if (!is.finite(sp) || sp == 0) {
    return(NA_real_)
  }
  (mean(a) - mean(b)) / sp
}

cramers_v <- function(tab) {
  n <- sum(tab)
  if (!n) {
    return(NA_real_)
  }
  chi <- suppressWarnings(stats::chisq.test(tab)$statistic)
  k <- min(nrow(tab), ncol(tab)) - 1
  if (!is.finite(chi) || k < 1) {
    return(NA_real_)
  }
  as.numeric(sqrt(chi / (n * k)))
}

fit_stat_test <- function(df, spec, notes = character()) {
  target <- match_column(spec$target, df)
  feats <- parse_name_list(spec$features, df)
  other <- if (length(feats)) feats[[1]] else NULL
  if (is.null(target)) {
    target <- first_numeric(df)
  }
  if (is.null(target)) {
    return(model_fail("Name a target column for the test.", spec))
  }
  if (is.null(other)) {
    cats <- names(df)[vapply(df, function(x) is_group_col(x) && n_distinct_safe(x) %in% 2:12, logical(1))]
    nums <- names(df)[vapply(df, is.numeric, logical(1))]
    other <- setdiff(c(cats, nums), target)[1]
  }
  if (is.null(other) || is.na(other)) {
    return(model_fail("Name a second column (group, or the other numeric).", spec))
  }

  a <- df[[target]]
  b <- df[[other]]
  plot <- resolve_ml_plot(spec$plot)

  if (is.numeric(a) && is.numeric(b) && !is_group_col(b)) {
    ct <- stats::cor.test(a, b, use = "complete.obs")
    td <- broom::tidy(ct)
    res_plot <- plot %or% "actual_pred"
    scores <- data.frame(actual = a, fitted = b, residual = a - b)
    return(finish_result(list(
      ok = TRUE,
      method = "test",
      engine = "cor.test",
      title = spec$title %or% paste("Correlation:", target, "vs", other),
      fit_label = paste0("Pearson cor.test · ", target, " vs ", other),
      target = target,
      features = other,
      plot = res_plot,
      notes = c(notes, "Correlation is an association, not a cause."),
      metrics = list(
        estimate = unname(ct$estimate),
        conf.low = ct$conf.int[[1]],
        conf.high = ct$conf.int[[2]],
        p.value = ct$p.value
      ),
      coefs = td,
      scores = scores,
      test_kind = "correlation",
      prepare_sql = spec$prepare_sql %or% "",
      n = sum(stats::complete.cases(a, b)),
      n_train = NA_integer_,
      n_test = NA_integer_,
      split = "full sample"
    )))
  }

  group <- if (is_group_col(b) || (!is.numeric(b))) b else a
  value <- if (is_group_col(b) || (!is.numeric(b))) a else b
  gname <- if (is_group_col(b) || (!is.numeric(b))) other else target
  vname <- if (is_group_col(b) || (!is.numeric(b))) target else other

  if ((is.character(a) || is.factor(a)) && (is.character(b) || is.factor(b))) {
    tab <- table(a, b, useNA = "no")
    test <- if (any(tab < 5)) stats::fisher.test(tab, simulate.p.value = nrow(tab) > 2 || ncol(tab) > 2) else stats::chisq.test(tab)
    td <- broom::tidy(test)
    expected <- tryCatch(as.data.frame(as.table(stats::chisq.test(tab)$stdres)), error = function(...) {
      as.data.frame(tab)
    })
    names(expected) <- c("x", "y", "residual")
    return(finish_result(list(
      ok = TRUE,
      method = "test",
      engine = if (inherits(test, "htest") && grepl("Fisher", test$method, ignore.case = TRUE)) "fisher" else "chisq",
      title = spec$title %or% paste("Association:", target, "×", other),
      fit_label = paste(test$method, "·", target, "×", other),
      target = target,
      features = other,
      plot = plot %or% "heatmap",
      notes = notes,
      metrics = list(
        statistic = unname(test$statistic) %or% NA_real_,
        p.value = test$p.value,
        cramers_v = cramers_v(tab)
      ),
      coefs = td,
      scores = expected,
      test_kind = "chisq",
      prepare_sql = spec$prepare_sql %or% "",
      n = sum(tab),
      n_train = NA_integer_,
      n_test = NA_integer_,
      split = "full sample"
    )))
  }

  if (!is.numeric(value)) {
    return(model_fail("Need a numeric column and a grouping column, or two numerics, or two categoricals.", spec))
  }
  g <- as.character(group)
  keep <- is.finite(value) & !is.na(g) & nzchar(g)
  value <- value[keep]
  g <- g[keep]
  lev <- unique(g)
  if (length(lev) < 2) {
    return(model_fail("The grouping column needs at least two levels.", spec))
  }

  if (length(lev) == 2) {
    x1 <- value[g == lev[[1]]]
    x2 <- value[g == lev[[2]]]
    test <- stats::t.test(x1, x2)
    td <- broom::tidy(test)
    d <- cohen_d(x1, x2)
    scores <- data.frame(actual = value, actual_label = g, fitted = value, residual = 0)
    return(finish_result(list(
      ok = TRUE,
      method = "test",
      engine = "welch_t",
      title = spec$title %or% paste("t-test:", vname, "by", gname),
      fit_label = paste0("Welch t-test · ", vname, " by ", gname),
      target = vname,
      features = gname,
      subgroup = gname,
      plot = plot %or% "subgroup",
      notes = c(notes, "Welch t-test does not assume equal variances. Not causal."),
      metrics = list(
        estimate = unname(diff(c(mean(x2), mean(x1)))),
        conf.low = test$conf.int[[1]],
        conf.high = test$conf.int[[2]],
        p.value = test$p.value,
        cohen_d = d
      ),
      coefs = td,
      scores = scores,
      subgroup_table = data.frame(
        group = lev,
        n = c(length(x1), length(x2)),
        mean = c(mean(x1), mean(x2)),
        stringsAsFactors = FALSE
      ),
      test_kind = "t",
      prepare_sql = spec$prepare_sql %or% "",
      n = length(value),
      n_train = NA_integer_,
      n_test = NA_integer_,
      split = "full sample"
    )))
  }

  fit <- stats::aov(value ~ factor(g))
  td <- broom::tidy(fit)
  tukey <- tryCatch(broom::tidy(stats::TukeyHSD(fit)), error = function(...) NULL)
  scores <- data.frame(actual = value, actual_label = g, fitted = as.numeric(stats::fitted(fit)), residual = as.numeric(stats::resid(fit)))
  finish_result(list(
    ok = TRUE,
    method = "test",
    engine = "anova",
    title = spec$title %or% paste("ANOVA:", vname, "by", gname),
    fit_label = paste0("One-way ANOVA · ", vname, " by ", gname),
    target = vname,
    features = gname,
    subgroup = gname,
    plot = plot %or% "subgroup",
    notes = c(notes, "ANOVA asks whether any group mean differs. Tukey HSD is in the coefficient table."),
    metrics = list(
      statistic = td$statistic[td$term != "Residuals"][1],
      p.value = td$p.value[td$term != "Residuals"][1]
    ),
    coefs = if (!is.null(tukey) && nrow(tukey)) tukey else td,
    scores = scores,
    test_kind = "anova",
    prepare_sql = spec$prepare_sql %or% "",
    n = length(value),
    n_train = NA_integer_,
    n_test = NA_integer_,
    split = "full sample"
  ))
}
