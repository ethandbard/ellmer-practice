# Lightweight models for the Model page: lm, glm, or a short forecast.
# The chat proposes a spec; R fits it and draws metrics / ggplot.

allowed_ml_methods <- c(
  "regression", "classification", "forecast",
  "poisson", "tree", "forest", "ridge", "elastic", "gam",
  "test", "pca", "kmeans", "correlation"
)

method_aliases <- c(
  lm = "regression",
  linear = "regression",
  ols = "regression",
  regress = "regression",
  glm = "classification",
  logistic = "classification",
  binary = "classification",
  class = "classification",
  classify = "classification",
  logit = "classification",
  timeseries = "forecast",
  "time-series" = "forecast",
  hw = "forecast",
  holt = "forecast",
  holtwinters = "forecast",
  ts = "forecast",
  predict = "regression",
  count = "poisson",
  counts = "poisson",
  rpart = "tree",
  cart = "tree",
  dt = "tree",
  rf = "forest",
  ranger = "forest",
  randomforest = "forest",
  enet = "elastic",
  elasticnet = "elastic",
  spline = "gam",
  additive = "gam",
  ttest = "test",
  anova = "test",
  chisq = "test",
  inference = "test",
  hypothesis = "test",
  princomp = "pca",
  cluster = "kmeans",
  clusters = "kmeans",
  corr = "correlation",
  cor = "correlation",
  heatmap = "correlation"
)

allowed_ml_plots <- c(
  "coef", "actual_pred", "residual", "forecast", "cv", "subgroup",
  "roc", "pr", "confusion", "calibration",
  "tree", "importance", "pdp", "path",
  "scree", "biplot", "cluster", "elbow",
  "heatmap", "stl", "test"
)

plot_aliases <- c(
  coefficients = "coef",
  coefficient = "coef",
  estimates = "coef",
  pred = "actual_pred",
  predicted = "actual_pred",
  actualvspredicted = "actual_pred",
  fit = "actual_pred",
  fitted = "actual_pred",
  scatter = "actual_pred",
  residuals = "residual",
  resid = "residual",
  diagnostic = "residual",
  roccurve = "roc",
  auc = "roc",
  prcurve = "pr",
  precisionrecall = "pr",
  confmat = "confusion",
  confusionmatrix = "confusion",
  cal = "calibration",
  reliability = "calibration",
  rpart = "tree",
  vi = "importance",
  varimp = "importance",
  partial = "pdp",
  partialdependence = "pdp",
  smooth = "pdp",
  marginal = "pdp",
  ice = "pdp",
  regularizationpath = "path",
  lambdapath = "path",
  coefpath = "path",
  pca = "scree",
  clusters = "cluster",
  wss = "elbow",
  corr = "heatmap",
  decompose = "stl",
  decomposition = "stl",
  future = "forecast",
  horizon = "forecast",
  timeseries = "forecast",
  folds = "cv",
  crossval = "cv",
  crossvalidation = "cv",
  segment = "subgroup",
  segments = "subgroup",
  breakdown = "subgroup",
  bygroup = "subgroup"
)

build_modeler_prompt <- function(columns, n, label, extra_path = "ml-instructions.md", df = NULL) {
  extra <- if (file.exists(extra_path)) {
    paste(readLines(extra_path, warn = FALSE), collapse = "\n")
  } else {
    ""
  }
  paste(extra, schema_note(columns, n, label, df), sep = "\n\n")
}

dataset_model_greeting <- function(label, columns, n) {
  cols <- paste0("`", columns, "`", collapse = ", ")
  paste0(
    "Now modeling **", label, "**.\n\n",
    "DuckDB table: `data` · ", format(n, big.mark = ","), " rows.\n\n",
    "Columns: ", cols, ".\n\n",
    "Ask to **regress**, **classify**, **forecast**, **test**, or cluster. I'll fit a small model and show metrics + a ggplot.\n\n",
    "I've already fit a starting model. ",
    "<span class=\"suggestion\">Tell me about this preview model</span>"
  )
}

resolve_ml_method <- function(x) {
  key <- normalize_key(x)
  if (is.null(key)) {
    return(NULL)
  }
  if (key %in% allowed_ml_methods) {
    return(key)
  }
  # NA from an unmatched lookup would pass the is.null() check in
  # run_model_request() and fall through to the regression branch, silently
  # fitting the wrong family. Unknown methods must return NULL so it errors.
  alias <- unname(method_aliases[key])
  if (length(alias) == 1 && !is.na(alias)) {
    return(alias)
  }
  NULL
}

resolve_ml_plot <- function(x) {
  key <- normalize_key(x)
  if (is.null(key) || key %in% c("auto", "default")) {
    return(NULL)
  }
  if (key %in% allowed_ml_plots) {
    return(key)
  }
  alias <- unname(plot_aliases[key])
  if (length(alias) == 1 && !is.na(alias)) {
    return(alias)
  }
  NULL
}

resolve_period <- function(x) {
  key <- normalize_key(x)
  if (is.null(key) || key %in% c("auto", "default")) {
    return("auto")
  }
  if (key %in% c("month", "monthly", "mo")) {
    return("month")
  }
  if (key %in% c("week", "weekly", "wk")) {
    return("week")
  }
  if (key %in% c("day", "daily", "date")) {
    return("day")
  }
  if (key %in% c("year", "yearly", "annual")) {
    return("year")
  }
  "auto"
}

period_trunc <- function(period) {
  switch(period, week = "week", day = "day", year = "year", "month")
}

period_freq <- function(period) {
  switch(period, week = 52, day = 7, year = 1, 12)
}

period_by <- function(period) {
  switch(period, week = "week", day = "day", year = "year", "month")
}

parse_name_list <- function(x, df) {
  raw <- empty_to_null(x)
  if (is.null(raw) || is.null(df)) {
    return(character())
  }
  parts <- trimws(unlist(strsplit(raw, "[,;]+")))
  parts <- parts[nzchar(parts)]
  resolved <- character()
  for (nm in parts) {
    hit <- match_column(nm, df)
    if (!is.null(hit)) {
      resolved <- c(resolved, hit)
    }
  }
  unique(resolved)
}

resolve_horizon <- function(x, default = 3L) {
  if (is.numeric(x) && length(x) == 1 && is.finite(x)) {
    n <- as.integer(round(x))
  } else {
    raw <- empty_to_null(x)
    if (is.null(raw)) {
      return(default)
    }
    n <- suppressWarnings(as.integer(gsub("[^0-9-]", "", raw)))
  }
  if (is.na(n) || n < 1) {
    return(default)
  }
  as.integer(min(24L, max(1L, n)))
}

resolve_cv_folds <- function(x, default = 5L) {
  # Test the skip vocabulary against the RAW value: empty_to_null() folds
  # "none" to NULL, so checking after it meant `cv_folds = "none"` silently
  # ran 5 folds — the opposite of what the tool description promises.
  skip_key <- if (length(x) == 1 && !is.na(x)) {
    gsub("[^a-z0-9]+", "", tolower(as.character(x)))
  } else {
    ""
  }
  if (nzchar(skip_key) && skip_key %in% c("0", "none", "off", "no", "skip")) {
    return(0L)
  }
  raw <- empty_to_null(x)
  if (is.null(raw)) {
    return(default)
  }
  n <- suppressWarnings(as.integer(round(as.numeric(gsub("[^0-9.-]", "", raw)))))
  if (is.na(n)) {
    return(default)
  }
  if (n <= 1) {
    return(0L)
  }
  as.integer(min(10L, max(2L, n)))
}

r_name <- function(nm) {
  if (identical(make.names(nm), nm)) {
    nm
  } else {
    paste0("`", nm, "`")
  }
}

model_fail <- function(error, spec = list()) {
  list(
    ok = FALSE,
    error = error,
    title = empty_to_null(spec$title) %or% "Model",
    method = spec$method %or% "",
    plot = spec$plot %or% "coef",
    summary_text = paste0("# Could not fit\n\n", error),
    code_text = "# Fix the spec and ask again."
  )
}

infer_period_from_dates <- function(x) {
  if (is_year_number(x)) {
    return("year")
  }
  d <- if (inherits(x, "POSIXt")) {
    as.Date(x)
  } else if (inherits(x, "Date")) {
    x
  } else if (looks_like_date_vec(x)) {
    as.Date(as.character(x))
  } else {
    return("month")
  }
  d <- sort(unique(d[!is.na(d)]))
  if (length(d) < 3) {
    return("month")
  }
  gaps <- as.numeric(diff(d))
  med <- stats::median(gaps, na.rm = TRUE)
  if (is.na(med)) {
    return("month")
  }
  if (med <= 2) {
    "day"
  } else if (med <= 10) {
    "week"
  } else if (med <= 45) {
    "month"
  } else {
    "year"
  }
}

auto_features <- function(df, target, max_num = 6L, max_cat = 3L) {
  prof <- profile_table(df)
  prof <- prof[prof$name != target, , drop = FALSE]
  notes <- character()
  nums <- prof$name[prof$role %in% c("measure", "count")]
  cats <- prof$name[prof$role == "category" & prof$n_distinct >= 2 & prof$n_distinct <= 8]
  if (length(nums) > max_num) {
    notes <- c(notes, paste0("Auto-picked ", max_num, " numeric features."))
    nums <- nums[seq_len(max_num)]
  }
  if (length(cats) > max_cat) {
    notes <- c(notes, paste0("Auto-picked ", max_cat, " categorical features."))
    cats <- cats[seq_len(max_cat)]
  }
  list(features = c(nums, cats), notes = notes)
}

clean_requested_features <- function(df, features, target) {
  notes <- character()
  keep <- character()
  for (nm in features) {
    if (identical(nm, target)) {
      notes <- c(notes, paste0("Dropped `", nm, "` (that is the target)."))
      next
    }
    if (!nm %in% names(df)) {
      next
    }
    x <- df[[nm]]
    if (looks_like_id_name(nm)) {
      notes <- c(notes, paste0("Dropped `", nm, "` (looks like an id)."))
      next
    }
    if (looks_like_date_vec(x)) {
      notes <- c(notes, paste0("Dropped `", nm, "` (a clock — use forecast for time)."))
      next
    }
    if (is_group_col(x) && n_distinct_safe(x) > 20) {
      notes <- c(notes, paste0("Dropped `", nm, "` (", n_distinct_safe(x), " levels)."))
      next
    }
    keep <- c(keep, nm)
  }
  list(features = unique(keep), notes = notes)
}

pick_split_time <- function(df, exclude = character()) {
  nms <- setdiff(names(df), exclude)
  for (nm in nms) {
    if (looks_like_date_vec(df[[nm]])) {
      return(nm)
    }
  }
  NULL
}

split_frame <- function(df, time_col = NULL, strat = NULL, test_frac = 0.2) {
  n <- nrow(df)
  if (n < 40) {
    return(list(
      train = df,
      test = df,
      n_train = n,
      n_test = n,
      label = "in-sample (fewer than 40 rows)",
      in_sample = TRUE
    ))
  }
  n_test <- max(8L, as.integer(round(n * test_frac)))
  n_test <- min(n_test, n - 10L)
  prop <- (n - n_test) / n
  if (!is.null(time_col) && time_col %in% names(df)) {
    df_ord <- df[order(df[[time_col]]), , drop = FALSE]
    spl <- rsample::initial_time_split(df_ord, prop = prop)
    train <- rsample::training(spl)
    test <- rsample::testing(spl)
    return(list(
      train = train,
      test = test,
      n_train = nrow(train),
      n_test = nrow(test),
      label = paste0("chronological holdout on `", time_col, "`"),
      in_sample = FALSE
    ))
  }
  set.seed(1)
  spl <- tryCatch(
    {
      if (!is.null(strat) && strat %in% names(df) && n_distinct_safe(df[[strat]]) >= 2) {
        rlang::inject(rsample::initial_split(df, prop = prop, strata = !!rlang::sym(strat)))
      } else {
        rsample::initial_split(df, prop = prop)
      }
    },
    error = function(...) rsample::initial_split(df, prop = prop)
  )
  train <- rsample::training(spl)
  test <- rsample::testing(spl)
  list(
    train = train,
    test = test,
    n_train = nrow(train),
    n_test = nrow(test),
    label = "random 80/20 holdout (seed 1)",
    in_sample = FALSE
  )
}

sample_frame <- function(df, n_max = 8000L) {
  if (is.null(df) || nrow(df) <= n_max) {
    return(list(df = df, sampled = FALSE, n_raw = if (is.null(df)) 0L else nrow(df)))
  }
  set.seed(1)
  list(
    df = df[sample.int(nrow(df), n_max), , drop = FALSE],
    sampled = TRUE,
    n_raw = nrow(df)
  )
}

run_cv <- function(df, method, y_col, form, k = 5L, min_rows = 30L,
                   engine_kind = "lm", penalty = 0.01, positive = NULL) {
  n <- nrow(df)
  if (n < min_rows) {
    return(list(ok = FALSE, note = paste0("Too few rows (", n, ") for reliable ", k, "-fold CV; showing the single holdout only.")))
  }
  set.seed(1)
  folded <- tryCatch(rsample::vfold_cv(df, v = k), error = function(e) e)
  if (inherits(folded, "error")) {
    return(list(ok = FALSE, note = "Cross-validation folds failed to fit; showing the single holdout only."))
  }

  rows <- lapply(seq_len(nrow(folded)), function(i) {
    train <- rsample::analysis(folded$splits[[i]])
    test <- rsample::assessment(folded$splits[[i]])
    wf <- tryCatch(
      make_supervised_workflow(form, train, method, engine_kind, penalty),
      error = function(e) e
    )
    if (inherits(wf, "error")) {
      return(NULL)
    }
    fit <- tryCatch(workflows::fit(wf, data = train), error = function(e) e)
    if (inherits(fit, "error")) {
      return(NULL)
    }
    pred <- tryCatch(
      predict_supervised(fit, test, method, positive),
      error = function(...) NULL
    )
    if (is.null(pred)) {
      return(NULL)
    }
    actual <- test[[y_col]]
    if (identical(method, "classification")) {
      actual_bin <- if (is.factor(actual)) as.integer(actual) - 1L else as.integer(actual)
      acc <- mean((as.integer(pred >= 0.5)) == actual_bin, na.rm = TRUE)
      data.frame(fold = i, metric = acc, metric2 = auc_binary(actual_bin, pred))
    } else {
      data.frame(fold = i, metric = rmse_of(actual, pred), metric2 = r2_of(actual, pred))
    }
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) < 2) {
    return(list(ok = FALSE, note = "Cross-validation folds failed to fit; showing the single holdout only."))
  }
  folds <- do.call(rbind, rows)
  metric_name <- if (identical(method, "classification")) "accuracy" else "rmse"
  metric2_name <- if (identical(method, "classification")) "auc" else "r2"
  names(folds)[names(folds) == "metric"] <- metric_name
  names(folds)[names(folds) == "metric2"] <- metric2_name
  list(
    ok = TRUE,
    k = length(rows),
    metric_name = metric_name,
    metric2_name = metric2_name,
    folds = folds,
    mean = mean(folds[[metric_name]], na.rm = TRUE),
    sd = stats::sd(folds[[metric_name]], na.rm = TRUE),
    mean2 = mean(folds[[metric2_name]], na.rm = TRUE)
  )
}

binarize_target <- function(x) {
  keep <- !is.na(x)
  ux <- unique(x[keep])
  if (length(ux) != 2) {
    return(NULL)
  }
  labels <- as.character(ux)
  yesish <- tolower(labels) %in% c("1", "yes", "true", "y", "ok", "positive")
  if (any(yesish)) {
    pos <- ux[yesish][[1]]
  } else {
    tab <- vapply(ux, function(v) sum(x[keep] == v), integer(1))
    pos <- ux[[which.min(tab)]]
  }
  neg <- ux[ux != pos][[1]]
  list(
    y = as.integer(x == pos),
    positive = as.character(pos),
    negative = as.character(neg)
  )
}

auc_binary <- function(y, p) {
  y <- as.integer(y)
  ok <- is.finite(p) & !is.na(y)
  y <- y[ok]
  p <- p[ok]
  n1 <- sum(y == 1L)
  n0 <- sum(y == 0L)
  if (n1 == 0L || n0 == 0L) {
    return(NA_real_)
  }
  truth <- factor(y, levels = c(0L, 1L))
  as.numeric(yardstick::roc_auc_vec(truth, p, event_level = "second"))
}

rmse_of <- function(actual, pred) {
  p <- finite_pairs(actual, pred)
  if (!length(p$actual)) {
    return(NA_real_)
  }
  as.numeric(yardstick::rmse_vec(p$actual, p$pred))
}

mae_of <- function(actual, pred) {
  p <- finite_pairs(actual, pred)
  if (!length(p$actual)) {
    return(NA_real_)
  }
  as.numeric(yardstick::mae_vec(p$actual, p$pred))
}

mape_of <- function(actual, pred) {
  ok <- is.finite(actual) & is.finite(pred) & abs(actual) > 1
  if (sum(ok) < 2) {
    return(NA_real_)
  }
  as.numeric(yardstick::mape_vec(actual[ok], pred[ok]))
}

baseline_future <- function(y, horizon, freq, seasonal) {
  n <- length(y)
  if (isTRUE(seasonal) && n >= freq) {
    idx <- n + seq_len(horizon) - freq
    idx[idx < 1] <- pmax(1L, n - freq + 1L)
    return(as.numeric(y[idx]))
  }
  rep(as.numeric(y[[n]]), horizon)
}

clip_nonnegative <- function(res, history) {
  if (is.null(history) || !all(history >= 0, na.rm = TRUE)) {
    return(res)
  }
  if (!is.null(res$forecast)) {
    res$forecast$y <- pmax(res$forecast$y, 0)
    if ("lo" %in% names(res$forecast)) {
      res$forecast$lo <- pmax(res$forecast$lo, 0)
    }
    if ("hi" %in% names(res$forecast)) {
      res$forecast$hi <- pmax(res$forecast$hi, 0)
    }
  }
  if (!is.null(res$series)) {
    fc <- res$series$part == "forecast"
    res$series$y[fc] <- pmax(res$series$y[fc], 0)
    res$series$lo[fc] <- pmax(res$series$lo[fc], 0)
    res$series$hi[fc] <- pmax(res$series$hi[fc], 0)
  }
  res
}

r2_of <- function(actual, pred) {
  ok <- is.finite(actual) & is.finite(pred)
  if (sum(ok) < 3) {
    return(NA_real_)
  }
  if (stats::sd(actual[ok]) == 0) {
    return(NA_real_)
  }
  as.numeric(yardstick::rsq_trad_vec(actual[ok], pred[ok]))
}

coef_table <- function(fit) {
  if (inherits(fit, "workflow")) {
    return(tidy_supervised(fit))
  }
  td <- tryCatch(broom::tidy(fit, conf.int = TRUE), error = function(...) NULL)
  if (is.null(td) || !nrow(td)) {
    return(NULL)
  }
  if ("statistic" %in% names(td) && !"stat" %in% names(td)) {
    td$stat <- td$statistic
  }
  td
}

make_formula <- function(target, features, interactions = character()) {
  base_terms <- vapply(features, r_name, "")
  terms <- c(base_terms, interactions)
  rhs <- if (!length(terms)) "1" else paste(terms, collapse = " + ")
  stats::as.formula(paste(r_name(target), "~", rhs), env = baseenv())
}

add_ci <- function(fit, tbl) {
  if (!is.null(tbl) && all(c("conf.low", "conf.high") %in% names(tbl))) {
    return(tbl)
  }
  extra <- tryCatch(tidy_supervised(fit), error = function(...) NULL)
  if (is.null(extra) || !all(c("term", "conf.low", "conf.high") %in% names(extra))) {
    return(tbl)
  }
  merged <- merge(tbl, extra[, c("term", "conf.low", "conf.high")], by = "term", sort = FALSE)
  merged[match(tbl$term, merged$term), , drop = FALSE]
}

parse_interaction_terms <- function(x, df, features) {
  raw <- empty_to_null(x)
  if (is.null(raw) || is.null(df)) {
    return(list(terms = character(), extra_features = character(), notes = character()))
  }
  parts <- trimws(unlist(strsplit(raw, "[,;]+")))
  parts <- parts[nzchar(parts)]
  terms <- character()
  extra <- character()
  notes <- character()
  for (part in parts) {
    poly_m <- regmatches(part, regexec("^([A-Za-z0-9_.` ]+)\\^([0-9]+)$", part))[[1]]
    if (length(poly_m) == 3) {
      base <- match_column(trimws(poly_m[[2]]), df)
      power <- suppressWarnings(as.integer(poly_m[[3]]))
      if (is.null(base) || is.na(power) || power < 2) {
        notes <- c(notes, paste0("Dropped '", part, "' (unresolved polynomial term)."))
        next
      }
      terms <- c(terms, paste0("I(", r_name(base), "^", power, ")"))
      extra <- c(extra, base)
      next
    }
    sides <- trimws(unlist(strsplit(part, "[:*]")))
    if (length(sides) != 2) {
      notes <- c(notes, paste0("Dropped '", part, "' (expected a:b, a*b, or a^2)."))
      next
    }
    a <- match_column(sides[[1]], df)
    b <- match_column(sides[[2]], df)
    if (is.null(a) || is.null(b)) {
      notes <- c(notes, paste0("Dropped '", part, "' (column not found)."))
      next
    }
    # Main effects for a/b are already ensured via extra_features, so the
    # formula only needs the pairwise interaction term itself.
    terms <- c(terms, paste0(r_name(a), ":", r_name(b)))
    extra <- c(extra, a, b)
  }
  list(terms = unique(terms), extra_features = unique(extra), notes = notes)
}

default_plot_for_method <- function(method) {
  switch(
    method,
    forecast = "forecast",
    classification = "coef",
    tree = "tree",
    forest = "importance",
    test = "test",
    pca = "scree",
    kmeans = "cluster",
    correlation = "heatmap",
    gam = "pdp",
    "actual_pred"
  )
}

align_plot_to_method <- function(plot, method) {
  if (is.null(plot)) {
    return(default_plot_for_method(method))
  }
  if (identical(method, "forecast") && !plot %in% c("forecast", "stl")) {
    return("forecast")
  }
  if (!identical(method, "forecast") && plot %in% c("forecast", "stl")) {
    return(default_plot_for_method(method))
  }
  plot
}

resolve_threshold <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(0.5)
  }
  if (is.character(x) && length(x) == 1L && !nzchar(x)) {
    return(0.5)
  }
  n <- suppressWarnings(as.numeric(x[[1]]))
  if (!is.finite(n)) {
    return(0.5)
  }
  min(max(n, 0), 1)
}

fmt_num <- function(x, digits = 3) {
  if (length(x) != 1 || is.null(x) || is.na(x)) {
    return("—")
  }
  if (is.integer(x) || (is.numeric(x) && abs(x - round(x)) < 1e-9 && abs(x) >= 1)) {
    return(format(round(as.numeric(x)), big.mark = ","))
  }
  format(round(as.numeric(x), digits), nsmall = min(digits, 3), big.mark = ",")
}

fmt_p <- function(p) {
  if (length(p) != 1 || is.na(p)) {
    return("—")
  }
  if (p < 0.001) {
    "<0.001"
  } else {
    formatC(p, digits = 3, format = "f")
  }
}

kv_block <- function(pairs) {
  keys <- names(pairs)
  width <- max(nchar(keys), 8)
  paste(
    vapply(seq_along(pairs), function(i) {
      sprintf(paste0("%-", width, "s  %s"), keys[[i]], pairs[[i]])
    }, character(1)),
    collapse = "\n"
  )
}

format_model_spec <- function(res) {
  if (!isTRUE(res$ok)) {
    return(res$summary_text %or% "# Could not fit")
  }
  head_lines <- c(
    res$title %or% "Model",
    if (nzchar(res$fit_label %or% "") && !identical(res$fit_label, res$title)) {
      res$fit_label
    },
    ""
  )
  meta <- c(
    method = res$method,
    target = res$target
  )
  if (length(res$features)) {
    meta[["features"]] <- paste(res$features, collapse = ", ")
  }
  if (nzchar(res$subgroup %or% "")) {
    meta[["subgroup"]] <- res$subgroup
  }
  if (length(res$interactions)) {
    meta[["interactions"]] <- paste(res$interactions, collapse = ", ")
  }
  if (identical(res$method, "forecast")) {
    meta[["time"]] <- paste0(res$time_col, " · ", res$period)
    meta[["horizon"]] <- as.character(res$horizon)
  }
  if (is.finite(res$n %or% NA_real_)) {
    meta[["n"]] <- paste0(
      fmt_num(res$n),
      if (isTRUE(res$sampled)) paste0(" (sampled from ", fmt_num(res$n_raw), ")") else ""
    )
  }
  if (style_value_present(res$split)) {
    meta[["split"]] <- paste0(
      res$split,
      if (is.finite(res$n_train %or% NA_real_)) {
        paste0(" · train ", fmt_num(res$n_train), " / test ", fmt_num(res$n_test))
      } else {
        ""
      }
    )
  }
  if (identical(res$method, "classification") && !is.null(res$positive)) {
    meta[["positive"]] <- paste0(res$positive, " vs ", res$negative)
  }

  m <- res$metrics
  metrics <- c()
  if (identical(res$method, "classification") || (!is.null(m$accuracy) && is.null(m$rmse))) {
    metrics <- c(
      accuracy = fmt_num(m$accuracy),
      baseline = paste0(fmt_num(m$baseline), " (", m$baseline_name %or% "majority", ")"),
      auc = fmt_num(m$auc),
      logloss = fmt_num(m$logloss)
    )
  } else if (!is.null(m$rmse)) {
    metrics <- c(
      rmse = fmt_num(m$rmse),
      mae = fmt_num(m$mae)
    )
    if (!is.null(m$mape) && !is.na(m$mape) && m$mape < 200) {
      metrics[["mape"]] <- paste0(fmt_num(m$mape, 1), "%")
    }
    if (!is.null(m$r2) && !is.na(m$r2)) {
      metrics[["r2"]] <- fmt_num(m$r2)
    }
    if (!is.null(m$baseline_rmse) && !is.na(m$baseline_rmse)) {
      metrics[["baseline"]] <- paste0(
        fmt_num(m$baseline_rmse),
        " RMSE (", m$baseline_name %or% "mean", ")"
      )
    }
  }
  if (!is.null(m$p.value)) {
    metrics[["p.value"]] <- fmt_p(m$p.value)
  }
  if (!is.null(m$cohen_d) && is.finite(m$cohen_d)) {
    metrics[["cohen_d"]] <- fmt_num(m$cohen_d)
  }
  if (!is.null(m$cramers_v) && is.finite(m$cramers_v)) {
    metrics[["cramers_v"]] <- fmt_num(m$cramers_v)
  }
  if (!is.null(m$estimate) && is.null(m$rmse) && is.null(m$accuracy)) {
    metrics[["estimate"]] <- fmt_num(m$estimate)
  }

  lines <- c(head_lines, kv_block(meta), "", "Holdout", kv_block(metrics))

  if (!is.null(res$coefs) && nrow(res$coefs)) {
    show <- res$coefs
    if (nrow(show) > 14 && "estimate" %in% names(show)) {
      show <- show[order(-abs(show$estimate)), , drop = FALSE]
      show <- show[seq_len(14), , drop = FALSE]
    }
    has_ci <- all(c("conf.low", "conf.high") %in% names(show))
    has_se <- "std.error" %in% names(show)
    coef_lines <- vapply(seq_len(nrow(show)), function(i) {
      row <- show[i, , drop = FALSE]
      base <- sprintf(
        "  %-22s %10s",
        substr(as.character(if ("term" %in% names(row)) row$term[[1]] else i), 1, 22),
        fmt_num(as.numeric((if ("estimate" %in% names(row)) row$estimate else if ("statistic" %in% names(row)) row$statistic else NA)[[1]]))
      )
      if (has_se) {
        base <- sprintf(
          "%s  se %8s  p %s",
          base,
          fmt_num(as.numeric(row$std.error[[1]])),
          fmt_p(as.numeric(row$p.value[[1]]))
        )
      }
      if (has_ci) {
        paste0(
          base, "  95% CI [", fmt_num(as.numeric(row$conf.low[[1]])),
          ", ", fmt_num(as.numeric(row$conf.high[[1]])), "]"
        )
      } else {
        base
      }
    }, character(1))
    lines <- c(lines, "", "Coefficients", coef_lines)
  }

  if (!is.null(res$cv) && isTRUE(res$cv$ok)) {
    cv <- res$cv
    cv_meta <- c(
      folds = as.character(cv$k)
    )
    cv_meta[[cv$metric_name]] <- paste0(fmt_num(cv$mean), " ± ", fmt_num(cv$sd))
    cv_meta[[cv$metric2_name]] <- fmt_num(cv$mean2)
    lines <- c(lines, "", "Cross-validation (training rows)", kv_block(cv_meta))
  }

  if (!is.null(res$subgroup_table) && nrow(res$subgroup_table)) {
    sg <- res$subgroup_table
    metric_col <- if ("accuracy" %in% names(sg)) {
      "accuracy"
    } else if ("rmse" %in% names(sg)) {
      "rmse"
    } else if ("mean" %in% names(sg)) {
      "mean"
    } else {
      names(sg)[names(sg) != res$subgroup][[1]]
    }
    grp_col <- if (res$subgroup %in% names(sg)) res$subgroup else names(sg)[[1]]
    sg_lines <- vapply(seq_len(nrow(sg)), function(i) {
      row <- sg[i, , drop = FALSE]
      sprintf(
        "  %-18s n %-5s %s %8s",
        substr(as.character(row[[grp_col]][[1]]), 1, 18),
        fmt_num(as.numeric(row$n[[1]])),
        metric_col,
        fmt_num(as.numeric(row[[metric_col]][[1]]))
      )
    }, character(1))
    lines <- c(lines, "", paste0("By ", res$subgroup), sg_lines)
  }

  if (identical(res$method, "forecast") && !is.null(res$forecast) && nrow(res$forecast)) {
    fc_lines <- apply(res$forecast, 1, function(row) {
      sprintf(
        "  %s   %s",
        as.character(row[["period"]]),
        fmt_num(as.numeric(row[["y"]]), 2)
      )
    })
    lines <- c(lines, "", "Forecast", fc_lines)
  }

  if (length(res$notes)) {
    lines <- c(lines, "", "Notes", paste0("  ", res$notes))
  }

  paste(lines, collapse = "\n")
}

finish_result <- function(res) {
  res$summary_text <- format_model_spec(res)
  res$code_text <- model_code_chunk(res)
  res
}

load_model_frame <- function(con, prepare_sql, columns) {
  sql <- empty_to_null(prepare_sql)
  if (is.null(sql)) {
    sql <- default_detail_sql()
  }
  res <- run_sql(con, sql)
  if (!res$ok) {
    return(list(
      ok = FALSE,
      error = paste0(
        "prepare_sql failed: ", res$error,
        " Allowed columns: ", paste(columns, collapse = ", "),
        ". The table is named data."
      ),
      sql = sql,
      df = NULL
    ))
  }
  df <- coerce_result_df(res$df)
  if (is.null(df) || !nrow(df)) {
    return(list(ok = FALSE, error = "prepare_sql returned no rows.", sql = sql, df = NULL))
  }
  list(ok = TRUE, error = NULL, sql = res$sql, df = df)
}

fit_supervised <- function(df, method, target, features, title, plot, prepare_sql, notes = character(),
                            interactions = "", cv_folds = 5L, subgroup = "", auto_select = FALSE,
                            threshold = 0.5) {
  spec_method <- method
  if (!target %in% names(df)) {
    return(model_fail(
      paste0("Target `", target, "` is not in the modeling frame. Columns: ", paste(names(df), collapse = ", "), "."),
      list(title = title, method = method, plot = plot)
    ))
  }
  if (!length(features)) {
    auto <- auto_features(df, target)
    features <- auto$features
    notes <- c(notes, auto$notes)
  } else {
    cleaned <- clean_requested_features(df, features, target)
    features <- cleaned$features
    notes <- c(notes, cleaned$notes)
  }
  if (!length(features)) {
    return(model_fail(
      "No usable features. Name a few columns, or pick a numeric target with some predictors.",
      list(title = title, method = method, plot = plot)
    ))
  }

  interact <- parse_interaction_terms(interactions, df, features)
  interaction_terms <- interact$terms
  if (length(interact$extra_features)) {
    features <- unique(c(features, interact$extra_features))
  }
  notes <- c(notes, interact$notes)

  subgroup_col <- NULL
  sg_raw <- empty_to_null(subgroup)
  if (!is.null(sg_raw)) {
    hit <- match_column(sg_raw, df)
    if (is.null(hit)) {
      notes <- c(notes, paste0("Dropped subgroup '", sg_raw, "' (not a column)."))
    } else if (identical(hit, target)) {
      notes <- c(notes, paste0("Dropped subgroup `", hit, "` (that is the target)."))
    } else if (!is_group_col(df[[hit]])) {
      notes <- c(notes, paste0("Dropped subgroup `", hit, "` (not categorical)."))
    } else {
      subgroup_col <- hit
    }
  }

  split_time <- pick_split_time(df, exclude = target)
  used <- unique(c(target, features, split_time, subgroup_col))
  frame <- df[, used, drop = FALSE]
  frame <- frame[stats::complete.cases(frame[, c(target, features), drop = FALSE]), , drop = FALSE]
  if (nrow(frame) < 12) {
    return(model_fail(
      paste0("Only ", nrow(frame), " complete rows after dropping missing values. Need at least 12."),
      list(title = title, method = method, plot = plot)
    ))
  }

  sampled <- sample_frame(frame)
  frame <- sampled$df
  if (sampled$sampled) {
    notes <- c(notes, paste0("Sampled 8,000 of ", format(sampled$n_raw, big.mark = ","), " rows for a snappy preview."))
  }

  positive <- NULL
  negative <- NULL
  work <- frame
  nlev <- n_distinct_safe(work[[target]])
  class_like <- identical(method, "classification") ||
    (method %in% c("tree", "forest", "ridge", "elastic") &&
      (!is.numeric(work[[target]]) || nlev == 2L))
  if (method %in% c("tree", "forest") && class_like) {
    method <- "classification"
  }

  if (identical(method, "classification") || class_like) {
    method <- "classification"
    if (nlev == 2) {
      bin <- binarize_target(work[[target]])
      if (is.null(bin)) {
        return(model_fail("Could not binarize that two-level target.", list(title = title, method = method, plot = plot)))
      }
      work$.class <- factor(
        ifelse(bin$y == 1L, bin$positive, bin$negative),
        levels = c(bin$negative, bin$positive)
      )
      work$.y <- bin$y
      positive <- bin$positive
      negative <- bin$negative
      notes <- c(notes, paste0("Positive class: `", positive, "` (vs `", negative, "`)."))
    } else if (nlev >= 3 && nlev <= 12) {
      work$.class <- factor(work[[target]])
      levs <- levels(work$.class)
      positive <- levs[[length(levs)]]
      negative <- paste(levs[-length(levs)], collapse = "|")
      notes <- c(notes, paste0("Multiclass: ", nlev, " levels. Confusion matrix is the honest plot."))
    } else {
      return(model_fail(
        paste0("`", target, "` has ", nlev, " distinct values. Use 2–12 levels for classification, or pick a numeric target."),
        list(title = title, method = method, plot = plot)
      ))
    }
    y_col <- ".class"
  } else {
    if (!is.numeric(work[[target]])) {
      return(model_fail(
        paste0("Regression target `", target, "` must be numeric. For a yes/no column use classification."),
        list(title = title, method = method, plot = plot)
      ))
    }
    y_col <- target
  }

  if (!is.null(split_time) && split_time %in% names(work)) {
    split <- split_frame(work, time_col = split_time, strat = if (identical(method, "classification")) y_col else NULL)
  } else {
    split <- split_frame(work, strat = if (identical(method, "classification")) y_col else NULL)
  }

  form <- make_formula(y_col, features, interaction_terms)
  requested <- spec_method
  engine_kind <- if (isTRUE(auto_select)) {
    "lasso"
  } else if (requested %in% c("poisson", "tree", "forest", "ridge", "elastic", "gam")) {
    requested
  } else if (identical(method, "classification") && nlev > 2) {
    "multinom"
  } else if (identical(method, "classification")) {
    "glm"
  } else {
    "lm"
  }
  penalty <- NULL
  if (engine_kind %in% c("lasso", "ridge", "elastic")) {
    penalty <- choose_lasso_penalty(split$train, form, method)
    notes <- c(
      notes,
      paste0(
        engine_kind, " (mixture = ", switch(engine_kind, ridge = 0, elastic = 0.5, 1),
        ") with lambda = ", signif(penalty, 3),
        " (cv.glmnet lambda.1se)."
      )
    )
  }

  wf <- tryCatch(
    make_supervised_workflow(form, split$train, method, engine_kind, penalty %or% 0.01),
    error = function(e) e
  )
  if (inherits(wf, "error")) {
    return(model_fail(paste0("Workflow failed: ", conditionMessage(wf)), list(title = title, method = method, plot = plot)))
  }
  fit <- tryCatch(workflows::fit(wf, data = split$train), error = function(e) e)
  if (inherits(fit, "error")) {
    return(model_fail(paste0("Fit failed: ", conditionMessage(fit)), list(title = title, method = method, plot = plot)))
  }

  pred <- tryCatch(
    predict_supervised(fit, split$test, method, positive),
    error = function(e) e
  )
  if (inherits(pred, "error")) {
    return(model_fail(paste0("Predict failed: ", conditionMessage(pred)), list(title = title, method = method, plot = plot)))
  }

  th <- resolve_threshold(threshold)
  actual_raw <- split$test[[y_col]]
  actual <- if (identical(method, "classification") && nlev == 2) {
    as.integer(actual_raw == positive)
  } else {
    actual_raw
  }

  metrics <- list()
  predicted_label <- NULL
  if (identical(method, "classification")) {
    if (nlev == 2) {
      hat <- as.integer(pred >= th)
      acc <- mean(hat == actual, na.rm = TRUE)
      train_y <- as.integer(split$train[[y_col]] == positive)
      base_level <- as.integer(mean(train_y) >= 0.5)
      base_acc <- mean(actual == base_level, na.rm = TRUE)
      predicted_label <- ifelse(hat == 1L, positive, negative)
      metrics <- list(
        accuracy = acc,
        baseline = base_acc,
        baseline_name = paste0("always ", if (base_level == 1) positive else negative),
        auc = auc_binary(actual, pred),
        logloss = logloss_of(actual, pred),
        rmse = NA_real_,
        mae = NA_real_,
        r2 = NA_real_
      )
    } else {
      cls <- tryCatch(
        as.character(stats::predict(fit, new_data = split$test, type = "class")$.pred_class),
        error = function(...) rep(NA_character_, nrow(split$test))
      )
      acc <- mean(cls == as.character(actual_raw), na.rm = TRUE)
      maj <- names(sort(table(as.character(split$train[[y_col]])), decreasing = TRUE))[[1]]
      base_acc <- mean(as.character(actual_raw) == maj, na.rm = TRUE)
      pred <- as.numeric(cls == positive)
      actual <- as.integer(as.character(actual_raw) == positive)
      predicted_label <- cls
      metrics <- list(
        accuracy = acc,
        baseline = base_acc,
        baseline_name = paste0("always ", maj),
        auc = NA_real_,
        logloss = NA_real_,
        rmse = NA_real_,
        mae = NA_real_,
        r2 = NA_real_
      )
    }
    scores <- data.frame(
      actual = actual,
      actual_label = as.character(actual_raw),
      predicted_label = as.character(predicted_label),
      fitted = pred,
      residual = actual - pred,
      stringsAsFactors = FALSE
    )
  } else {
    base_pred <- mean(split$train[[y_col]], na.rm = TRUE)
    metrics <- list(
      rmse = rmse_of(actual, pred),
      mae = mae_of(actual, pred),
      mape = mape_of(actual, pred),
      r2 = r2_of(actual, pred),
      baseline_rmse = rmse_of(actual, rep(base_pred, length(actual))),
      baseline_name = "train mean"
    )
    scores <- data.frame(
      actual = actual,
      fitted = pred,
      residual = actual - pred,
      stringsAsFactors = FALSE
    )
  }

  subgroup_table <- NULL
  if (!is.null(subgroup_col) && subgroup_col %in% names(split$test)) {
    scores$.subgroup <- as.character(split$test[[subgroup_col]])
    subgroup_table <- if (identical(method, "classification")) {
      scores |>
        dplyr::summarise(
          n = dplyr::n(),
          accuracy = mean(as.integer(fitted >= th) == actual, na.rm = TRUE),
          mean_residual = mean(residual, na.rm = TRUE),
          .by = ".subgroup"
        )
    } else {
      scores |>
        dplyr::summarise(
          n = dplyr::n(),
          rmse = sqrt(mean(residual^2, na.rm = TRUE)),
          mae = mean(abs(residual), na.rm = TRUE),
          mean_residual = mean(residual, na.rm = TRUE),
          .by = ".subgroup"
        )
    }
    names(subgroup_table)[names(subgroup_table) == ".subgroup"] <- subgroup_col
    subgroup_table <- subgroup_table[order(-subgroup_table$n), , drop = FALSE]
  }

  coefs <- tidy_supervised(fit, engine_kind, penalty)
  if (identical(engine_kind, "lasso") && !is.null(coefs) && nrow(coefs)) {
    kept <- setdiff(coefs$term, "(Intercept)")
    notes <- c(notes, paste0(
      "Lasso kept ", length(kept), " term", if (length(kept) == 1) "" else "s",
      if (length(kept)) paste0(": ", paste(utils::head(kept, 8), collapse = ", ")) else "",
      "."
    ))
  }
  plot <- align_plot_to_method(plot, spec_method)
  formula_rhs <- paste(c(vapply(features, r_name, ""), interaction_terms), collapse = " + ")
  if (identical(engine_kind, "gam")) {
    formula_rhs <- paste(deparse(make_gam_formula(form, split$train)[[3]]), collapse = " ")
  }
  fit_label <- paste0(
    switch(
      engine_kind,
      lasso = "Lasso",
      ridge = "Ridge",
      elastic = "Elastic net",
      poisson = "Poisson glm",
      tree = "Decision tree (rpart)",
      forest = "Random forest (ranger)",
      gam = "GAM (mgcv)",
      multinom = "Multinomial (nnet)",
      glm = "Logistic glm (parsnip)",
      "Linear model (parsnip)"
    ),
    " · ", r_name(target), " ~ ", formula_rhs
  )
  extras <- extract_teaching_plots(
    fit, split$train, engine_kind, method, positive, features, coefs, plot, penalty
  )

  cv <- NULL
  if (cv_folds > 0) {
    cv_res <- run_cv(
      split$train, method, y_col, form,
      k = cv_folds,
      engine_kind = engine_kind,
      penalty = penalty %or% 0.01,
      positive = positive
    )
    if (isTRUE(cv_res$ok)) {
      cv <- cv_res
    } else if (!is.null(cv_res$note)) {
      notes <- c(notes, cv_res$note)
    }
  }

  finish_result(list(
    ok = TRUE,
    error = NULL,
    notes = notes,
    title = title %or% paste(tools::toTitleCase(method), "of", target),
    method = spec_method,
    engine = engine_kind,
    fit_label = fit_label,
    prepare_sql = prepare_sql %or% "",
    target = target,
    features = features,
    interactions = interaction_terms,
    auto_select = isTRUE(auto_select),
    penalty = penalty,
    time_col = "",
    horizon = NA_integer_,
    period = "",
    plot = plot,
    n = nrow(work),
    n_raw = sampled$n_raw,
    sampled = sampled$sampled,
    n_train = split$n_train,
    n_test = split$n_test,
    split = split$label,
    metrics = metrics,
    coefs = coefs,
    scores = scores,
    subgroup = subgroup_col %or% "",
    subgroup_table = subgroup_table,
    cv = cv,
    series = NULL,
    forecast = NULL,
    positive = positive,
    negative = negative,
    threshold = th,
    tree_nodes = extras$tree_nodes,
    tree_edges = extras$tree_edges,
    pdp = extras$pdp,
    path = extras$path,
    formula_txt = fit_label
  ))
}

as_clock_date <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (inherits(x, "Date")) {
    return(x)
  }
  if (is_year_number(x)) {
    return(as.Date(paste0(as.integer(x), "-01-01")))
  }
  if (looks_like_date_vec(x)) {
    return(as.Date(as.character(x)))
  }
  rep(as.Date(NA), length(x))
}

regularize_series <- function(df, time_col, target, period, fill_zero = TRUE) {
  d <- as_clock_date(df[[time_col]])
  y <- as.numeric(df[[target]])
  keep <- !is.na(d) & is.finite(y)
  d <- d[keep]
  y <- y[keep]
  if (!length(d)) {
    return(NULL)
  }
  agg <- stats::aggregate(y, by = list(period = d), FUN = sum, na.rm = TRUE)
  names(agg) <- c("period", "y")
  agg <- agg[order(agg$period), , drop = FALSE]
  by <- period_by(period)
  full <- tryCatch(
    seq.Date(min(agg$period), max(agg$period), by = by),
    error = function(...) unique(agg$period)
  )
  out <- data.frame(period = full)
  out <- merge(out, agg, by = "period", all.x = TRUE)
  if (fill_zero) {
    out$y[is.na(out$y)] <- 0
  } else {
    out$y <- zoo_na_approx(out$y)
  }
  out[order(out$period), , drop = FALSE]
}

# Avoid a zoo dependency: linear interpolate NAs, edges get last/first observed.
zoo_na_approx <- function(x) {
  if (!anyNA(x)) {
    return(x)
  }
  i <- seq_along(x)
  ok <- is.finite(x)
  if (sum(ok) < 2) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    return(x)
  }
  x[!ok] <- stats::approx(i[ok], x[ok], xout = i[!ok], rule = 2)$y
  x
}

advance_dates <- function(last, period, horizon) {
  seq.Date(last, by = period_by(period), length.out = horizon + 1L)[-1]
}

fit_forecast_series <- function(series, horizon, period, title, plot, prepare_sql, target, time_col, notes = character()) {
  n <- nrow(series)
  if (n < 6) {
    return(model_fail(
      paste0("Forecast needs at least 6 periods; the series has ", n, "."),
      list(title = title, method = "forecast", plot = "forecast")
    ))
  }
  freq <- period_freq(period)
  h <- min(horizon, max(1L, n - 4L))
  hold_n <- min(h, max(2L, floor(n / 5)))
  train_n <- n - hold_n
  y_all <- series$y
  y_train <- y_all[seq_len(train_n)]
  y_hold <- y_all[seq.int(train_n + 1L, n)]

  naive <- rep(y_train[[length(y_train)]], hold_n)
  if (train_n > freq) {
    seasonal <- y_all[seq.int(train_n + 1L, n) - freq]
    if (length(seasonal) == hold_n && all(is.finite(seasonal))) {
      base_pred <- seasonal
      base_name <- paste0("seasonal naive (", period, ")")
    } else {
      base_pred <- naive
      base_name <- "last value"
    }
  } else {
    base_pred <- naive
    base_name <- "last value"
  }

  engine <- "lm"
  fit_label <- paste0("Linear trend · ", target, " by ", period)
  hold_hat <- NULL
  future <- NULL
  future_lo <- NULL
  future_hi <- NULL
  coefs <- NULL

  hw_ok <- FALSE
  if (n >= 8) {
    seasonal_ready <- train_n >= (2 * freq)
    ts_train <- stats::ts(y_train, frequency = freq)
    hw <- tryCatch(
      {
        if (seasonal_ready) {
          stats::HoltWinters(ts_train)
        } else {
          stats::HoltWinters(ts_train, gamma = FALSE)
        }
      },
      error = function(...) NULL
    )
    if (!is.null(hw)) {
      hw_ok <- TRUE
      engine <- if (seasonal_ready) "holtwinters" else "holt"
      fit_label <- if (seasonal_ready) {
        paste0("Holt-Winters seasonal · ", target, " by ", period)
      } else {
        paste0("Holt trend (no seasonal — need ", 2 * freq, " ", period, "s) · ", target)
      }
      if (!seasonal_ready) {
        notes <- c(notes, paste0(
          "Holt-Winters seasonal wants ~", 2 * freq, " ", period,
          "s; fitted a Holt trend instead. Baseline still includes seasonal-naive when possible."
        ))
      }
      hold_pred <- tryCatch(
        as.numeric(stats::predict(hw, n.ahead = hold_n)),
        error = function(...) NULL
      )
      if (!is.null(hold_pred)) {
        hold_hat <- hold_pred
      }
      ts_all <- stats::ts(y_all, frequency = freq)
      hw_all <- tryCatch(
        {
          if (seasonal_ready && n >= (2 * freq)) {
            stats::HoltWinters(ts_all)
          } else {
            stats::HoltWinters(ts_all, gamma = FALSE)
          }
        },
        error = function(...) hw
      )
      fc <- tryCatch(
        stats::predict(hw_all, n.ahead = horizon, prediction.interval = TRUE),
        error = function(...) NULL
      )
      if (!is.null(fc)) {
        future <- as.numeric(fc[, 1])
        future_lo <- as.numeric(fc[, "lwr"])
        future_hi <- as.numeric(fc[, "upr"])
      }
    }
  }

  if (!hw_ok) {
    t_train <- seq_len(train_n)
    fit <- stats::lm(y ~ t, data = data.frame(y = y_train, t = t_train))
    hold_hat <- as.numeric(stats::predict(fit, newdata = data.frame(t = train_n + seq_len(hold_n))))
    fit_all <- stats::lm(y ~ t, data = data.frame(y = y_all, t = seq_len(n)))
    fut_t <- data.frame(t = n + seq_len(horizon))
    fc <- stats::predict(fit_all, newdata = fut_t, interval = "prediction", level = 0.95)
    future <- as.numeric(fc[, "fit"])
    future_lo <- as.numeric(fc[, "lwr"])
    future_hi <- as.numeric(fc[, "upr"])
    coefs <- coef_table(fit_all)
    engine <- "lm"
    fit_label <- paste0("Linear trend · ", target, " by ", period)
  }

  if (is.null(hold_hat) || length(hold_hat) != hold_n) {
    hold_hat <- naive
  }
  if (is.null(future) || !length(future)) {
    last <- y_all[[n]]
    future <- rep(last, horizon)
    spread <- stats::sd(y_all, na.rm = TRUE)
    if (!is.finite(spread)) {
      spread <- 0
    }
    future_lo <- future - spread
    future_hi <- future + spread
  }

  holt_rmse <- rmse_of(y_hold, hold_hat)
  base_rmse <- rmse_of(y_hold, base_pred)
  use_baseline <- !is.finite(holt_rmse) || (is.finite(base_rmse) && base_rmse <= holt_rmse + 1e-9)
  if (use_baseline) {
    seasonal <- grepl("seasonal", base_name, ignore.case = TRUE)
    future <- baseline_future(y_all, horizon, freq, seasonal)
    err <- stats::sd(y_hold - base_pred, na.rm = TRUE)
    if (!is.finite(err) || err == 0) {
      err <- stats::sd(y_all, na.rm = TRUE)
    }
    if (!is.finite(err)) {
      err <- 0
    }
    future_lo <- future - 1.96 * err
    future_hi <- future + 1.96 * err
    hold_hat <- base_pred
    if (is.finite(holt_rmse)) {
      alt <- switch(
        engine,
        holtwinters = "Holt-Winters",
        holt = "Holt",
        "linear trend"
      )
      notes <- c(notes, paste0(
        alt, " holdout RMSE ", fmt_num(holt_rmse),
        " lost to ", base_name, " (", fmt_num(base_rmse), "); using the baseline forecast."
      ))
    }
    engine <- "baseline"
    fit_label <- paste0(tools::toTitleCase(base_name), " · ", target, " by ", period)
  }

  future_dates <- advance_dates(max(series$period), period, horizon)
  forecast_df <- data.frame(
    period = future_dates,
    y = as.numeric(future),
    lo = as.numeric(future_lo),
    hi = as.numeric(future_hi),
    stringsAsFactors = FALSE
  )

  hist <- data.frame(
    period = series$period,
    y = series$y,
    part = ifelse(seq_len(n) <= train_n, "history", "holdout"),
    lo = NA_real_,
    hi = NA_real_,
    stringsAsFactors = FALSE
  )
  fc_plot <- data.frame(
    period = forecast_df$period,
    y = forecast_df$y,
    part = "forecast",
    lo = forecast_df$lo,
    hi = forecast_df$hi,
    stringsAsFactors = FALSE
  )

  scores <- data.frame(
    actual = y_hold,
    fitted = as.numeric(hold_hat),
    residual = y_hold - as.numeric(hold_hat),
    period = series$period[seq.int(train_n + 1L, n)],
    stringsAsFactors = FALSE
  )

  plot <- align_plot_to_method(plot %or% "forecast", "forecast")
  res <- clip_nonnegative(list(
    ok = TRUE,
    error = NULL,
    notes = notes,
    title = title %or% paste("Forecast", target),
    method = "forecast",
    engine = engine,
    fit_label = fit_label,
    prepare_sql = prepare_sql %or% "",
    target = target,
    features = character(),
    time_col = time_col,
    horizon = as.integer(horizon),
    period = period,
    plot = plot,
    n = n,
    n_raw = n,
    sampled = FALSE,
    n_train = train_n,
    n_test = hold_n,
    split = paste0("last ", hold_n, " ", period, "s held out"),
    metrics = list(
      rmse = rmse_of(y_hold, hold_hat),
      mae = mae_of(y_hold, hold_hat),
      mape = mape_of(y_hold, hold_hat),
      r2 = r2_of(y_hold, hold_hat),
      baseline_rmse = rmse_of(y_hold, base_pred),
      baseline_name = base_name
    ),
    coefs = coefs,
    scores = scores,
    series = rbind(hist, fc_plot),
    forecast = forecast_df,
    positive = NULL,
    negative = NULL,
    formula_txt = fit_label
  ), series$y)
  freq <- period_freq(period)
  if (n >= 2 * freq) {
    stl_fit <- tryCatch(
      stats::stl(stats::ts(series$y, frequency = freq), s.window = "periodic"),
      error = function(...) NULL
    )
    if (!is.null(stl_fit)) {
      res$stl <- data.frame(
        period = series$period,
        trend = as.numeric(stl_fit$time.series[, "trend"]),
        seasonal = as.numeric(stl_fit$time.series[, "seasonal"]),
        remainder = as.numeric(stl_fit$time.series[, "remainder"])
      )
    }
  }
  finish_result(res)
}

build_forecast_sql <- function(time_col, target, period, fn = "SUM") {
  sprintf(
    "SELECT date_trunc('%s', TRY_CAST(%s AS DATE)) AS %s, %s(%s) AS %s FROM data GROUP BY 1 ORDER BY 1",
    period_trunc(period),
    sql_ident(time_col),
    sql_ident("period"),
    fn,
    sql_ident(target),
    sql_ident(target)
  )
}

fit_forecast <- function(con, df, spec, columns, notes = character()) {
  period <- resolve_period(spec$period)
  time_col <- match_column(spec$time_col, df)
  target <- match_column(spec$target, df)
  prepared <- !is.null(empty_to_null(spec$prepare_sql))

  if (is.null(target)) {
    target <- first_numeric(df)
  }
  if (is.null(time_col)) {
    time_col <- first_date(df)
    if (is.null(time_col)) {
      for (nm in names(df)) {
        if (looks_like_date_vec(df[[nm]])) {
          time_col <- nm
          break
        }
      }
    }
  }
  if (is.null(time_col) || is.null(target)) {
    return(model_fail(
      paste0(
        "Forecast needs a date column and a numeric measure. ",
        "Columns: ", paste(names(df), collapse = ", "), "."
      ),
      spec
    ))
  }

  if (identical(period, "auto")) {
    period <- infer_period_from_dates(df[[time_col]])
  }

  frame <- df
  sql_used <- spec$prepare_sql %or% ""
  already_series <- n_distinct_safe(df[[time_col]]) == nrow(df) && nrow(df) < 400
  if (!prepared && !already_series) {
    fn <- measure_fn_for(target, df[[target]])
    sql_used <- build_forecast_sql(time_col, target, period, fn)
    loaded <- load_model_frame(con, sql_used, columns)
    if (!loaded$ok) {
      return(model_fail(loaded$error, spec))
    }
    frame <- loaded$df
    time_col <- match_column("period", frame) %or% match_column(time_col, frame) %or% names(frame)[[1]]
    target <- match_column(target, frame) %or% names(frame)[vapply(frame, is.numeric, logical(1))][[1]]
    notes <- c(notes, paste0("Aggregated to one `", target, "` per ", period, "."))
  } else if (prepared) {
    time_col <- match_column(spec$time_col, frame) %or% {
      dates <- names(frame)[vapply(frame, function(x) inherits(x, c("Date", "POSIXt")) || looks_like_date_vec(x), logical(1))]
      if (length(dates)) dates[[1]] else NULL
    }
    target <- match_column(spec$target, frame) %or% first_numeric(frame)
    if (is.null(time_col) || is.null(target)) {
      return(model_fail("prepare_sql must return a date column and a numeric measure.", spec))
    }
    if (identical(period, "auto")) {
      period <- infer_period_from_dates(frame[[time_col]])
    }
  }

  fn_is_sum <- TRUE
  if (target %in% names(df) && !prepared) {
    fn_is_sum <- identical(measure_fn_for(target, df[[target]]), "SUM")
  }
  series <- regularize_series(frame, time_col, target, period, fill_zero = fn_is_sum)
  if (is.null(series) || nrow(series) < 6) {
    return(model_fail(
      paste0("Could not build a regular ", period, " series with at least 6 points."),
      spec
    ))
  }
  if (any(series$y == 0) && fn_is_sum && any(is.na(merge(
    data.frame(period = series$period),
    data.frame(period = as.Date(as.character(frame[[time_col]]))),
    by = "period",
    all.x = TRUE
  )))) {
    notes <- c(notes, paste0("Filled missing ", period, "s with 0."))
  }

  horizon <- resolve_horizon(spec$horizon, 3L)
  title <- empty_to_null(spec$title) %or% paste("Forecast", target, "by", period)
  fit_forecast_series(
    series,
    horizon = horizon,
    period = period,
    title = title,
    plot = spec$plot,
    prepare_sql = sql_used,
    target = target,
    time_col = time_col,
    notes = notes
  )
}

run_model_request <- function(con, spec, columns) {
  method <- resolve_ml_method(spec$method)
  if (is.null(method)) {
    return(model_fail(
      paste0(
        "method must be one of: ",
        paste(allowed_ml_methods, collapse = ", "), "."
      ),
      spec
    ))
  }
  spec$method <- method
  spec$plot <- resolve_ml_plot(spec$plot)
  spec$title <- empty_to_null(spec$title) %or% tools::toTitleCase(method)

  loaded <- load_model_frame(con, spec$prepare_sql, columns)
  if (!loaded$ok) {
    return(model_fail(loaded$error, spec))
  }
  df <- loaded$df
  spec$prepare_sql <- loaded$sql
  notes <- character()

  if (identical(method, "forecast")) {
    return(fit_forecast(con, df, spec, columns, notes))
  }
  if (identical(method, "test")) {
    return(fit_stat_test(df, spec, notes))
  }
  if (method %in% c("pca", "kmeans", "correlation")) {
    return(fit_unsupervised(df, spec, notes))
  }

  target <- match_column(spec$target, df)
  if (is.null(target)) {
    if (identical(method, "classification")) {
      for (nm in names(df)) {
        if (n_distinct_safe(df[[nm]]) == 2 && !looks_like_id_name(nm)) {
          target <- nm
          notes <- c(notes, paste0("Auto-picked binary target `", nm, "`."))
          break
        }
      }
    } else {
      target <- first_numeric(df)
      if (!is.null(target)) {
        notes <- c(notes, paste0("Auto-picked numeric target `", target, "`."))
      }
    }
  }
  if (is.null(target)) {
    return(model_fail(
      paste0("Could not find a ", if (identical(method, "classification")) "binary" else "numeric", " target. Name one."),
      spec
    ))
  }
  features <- parse_name_list(spec$features, df)
  fit_supervised(
    df,
    method = method,
    target = target,
    features = features,
    title = spec$title,
    plot = spec$plot,
    prepare_sql = spec$prepare_sql,
    notes = notes,
    interactions = spec$interactions %or% "",
    cv_folds = resolve_cv_folds(spec$cv_folds),
    subgroup = spec$subgroup %or% "",
    auto_select = is_yes(spec$auto_select),
    threshold = spec$threshold
  )
}

default_model_spec_for <- function(df, label = "Current table") {
  # Real calendars only. Integer `year` (penguins, mpg) is a category, not a clock.
  if (is.null(df) || !ncol(df) || !nrow(df)) {
    return(list(
      title = "Model",
      method = "regression",
      prepare_sql = "",
      target = "",
      features = "",
      time_col = "",
      horizon = "3",
      period = "auto",
      plot = "actual_pred"
    ))
  }
  prof <- profile_table(df)
  date_nm <- pick_clock(prof)
  measure <- pick_measure(prof)
  if (!is.null(date_nm) && !is.null(measure)) {
    return(list(
      title = paste("Forecast", measure),
      method = "forecast",
      prepare_sql = "",
      target = measure,
      features = "",
      time_col = date_nm,
      horizon = "3",
      period = "auto",
      plot = "forecast"
    ))
  }
  target <- measure %or% first_numeric(df)
  list(
    title = if (is.null(target)) "Model" else paste("Linear model of", target),
    method = "regression",
    prepare_sql = "",
    target = target %or% "",
    features = "",
    time_col = "",
    horizon = "3",
    period = "auto",
    plot = "actual_pred"
  )
}

default_model_result <- function(df, con, label = "Current table") {
  tryCatch(
    {
      spec <- default_model_spec_for(df, label)
      run_model_request(con, spec, names(df))
    },
    error = function(e) {
      model_fail(conditionMessage(e), list(title = label, method = "regression"))
    }
  )
}

model_tool_description <- paste(
  "Fit a lightweight model on the DuckDB table named data and update the Model page.",
  "Call this EXACTLY ONCE per user question. Never loop over category levels.",
  "To break results down by a category (region, channel, cuisine): one fit with subgroup=<column> and plot=subgroup.",
  "Do not call set_model once per level, and do not filter prepare_sql to each level.",
  "method = regression, classification, forecast, poisson, tree, forest, ridge, elastic, gam, test, pca, kmeans, or correlation.",
  "prepare_sql = optional SELECT that builds the modeling frame (a slice like WHERE region = 'West' only if they asked for that slice).",
  "target = column to predict (the series for forecast).",
  "features = comma-separated predictors. Omit to auto-pick. Leave empty for forecast.",
  "time_col = date column for forecast.",
  "horizon = integer periods ahead (default 3).",
  "period = month, week, day, year, or auto.",
  "plot = coef, actual_pred, residual, forecast, cv, subgroup, roc, confusion, tree, pdp, path, stl, …",
  "cv_folds, subgroup, interactions, auto_select, and threshold apply to supervised fits.",
  "Fields you omit are kept from the current fit. A breakdown follow-up can be just subgroup (and plot=subgroup).",
  "cv_folds = k-fold CV on training rows (default 5; 0 to skip).",
  "auto_select = true to fit a lasso (mixture = 1) instead of unpenalized lm/glm.",
  "threshold = classification cutoff for labels and the confusion matrix (default 0.5)."
)

model_core_fields <- c("method", "target", "features", "prepare_sql", "time_col")

current_model_spec <- function(res) {
  if (is.null(res) || !isTRUE(res$ok)) {
    return(list())
  }
  list(
    title = res$title %or% "",
    method = res$method,
    prepare_sql = res$prepare_sql %or% "",
    target = res$target %or% "",
    features = if (length(res$features)) paste(res$features, collapse = ", ") else "",
    time_col = res$time_col %or% "",
    horizon = res$horizon,
    period = res$period %or% "",
    plot = res$plot,
    cv_folds = if (!is.null(res$cv) && isTRUE(res$cv$ok)) res$cv$k else 5L,
    subgroup = res$subgroup %or% "",
    interactions = if (length(res$interactions)) paste(res$interactions, collapse = ", ") else "",
    auto_select = isTRUE(res$auto_select),
    k = res$k,
    threshold = res$threshold
  )
}

# NULL incoming field = omitted (keep current). A new target/method without
# features lets R auto-pick. A lone subgroup follow-up keeps the fit and
# switches the chart to the breakdown.
merge_model_spec <- function(current, incoming) {
  incoming <- as_named_list(incoming)
  current <- as_named_list(current)
  out <- current
  sent_core <- FALSE
  for (nm in names(incoming)) {
    val <- incoming[[nm]]
    if (is.null(val)) {
      next
    }
    out[[nm]] <- val
    if (nm %in% model_core_fields && style_value_present(val)) {
      sent_core <- TRUE
    }
  }
  if (sent_core && is.null(incoming$subgroup)) {
    out$subgroup <- ""
  }
  if ((style_value_present(incoming$target) || style_value_present(incoming$method)) &&
      is.null(incoming$features)) {
    out$features <- ""
  }
  if (style_value_present(out$subgroup) && is.null(incoming$plot)) {
    out$plot <- "subgroup"
  }
  out
}
