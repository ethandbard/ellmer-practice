# tidymodels wrappers for the Model page. Forecast stays on HoltWinters.

finite_pairs <- function(actual, pred) {
  ok <- is.finite(actual) & is.finite(pred)
  list(actual = actual[ok], pred = pred[ok])
}

class_prob_column <- function(probs, positive) {
  want <- paste0(".pred_", positive)
  if (want %in% names(probs)) {
    return(as.numeric(probs[[want]]))
  }
  pred_cols <- grep("^\\.pred_", names(probs), value = TRUE)
  if (length(pred_cols)) {
    return(as.numeric(probs[[pred_cols[[length(pred_cols)]]]]))
  }
  stop("No probability columns in predict() output.")
}

choose_lasso_penalty <- function(train, form, method) {
  train_mm <- train
  for (nm in names(train_mm)) {
    if (is.character(train_mm[[nm]])) {
      train_mm[[nm]] <- factor(train_mm[[nm]])
    }
  }
  mm <- tryCatch(stats::model.matrix(form, data = train_mm), error = function(e) e)
  if (inherits(mm, "error") || ncol(mm) < 2) {
    return(0.01)
  }
  x <- mm[, -1, drop = FALSE]
  y_name <- as.character(form[[2]])
  y <- train[[y_name]]
  family <- if (identical(method, "classification") || is.factor(y)) "binomial" else "gaussian"
  if (is.factor(y)) {
    y <- as.integer(y) - 1L
  }
  cv <- tryCatch(
    glmnet::cv.glmnet(x, y, family = family, alpha = 1, nfolds = min(5L, nrow(train))),
    error = function(e) NULL
  )
  if (is.null(cv) || !is.finite(cv$lambda.1se)) {
    return(0.01)
  }
  as.numeric(cv$lambda.1se)
}

make_gam_formula <- function(form, train) {
  y <- as.character(form[[2]])
  rhs_vars <- all.vars(form[[3]])
  if (!length(rhs_vars)) {
    return(form)
  }
  terms <- vapply(rhs_vars, function(f) {
    if (!f %in% names(train)) {
      return(r_name(f))
    }
    x <- train[[f]]
    if (is.numeric(x) && !inherits(x, c("Date", "POSIXt")) && n_distinct_safe(x) >= 10) {
      paste0("s(", r_name(f), ")")
    } else {
      r_name(f)
    }
  }, character(1))
  stats::as.formula(paste(r_name(y), "~", paste(terms, collapse = " + ")), env = baseenv())
}

make_supervised_workflow <- function(form, train, method, engine_kind = "lm", penalty = 0.01) {
  rec <- recipes::recipe(form, data = train) |>
    recipes::step_string2factor(recipes::all_string_predictors())
  if (!engine_kind %in% c("tree", "forest", "gam")) {
    rec <- rec |> recipes::step_dummy(recipes::all_nominal_predictors())
  }
  rec <- rec |> recipes::step_zv(recipes::all_predictors())
  mode <- if (identical(method, "classification")) "classification" else "regression"
  mix <- switch(engine_kind, ridge = 0, elastic = 0.5, lasso = 1, NULL)
  if (!is.null(mix)) {
    rec <- rec |> recipes::step_normalize(recipes::all_numeric_predictors())
    spec <- if (identical(method, "classification")) {
      parsnip::logistic_reg(penalty = penalty, mixture = mix) |>
        parsnip::set_engine("glmnet")
    } else {
      parsnip::linear_reg(penalty = penalty, mixture = mix) |>
        parsnip::set_engine("glmnet")
    }
  } else if (identical(engine_kind, "poisson")) {
    spec <- parsnip::poisson_reg() |> parsnip::set_engine("glm")
  } else if (identical(engine_kind, "tree")) {
    spec <- parsnip::decision_tree(mode = mode, tree_depth = 4) |>
      parsnip::set_engine("rpart")
  } else if (identical(engine_kind, "forest")) {
    spec <- parsnip::rand_forest(mode = mode, trees = 200) |>
      parsnip::set_engine("ranger", importance = "impurity")
  } else if (identical(engine_kind, "gam")) {
    spec <- parsnip::gen_additive_mod(mode = "regression") |> parsnip::set_engine("mgcv")
  } else if (identical(engine_kind, "multinom")) {
    spec <- parsnip::multinom_reg() |> parsnip::set_engine("nnet")
  } else if (identical(method, "classification")) {
    spec <- parsnip::logistic_reg() |> parsnip::set_engine("glm")
  } else {
    spec <- parsnip::linear_reg() |> parsnip::set_engine("lm")
  }
  if (identical(engine_kind, "gam")) {
    workflows::workflow() |>
      workflows::add_recipe(rec) |>
      workflows::add_model(spec, formula = make_gam_formula(form, train))
  } else {
    workflows::workflow() |>
      workflows::add_recipe(rec) |>
      workflows::add_model(spec)
  }
}

predict_supervised <- function(fit, new_data, method, positive = NULL) {
  if (identical(method, "classification")) {
    probs <- stats::predict(fit, new_data = new_data, type = "prob")
    class_prob_column(probs, positive)
  } else {
    as.numeric(stats::predict(fit, new_data = new_data)$.pred)
  }
}

tidy_supervised <- function(fit, engine_kind = "lm", penalty = NULL) {
  parsnip_fit <- workflows::extract_fit_parsnip(fit)
  td <- if (engine_kind %in% c("lasso", "ridge", "elastic")) {
    broom::tidy(parsnip_fit, penalty = penalty)
  } else if (engine_kind %in% c("tree", "forest")) {
    imp <- tryCatch(broom::tidy(parsnip_fit), error = function(...) NULL)
    if (is.null(imp) || !"importance" %in% names(imp)) {
      eng <- tryCatch(workflows::extract_fit_engine(fit), error = function(...) NULL)
      if (inherits(eng, "ranger") && !is.null(eng$variable.importance)) {
        imp <- data.frame(term = names(eng$variable.importance), estimate = as.numeric(eng$variable.importance))
      } else if (inherits(eng, "rpart")) {
        imp <- data.frame(term = names(eng$variable.importance), estimate = as.numeric(eng$variable.importance))
      }
    }
    if (!is.null(imp) && "importance" %in% names(imp) && !"estimate" %in% names(imp)) {
      imp$estimate <- imp$importance
    }
    imp
  } else {
    broom::tidy(parsnip_fit, conf.int = TRUE)
  }
  if (is.null(td) || !nrow(td)) {
    return(NULL)
  }
  if ("statistic" %in% names(td) && !"stat" %in% names(td)) {
    td$stat <- td$statistic
  }
  if (engine_kind %in% c("lasso", "ridge", "elastic") && "estimate" %in% names(td)) {
    td <- td[is.finite(td$estimate) & abs(td$estimate) > 1e-8, , drop = FALSE]
  }
  td
}

logloss_of <- function(actual, pred) {
  ok <- is.finite(actual) & is.finite(pred)
  if (!any(ok)) {
    return(NA_real_)
  }
  truth <- factor(actual[ok], levels = c(0, 1))
  if (n_distinct_safe(truth) < 2) {
    pclip <- pmin(pmax(pred[ok], 1e-6), 1 - 1e-6)
    return(-mean(actual[ok] * log(pclip) + (1 - actual[ok]) * log(1 - pclip)))
  }
  as.numeric(yardstick::mn_log_loss_vec(truth, pred[ok], event_level = "second"))
}

# Binary-heap coordinates from rpart node ids (left = 2n, right = 2n+1).
# Stored as data frames so the recipe can draw the tree without the live fit.
rpart_layout <- function(fit) {
  if (!inherits(fit, "rpart")) {
    return(NULL)
  }
  frame <- fit$frame
  nodes <- as.integer(row.names(frame))
  is_leaf <- as.character(frame$var) == "<leaf>"
  depth <- floor(log2(pmax(nodes, 1)))
  xs <- setNames(rep(NA_real_, length(nodes)), as.character(nodes))
  leaves <- sort(nodes[is_leaf])
  if (length(leaves)) {
    xs[as.character(leaves)] <- seq_along(leaves)
  }
  for (i in order(depth, decreasing = TRUE)) {
    n <- nodes[[i]]
    key <- as.character(n)
    if (!is.na(xs[[key]])) {
      next
    }
    kids <- c(xs[as.character(2 * n)], xs[as.character(2 * n + 1)])
    xs[[key]] <- mean(kids, na.rm = TRUE)
  }
  pred <- if (!is.null(fit$ylevels)) {
    fit$ylevels[frame$yval]
  } else {
    format(signif(as.numeric(frame$yval), 3), trim = TRUE)
  }
  split_var <- as.character(frame$var)
  split_var[is_leaf] <- ""
  nodes_df <- data.frame(
    id = nodes,
    x = as.numeric(xs[as.character(nodes)]),
    y = -as.numeric(depth),
    kind = ifelse(is_leaf, "leaf", "split"),
    n = as.integer(frame$n),
    pred = as.character(pred),
    split = split_var,
    label = ifelse(
      is_leaf,
      paste0(pred, " (n=", frame$n, ")"),
      paste0(split_var, " (n=", frame$n, ")")
    ),
    stringsAsFactors = FALSE
  )
  kids <- nodes[nodes > 1]
  if (!length(kids)) {
    edges <- data.frame(
      x = numeric(), y = numeric(), xend = numeric(), yend = numeric()
    )
  } else {
    parent <- as.integer(kids %/% 2)
    edges <- data.frame(
      x = as.numeric(xs[as.character(parent)]),
      y = -floor(log2(pmax(parent, 1))),
      xend = as.numeric(xs[as.character(kids)]),
      yend = -floor(log2(kids)),
      stringsAsFactors = FALSE
    )
    edges <- edges[is.finite(edges$x) & is.finite(edges$xend), , drop = FALSE]
  }
  list(nodes = nodes_df, edges = edges)
}

pick_pdp_feature <- function(features, train, coefs = NULL) {
  features <- features[features %in% names(train)]
  if (!length(features)) {
    return(NULL)
  }
  numeric_feats <- features[vapply(features, function(f) {
    x <- train[[f]]
    is.numeric(x) && !is.factor(x) && !inherits(x, c("Date", "POSIXt")) &&
      n_distinct_safe(x) >= 5
  }, logical(1))]
  if (!is.null(coefs) && nrow(coefs) && "term" %in% names(coefs) && "estimate" %in% names(coefs)) {
    ranked <- coefs[order(-abs(coefs$estimate)), , drop = FALSE]
    for (term in ranked$term) {
      hit <- numeric_feats[vapply(numeric_feats, function(f) {
        identical(term, f) || startsWith(as.character(term), paste0(f, "_")) ||
          grepl(f, term, fixed = TRUE)
      }, logical(1))]
      if (length(hit)) {
        return(hit[[1]])
      }
    }
  }
  if (length(numeric_feats)) {
    return(numeric_feats[[1]])
  }
  features[[1]]
}

# Mean prediction while one feature walks a grid and the rest stay at
# their training values (median / mode). The textbook "how does this
# column move the answer?" plot.
partial_dependence <- function(fit, train, feature, method, positive = NULL, n_grid = 25L) {
  if (is.null(feature) || !feature %in% names(train)) {
    return(NULL)
  }
  x <- train[[feature]]
  if (is.numeric(x) && !is.factor(x) && !inherits(x, c("Date", "POSIXt"))) {
    rng <- range(x, na.rm = TRUE)
    if (!all(is.finite(rng)) || rng[[1]] == rng[[2]]) {
      return(NULL)
    }
    grid <- seq(rng[[1]], rng[[2]], length.out = n_grid)
    assign_val <- function(val) as.numeric(val)
    x_out <- as.numeric(grid)
  } else {
    tab <- sort(table(as.character(x), useNA = "no"), decreasing = TRUE)
    grid <- names(utils::head(tab, 12L))
    if (!length(grid)) {
      return(NULL)
    }
    assign_val <- function(val) {
      if (is.factor(x)) factor(val, levels = levels(x)) else val
    }
    x_out <- as.character(grid)
  }
  yhat <- vapply(seq_along(grid), function(i) {
    nd <- train
    nd[[feature]] <- assign_val(grid[[i]])
    pred <- tryCatch(
      predict_supervised(fit, nd, method, positive),
      error = function(...) NULL
    )
    if (is.null(pred)) NA_real_ else mean(as.numeric(pred), na.rm = TRUE)
  }, numeric(1))
  out <- data.frame(
    x = x_out,
    yhat = yhat,
    feature = feature,
    stringsAsFactors = FALSE
  )
  out[is.finite(out$yhat), , drop = FALSE]
}

glmnet_path_table <- function(fit) {
  eng <- tryCatch(workflows::extract_fit_engine(fit), error = function(...) NULL)
  if (is.null(eng) || is.null(eng$beta) || is.null(eng$lambda)) {
    return(NULL)
  }
  beta <- as.matrix(eng$beta)
  lambdas <- as.numeric(eng$lambda)
  terms <- rownames(beta)
  if (is.null(terms)) {
    terms <- paste0("x", seq_len(nrow(beta)))
  }
  keep <- seq_along(lambdas)
  if (length(keep) > 40L) {
    keep <- unique(round(seq(1, length(lambdas), length.out = 40)))
  }
  do.call(rbind, lapply(keep, function(j) {
    data.frame(
      term = terms,
      lambda = lambdas[[j]],
      estimate = as.numeric(beta[, j]),
      stringsAsFactors = FALSE
    )
  }))
}

extract_teaching_plots <- function(fit, train, engine_kind, method, positive,
                                   features, coefs, plot, penalty = NULL) {
  out <- list(tree_nodes = NULL, tree_edges = NULL, pdp = NULL, path = NULL)
  want_tree <- identical(plot, "tree") || identical(engine_kind, "tree")
  if (want_tree) {
    eng <- tryCatch(workflows::extract_fit_engine(fit), error = function(...) NULL)
    if (inherits(eng, "rpart")) {
      lay <- rpart_layout(eng)
      if (!is.null(lay)) {
        out$tree_nodes <- lay$nodes
        out$tree_edges <- lay$edges
      }
    }
  }
  want_pdp <- identical(plot, "pdp") || identical(engine_kind, "gam")
  if (want_pdp) {
    feat <- pick_pdp_feature(features, train, coefs)
    if (!is.null(feat)) {
      out$pdp <- tryCatch(
        partial_dependence(fit, train, feat, method, positive),
        error = function(...) NULL
      )
    }
  }
  if (identical(plot, "path") && engine_kind %in% c("lasso", "ridge", "elastic")) {
    out$path <- tryCatch(glmnet_path_table(fit), error = function(...) NULL)
  }
  out
}
