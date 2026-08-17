# PCA, k-means, and a correlation heatmap.

numeric_frame <- function(df, features = character(), exclude = character()) {
  if (length(features)) {
    keep <- features[features %in% names(df)]
  } else {
    keep <- names(df)[vapply(df, is.numeric, logical(1))]
  }
  keep <- setdiff(keep, exclude)
  out <- df[, keep, drop = FALSE]
  out <- out[, vapply(out, is.numeric, logical(1)), drop = FALSE]
  out[stats::complete.cases(out), , drop = FALSE]
}

fit_pca <- function(df, spec, notes = character()) {
  feats <- parse_name_list(spec$features, df)
  mat <- numeric_frame(df, feats)
  if (ncol(mat) < 2) {
    return(model_fail("PCA needs at least two numeric columns.", spec))
  }
  pc <- stats::prcomp(mat, center = TRUE, scale. = TRUE)
  var <- pc$sdev^2
  pct <- var / sum(var)
  scree <- data.frame(
    term = paste0("PC", seq_along(pct)),
    estimate = pct,
    stringsAsFactors = FALSE
  )
  scores <- as.data.frame(pc$x[, 1:min(3, ncol(pc$x)), drop = FALSE])
  loadings <- as.data.frame(pc$rotation[, 1:min(3, ncol(pc$rotation)), drop = FALSE])
  loadings$term <- rownames(pc$rotation)
  rownames(loadings) <- NULL
  plot <- resolve_ml_plot(spec$plot) %or% "scree"
  finish_result(list(
    ok = TRUE,
    method = "pca",
    engine = "prcomp",
    title = spec$title %or% "PCA",
    fit_label = paste0("prcomp · ", ncol(mat), " numeric columns"),
    target = "",
    features = names(mat),
    plot = plot,
    notes = notes,
    metrics = list(
      pc1 = pct[[1]],
      pc2 = if (length(pct) > 1) pct[[2]] else NA_real_,
      n_comp = length(pct)
    ),
    coefs = loadings,
    scores = scores,
    scree = scree,
    pca = pc,
    prepare_sql = spec$prepare_sql %or% "",
    n = nrow(mat),
    n_train = nrow(mat),
    n_test = NA_integer_,
    split = "full sample"
  ))
}

fit_kmeans <- function(df, spec, notes = character()) {
  feats <- parse_name_list(spec$features, df)
  mat <- numeric_frame(df, feats)
  if (ncol(mat) < 2 || nrow(mat) < 6) {
    return(model_fail("k-means needs at least two numeric columns and 6 complete rows.", spec))
  }
  k <- spec$k
  if (is.null(k) || !is.finite(as.numeric(k))) {
    k <- 3L
  }
  k <- as.integer(max(2L, min(8L, k)))
  scaled <- scale(mat)
  set.seed(1)
  elbow <- lapply(2:min(8L, nrow(mat) - 1L), function(kk) {
    km <- stats::kmeans(scaled, centers = kk, nstart = 10)
    data.frame(k = kk, tot.withinss = km$tot.withinss)
  })
  elbow <- do.call(rbind, elbow)
  km <- stats::kmeans(scaled, centers = k, nstart = 25)
  pc <- stats::prcomp(scaled, center = FALSE, scale. = FALSE)
  scores <- data.frame(
    PC1 = pc$x[, 1],
    PC2 = pc$x[, min(2, ncol(pc$x))],
    cluster = factor(km$cluster)
  )
  profile <- as.data.frame(km$centers)
  profile$cluster <- factor(seq_len(k))
  plot <- resolve_ml_plot(spec$plot) %or% "cluster"
  finish_result(list(
    ok = TRUE,
    method = "kmeans",
    engine = "kmeans",
    title = spec$title %or% paste("k-means, k =", k),
    fit_label = paste0("k-means · k = ", k, " · ", ncol(mat), " columns"),
    target = "cluster",
    features = names(mat),
    plot = plot,
    notes = c(notes, "Clusters are a partition of this table, not a natural law."),
    metrics = list(
      k = k,
      tot.withinss = km$tot.withinss,
      betweenss = km$betweenss
    ),
    coefs = profile,
    scores = scores,
    elbow = elbow,
    kmeans = km,
    prepare_sql = spec$prepare_sql %or% "",
    n = nrow(mat),
    n_train = nrow(mat),
    n_test = NA_integer_,
    split = "full sample"
  ))
}

fit_correlation <- function(df, spec, notes = character()) {
  feats <- parse_name_list(spec$features, df)
  mat <- numeric_frame(df, feats)
  if (ncol(mat) < 2) {
    return(model_fail("A correlation heatmap needs at least two numeric columns.", spec))
  }
  corm <- stats::cor(mat, use = "pairwise.complete.obs")
  grid <- as.data.frame(as.table(corm), stringsAsFactors = FALSE)
  names(grid) <- c("x", "y", "cor")
  finish_result(list(
    ok = TRUE,
    method = "correlation",
    engine = "cor",
    title = spec$title %or% "Correlation heatmap",
    fit_label = paste0("pairwise cor · ", ncol(mat), " numeric columns"),
    target = "",
    features = names(mat),
    plot = "heatmap",
    notes = notes,
    metrics = list(n_col = ncol(mat), n = nrow(mat)),
    coefs = grid,
    scores = grid,
    prepare_sql = spec$prepare_sql %or% "",
    n = nrow(mat),
    n_train = nrow(mat),
    n_test = NA_integer_,
    split = "full sample"
  ))
}

fit_unsupervised <- function(df, spec, notes = character()) {
  method <- spec$method
  if (identical(method, "pca")) {
    return(fit_pca(df, spec, notes))
  }
  if (identical(method, "kmeans")) {
    return(fit_kmeans(df, spec, notes))
  }
  fit_correlation(df, spec, notes)
}
