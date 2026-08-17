# Typed tool schemas, sticky style, and richer tool returns.
# Enums are generated from the registries / allowed_* vectors so a new
# theme or geom is one list entry, not a second schema edit.

opt_enum <- function(values, description) {
  ellmer::type_enum(values, description, required = FALSE)
}

opt_string <- function(description) {
  ellmer::type_string(description, required = FALSE)
}

opt_integer <- function(description) {
  ellmer::type_integer(description, required = FALSE)
}

opt_number <- function(description) {
  ellmer::type_number(description, required = FALSE)
}

opt_boolean <- function(description) {
  ellmer::type_boolean(description, required = FALSE)
}

plot_spec_type <- function() {
  ellmer::type_object(
    .description = paste(
      "What to show. Data mapping for the featured ggplot.",
      "Omit a field to let R auto-pick it."
    ),
    from = opt_enum(c("detail", "aggregate", "auto"), "Which SQL result to plot."),
    geom = opt_enum(c(allowed_geoms, "auto"), "ggplot geom. bar is accepted as col."),
    x = opt_string("Column for x, from the chosen SQL result."),
    y = opt_string("Column for y. Omit for histogram/density."),
    color = opt_string("Optional color/fill column."),
    facet = opt_string("Facet wrap column, or the grid's row variable."),
    facet_col = opt_string("Second facet column (grid columns)."),
    facet_scales = opt_enum(c("fixed", "free", "free_x", "free_y"), "Facet panel scales."),
    sort = opt_enum(
      c("auto", "value_asc", "value_desc", "alpha_asc", "alpha_desc"),
      "Reorder discrete x categories."
    ),
    position = opt_enum(c("stack", "dodge", "fill", "identity"), "Bar/area position."),
    title = opt_string("Chart title."),
    subtitle = opt_string("Optional subtitle."),
    caption = opt_string("Optional caption."),
    xlab = opt_string("X axis title. Send this alone and omit SQL to retitle the current chart. Empty string restores the column name."),
    ylab = opt_string("Y axis title. Send this alone and omit SQL to retitle the current chart. Empty string restores the column name."),
    legend = opt_string("Legend title when color/fill is used."),
    annotate = opt_string("Optional callout text."),
    annotate_x = opt_string("X position for the callout. Omit both x and y to pin it to the top-right corner instead of a data point."),
    annotate_y = opt_string("Y position for the callout. Omit both x and y to pin it to the top-right corner instead of a data point."),
    annotate_color = opt_string("Callout text/border color, e.g. #b23a48. Omit for the default ink color."),
    annotate_size = opt_number("Callout font size. Default 3.6."),
    smooth = opt_enum(c("none", "lm", "loess"), "Add a smooth to point/jitter charts."),
    hline = opt_string("Horizontal reference: a number, or mean/median."),
    vline = opt_string("Vertical reference: a number, or mean/median."),
    orientation = opt_enum(c("auto", "horizontal", "vertical"), "auto flips crowded bars; horizontal always flips."),
    ymin = opt_string("Lower bound column for errorbar/ribbon."),
    ymax = opt_string("Upper bound column for errorbar/ribbon."),
    .required = FALSE
  )
}

style_spec_type <- function() {
  ellmer::type_object(
    .description = paste(
      "How it looks. Style is sticky: only fields you send are overwritten,",
      "and they survive later questions until the user hits Reset."
    ),
    theme = opt_enum(names(theme_registry), "ggplot theme."),
    palette = opt_enum(names(palette_registry), "Color palette."),
    accent = opt_string("Single color for one-series charts, e.g. #2c3e50."),
    highlight = opt_string("Category value to emphasize; the rest are grayed."),
    legend_position = opt_enum(c("right", "bottom", "top", "left", "none"), "Legend position."),
    point_size = opt_number("Point size."),
    alpha = opt_number("Point/area transparency, 0-1."),
    linewidth = opt_number("Line width."),
    bar_width = opt_number("Bar width, 0-1."),
    base_size = opt_number("Theme base font size."),
    gridlines = opt_enum(c("both", "x", "y", "none"), "Which gridlines to draw."),
    xlim = opt_string("X zoom as 'min, max'. Uses coord_cartesian (does not drop rows)."),
    ylim = opt_string("Y zoom as 'min, max'. Uses coord_cartesian (does not drop rows)."),
    number_format = opt_enum(c("auto", "dollar", "comma", "percent"), "Tick and label format."),
    x_angle = opt_integer("Rotate x tick labels: 0, 45, or 90."),
    y_scale = opt_enum(c("linear", "log", "sqrt"), "Y scale transform."),
    x_scale = opt_enum(c("linear", "log", "sqrt"), "X scale transform."),
    date_breaks = opt_enum(c("auto", "1 month", "3 months", "1 year"), "Date x breaks."),
    labels = opt_boolean("TRUE to print values on bars/points/lines."),
    label_size = opt_number(
      "Font size of the value labels drawn when labels is TRUE. Default 3. Raise it (e.g. 4-5) to make labels bigger/easier to read, lower it (e.g. 2) to shrink them; headroom above the tallest bar/point is added automatically so larger labels stay clear of the plot edge."
    ),
    direct_labels = opt_boolean("TRUE to label series with ggrepel instead of a legend."),
    .required = FALSE
  )
}

dashboard_tool_arguments <- list(
  title = ellmer::type_string("Short dashboard title for this question."),
  detail_sql = opt_string("DuckDB SQL for line items, or omit if not needed."),
  aggregate_sql = opt_string("DuckDB SQL for a grouped/summary result. Alias every expression."),
  plot = plot_spec_type(),
  style = style_spec_type()
)

dashboard_tool_description <- paste(
  "Update the entire dashboard from up to two read-only SQL queries on the table named data,",
  "and describe a ggplot for the featured chart.",
  "detail_sql = line items (SELECT * FROM data WHERE ...).",
  "aggregate_sql = grouped summary with aliases.",
  "plot = what to show (from/geom/x/y/color/facet/sort/position/titles).",
  "style = how it looks (theme/palette/accent/legend/scales/value labels). Style is sticky across turns.",
  "SQL is optional for restyle or relabel follow-ups: omit both queries and send only the plot/style",
  "fields that changed (xlab, ylab, title, labels, theme, ...). Mapping fields you omit are kept.",
  "Call this once on every data or chart-edit question."
)

model_tool_arguments <- list(
  title = opt_string("Short title for this model. Omit to keep the current title."),
  method = opt_enum(allowed_ml_methods, "regression, classification, forecast, test, …. Omit to keep the current method."),
  prepare_sql = opt_string("Optional DuckDB SELECT for the modeling frame. Omit to keep the current frame."),
  target = opt_string("Column to predict. For forecast, the numeric series. Omit to keep the current target."),
  features = opt_string("Comma-separated predictor columns. Omit to auto-pick, or to keep the current features on a restyle."),
  time_col = opt_string("Date/month column for forecast."),
  horizon = opt_integer("Forecast periods ahead. Default 3."),
  period = opt_enum(c("auto", "month", "week", "day", "year"), "Forecast grain."),
  plot = opt_enum(allowed_ml_plots, "Which model chart to draw. Use subgroup when breaking holdout error down by a category."),
  cv_folds = opt_integer("K-fold CV on training rows. Default 5. 0 to skip."),
  subgroup = opt_string(
    "Categorical column to break holdout error down by (e.g. region). One fit, one table. Never call set_model once per level."
  ),
  interactions = opt_string("Comma-separated interaction (a:b) or polynomial (a^2) terms."),
  auto_select = opt_boolean("TRUE to fit a lasso (mixture = 1) instead of unpenalized lm/glm."),
  k = opt_integer("Number of clusters for k-means (2–8). Default 3."),
  threshold = opt_number("Classification threshold for the confusion matrix. Default 0.5.")
)

default_style <- function() {
  list(
    theme = "paper",
    palette = "okabe_ito",
    accent = accent_ink,
    highlight = "",
    legend_position = "right",
    point_size = 2.4,
    alpha = 0.75,
    linewidth = 1,
    bar_width = NA_real_,
    base_size = 13,
    gridlines = "both",
    xlim = "",
    ylim = "",
    number_format = "auto",
    x_angle = 0,
    y_scale = "linear",
    x_scale = "linear",
    date_breaks = "",
    labels = FALSE,
    label_size = 3,
    direct_labels = FALSE
  )
}

as_named_list <- function(x) {
  if (is.null(x)) {
    return(list())
  }
  if (is.list(x) && !is.data.frame(x)) {
    return(x)
  }
  list()
}

style_value_present <- function(val) {
  if (is.null(val) || length(val) == 0) {
    return(FALSE)
  }
  if (is.character(val) && length(val) == 1 && !nzchar(trimws(val))) {
    return(FALSE)
  }
  if (is.logical(val) && length(val) == 1 && is.na(val)) {
    return(FALSE)
  }
  if (is.numeric(val) && length(val) == 1 && is.na(val)) {
    return(FALSE)
  }
  TRUE
}

merge_style <- function(current, incoming) {
  out <- if (is.null(current) || !length(current)) default_style() else current
  incoming <- as_named_list(incoming)
  for (nm in names(incoming)) {
    if (style_value_present(incoming[[nm]])) {
      out[[nm]] <- incoming[[nm]]
    }
  }
  out
}

apply_style_to_spec <- function(spec, style) {
  style <- merge_style(default_style(), style)
  spec$theme <- style$theme
  spec$palette <- style$palette
  spec$accent <- style$accent
  spec$highlight <- style$highlight
  spec$legend_position <- style$legend_position
  spec$point_size <- style$point_size
  spec$alpha <- style$alpha
  spec$linewidth <- style$linewidth
  spec$bar_width <- style$bar_width
  spec$base_size <- style$base_size
  spec$gridlines <- style$gridlines
  spec$xlim <- style$xlim
  spec$ylim <- style$ylim
  # Number format / tick angle / scales / date breaks may already be chosen
  # by the auto view. Default style values must not blank those choices.
  if (style_value_present(style$number_format) && !identical(style$number_format, "auto")) {
    spec$label_format <- style$number_format
  }
  if (style_value_present(style$x_angle) && !identical(as.numeric(style$x_angle)[[1]], 0)) {
    spec$x_angle <- style$x_angle
  }
  if (style_value_present(style$y_scale) && !identical(style$y_scale, "linear")) {
    spec$y_scale <- style$y_scale
  }
  if (style_value_present(style$x_scale) && !identical(style$x_scale, "linear")) {
    spec$x_scale <- style$x_scale
  }
  if (style_value_present(style$date_breaks) && !identical(style$date_breaks, "auto")) {
    spec$date_breaks <- style$date_breaks
  }
  # Sticky style is the source of truth so "remove the value labels" can
  # turn them off. The old OR with spec$labels made them sticky-on forever.
  spec$labels <- isTRUE(style$labels)
  spec$label_size <- style$label_size
  spec$direct_labels <- isTRUE(style$direct_labels)
  spec
}

plot_mapping_fields <- c(
  "from", "geom", "x", "y", "color", "facet", "facet_col",
  "facet_scales", "sort", "position", "ymin", "ymax"
)

plot_lab_fields <- c(
  "title", "subtitle", "caption", "xlab", "ylab", "legend",
  "annotate", "annotate_x", "annotate_y", "annotate_color", "annotate_size"
)

# NULL in the incoming list means the model omitted the field (keep current).
# A sent empty string clears a lab so ggplot falls back to the column name.
merge_plot_spec <- function(current, incoming) {
  incoming <- as_named_list(incoming)
  current <- as_named_list(current)
  if (!length(current)) {
    return(incoming)
  }
  out <- current
  sent_mapping <- FALSE
  for (nm in names(incoming)) {
    val <- incoming[[nm]]
    if (is.null(val)) {
      next
    }
    out[[nm]] <- val
    if (nm %in% plot_mapping_fields && style_value_present(val)) {
      sent_mapping <- TRUE
    }
  }
  if (sent_mapping) {
    for (nm in plot_lab_fields) {
      if (is.null(incoming[[nm]])) {
        out[[nm]] <- ""
      }
    }
  }
  out
}

reuse_last_queries <- function(detail, agg, current) {
  if (isTRUE(detail$used) || isTRUE(agg$used)) {
    return(list(detail = detail, agg = agg, reused = FALSE))
  }
  list(
    detail = list(
      used = !is.null(current$detail_df),
      ok = TRUE,
      df = current$detail_df,
      sql = current$detail_sql %or% "",
      error = NULL
    ),
    agg = list(
      used = !is.null(current$aggregate_df),
      ok = TRUE,
      df = current$aggregate_df,
      sql = current$aggregate_sql %or% "",
      error = NULL
    ),
    reused = TRUE
  )
}

flatten_plot_args <- function(plot) {
  plot <- as_named_list(plot)
  # Exact extract. plot$y would steal ylab ("Revenue (USD)") via R's
  # partial matching and then treat the axis title as a column name.
  grab <- function(nm) {
    if (!nm %in% names(plot)) {
      return(NULL)
    }
    plot[[nm]]
  }
  list(
    from = grab("from"),
    geom = grab("geom"),
    x = grab("x"),
    y = grab("y"),
    color = grab("color"),
    facet = grab("facet"),
    facet_col = grab("facet_col"),
    facet_scales = grab("facet_scales"),
    sort = grab("sort"),
    position = grab("position"),
    title = grab("title"),
    subtitle = grab("subtitle"),
    caption = grab("caption"),
    xlab = grab("xlab"),
    ylab = grab("ylab"),
    legend = grab("legend"),
    annotate = grab("annotate"),
    annotate_x = grab("annotate_x"),
    annotate_y = grab("annotate_y"),
    annotate_color = grab("annotate_color"),
    annotate_size = grab("annotate_size"),
    labels = grab("labels"),
    smooth = grab("smooth"),
    hline = grab("hline"),
    vline = grab("vline"),
    orientation = grab("orientation"),
    ymin = grab("ymin"),
    ymax = grab("ymax")
  )
}

compact_df_md <- function(df, max_rows = 25L, max_cols = 10L) {
  if (is.null(df) || !nrow(df) || !ncol(df)) {
    return("(no rows)")
  }
  notes <- character()
  shown <- df
  if (ncol(shown) > max_cols) {
    shown <- shown[, seq_len(max_cols), drop = FALSE]
    notes <- c(notes, paste0(max_cols, " of ", ncol(df), " columns"))
  }
  if (nrow(shown) > max_rows) {
    shown <- shown[seq_len(max_rows), , drop = FALSE]
    notes <- c(notes, paste0(max_rows, " of ", nrow(df), " rows"))
  }
  fmt_cell <- function(x) {
    if (is.numeric(x)) {
      return(vapply(x, function(v) {
        if (!is.finite(v)) "" else format(signif(v, 4), trim = TRUE, scientific = FALSE)
      }, character(1)))
    }
    as.character(x)
  }
  header <- paste0("| ", paste(names(shown), collapse = " | "), " |")
  sep <- paste0("|", paste(rep("---", ncol(shown)), collapse = "|"), "|")
  rows <- vapply(seq_len(nrow(shown)), function(i) {
    paste0("| ", paste(vapply(shown, function(col) fmt_cell(col[[i]]), character(1)), collapse = " | "), " |")
  }, character(1))
  body <- paste(c(header, sep, rows), collapse = "\n")
  if (length(notes)) {
    paste0(body, "\n\n(truncated: ", paste(notes, collapse = "; "), ")")
  } else {
    body
  }
}

format_dashboard_return <- function(title, spec, detail, agg, notes = character()) {
  plot_line <- if (isTRUE(spec$auto) || is.null(empty_to_null(spec$geom))) {
    "Plot: auto."
  } else {
    paste0(
      "Plot: ", spec$geom, " on ", spec$from,
      " (x=", spec$x, ", y=", spec$y,
      if (nzchar(spec$color %or% "")) paste0(", color=", spec$color) else "",
      if (nzchar(spec$facet %or% "")) paste0(", facet=", spec$facet) else "",
      if (nzchar(spec$xlab %or% "")) paste0(", xlab=", spec$xlab) else "",
      if (nzchar(spec$ylab %or% "")) paste0(", ylab=", spec$ylab) else "",
      if (nzchar(spec$title %or% "")) paste0(", title=", spec$title) else "",
      if (isTRUE(spec$labels)) ", value labels on" else "",
      ")."
    )
  }
  result_df <- if (isTRUE(agg$used) && !is.null(agg$df)) agg$df else if (isTRUE(detail$used)) detail$df else NULL
  result_label <- if (isTRUE(agg$used) && !is.null(agg$df)) "Aggregate" else "Detail"
  paste(
    c(
      paste0("Dashboard updated (", title, ")."),
      if (isTRUE(detail$used)) paste0("Detail: ", nrow(detail$df), " rows.") else "No detail query.",
      if (isTRUE(agg$used)) paste0("Aggregate: ", nrow(agg$df), " rows.") else "No aggregate query.",
      plot_line,
      if (length(notes)) paste(notes, collapse = " "),
      "",
      paste0(result_label, " values (use these numbers when you describe the chart):"),
      compact_df_md(result_df)
    ),
    collapse = "\n"
  )
}

notify_try <- function(expr, what) {
  tryCatch(
    expr,
    error = function(e) {
      msg <- paste0(what, " failed: ", conditionMessage(e))
      session <- shiny::getDefaultReactiveDomain()
      if (!is.null(session)) {
        shiny::showNotification(msg, type = "warning", duration = 6)
      }
      warning(msg, call. = FALSE)
      NULL
    }
  )
}

ellmer_supported <- function(min = "0.4.2") {
  requireNamespace("ellmer", quietly = TRUE) &&
    utils::packageVersion("ellmer") >= min
}

querychat_supported <- function(min = "0.3.0") {
  requireNamespace("querychat", quietly = TRUE) &&
    utils::packageVersion("querychat") >= min
}

set_querychat_data <- function(qc, data_source) {
  # data_source should be the live DBI connection (table "data"), not a
  # data.frame -- a data.frame snapshot freezes rows at assignment time,
  # which is the bug this rebuild used to paper over. Reassigning here
  # forces a fresh DataSource so cached column names pick up a schema
  # change (e.g. a new upload); row reads are always live because the
  # DataSource wraps the connection, not a copy. Not a public API; kept
  # behind a tryCatch as a fallback if assignment fails after an upgrade.
  if (!querychat_supported()) {
    warning("querychat < 0.3.0: cannot attach the loaded table to QueryChat.", call. = FALSE)
    return(FALSE)
  }
  ok <- tryCatch({
    qc$data_source <- data_source
    TRUE
  }, error = function(e) FALSE)
  if (!ok) {
    warning("qc$data_source assignment failed; QueryChat still has the previous frame.", call. = FALSE)
  }
  ok
}
