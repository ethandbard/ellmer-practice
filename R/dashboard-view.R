# Execute up to two read-only SQL queries and draw whatever comes back.

active_table <- function() {
  "data"
}

schema_note <- function(columns, n, label, df = NULL) {
  bits <- c(
    paste0("The active DuckDB table is named `data`."),
    paste0("Loaded from: ", label, " (", format(n, big.mark = ","), " rows)."),
    paste0("Columns: ", paste(columns, collapse = ", "), "."),
    "Use FROM data in every query. Do not invent columns."
  )
  if (!is.null(df) && ncol(df) && nrow(df)) {
    bits <- c(bits, "", format_profile_md(profile_table(df), label, n))
  }
  paste(bits, collapse = "\n")
}

build_analyst_prompt <- function(columns, n, label, extra_path = "extra-instructions.md", df = NULL) {
  extra <- if (file.exists(extra_path)) {
    paste(readLines(extra_path, warn = FALSE), collapse = "\n")
  } else {
    ""
  }
  paste(extra, schema_note(columns, n, label, df), sep = "\n\n")
}

dataset_greeting <- function(label, columns, n) {
  cols <- paste0("`", columns, "`", collapse = ", ")
  paste0(
    "Now exploring **", label, "**.\n\n",
    "DuckDB table: `data` · ", format(n, big.mark = ","), " rows.\n\n",
    "Columns: ", cols, ".\n\n",
    "Ask in English about **this** table. I'll write SQL and a ggplot spec.\n\n",
    "I've already loaded a starting chart. ",
    "<span class=\"suggestion\">Tell me about this preview</span>"
  )
}

describe_dashboard_view <- function(spec, detail_sql, aggregate_sql, title) {
  sql <- empty_to_null(if (identical(spec$from, "detail")) detail_sql else aggregate_sql)
  if (is.null(spec) || isTRUE(spec$auto) || is.null(empty_to_null(spec$geom))) {
    return(paste0(
      "Title: ", title %or% "Untitled", ". ",
      "R auto-picked a chart from the query result shape (no explicit geom set yet). ",
      "SQL: ", sql %or% "(none)"
    ))
  }
  bits <- c(
    paste0("geom = ", spec$geom),
    if (!is.null(empty_to_null(spec$x))) paste0("x = ", spec$x),
    if (!is.null(empty_to_null(spec$y))) paste0("y = ", spec$y),
    if (!is.null(empty_to_null(spec$color))) paste0("color = ", spec$color),
    if (!is.null(empty_to_null(spec$facet))) paste0("facet = ", spec$facet),
    if (!is.null(empty_to_null(spec$facet_col))) paste0("facet_col = ", spec$facet_col),
    if (!is.null(empty_to_null(spec$title))) paste0("title = ", spec$title),
    if (!is.null(empty_to_null(spec$xlab))) paste0("xlab = ", spec$xlab),
    if (!is.null(empty_to_null(spec$ylab))) paste0("ylab = ", spec$ylab),
    if (isTRUE(spec$labels)) "value labels on"
  )
  paste0(
    "Title: ", title %or% "Untitled", ". ",
    "Chart: ", paste(bits, collapse = ", "), ". ",
    "SQL: ", sql %or% "(none)"
  )
}

default_detail_sql <- function(table = active_table()) {
  paste("SELECT * FROM", table)
}

looks_like_id_name <- function(name) {
  grepl("(^id$|_id$|^uuid$|_uuid$|^guid$|^pk$|^index$|^rowid$)", name, ignore.case = TRUE)
}

looks_like_date_vec <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) {
    return(TRUE)
  }
  if (!(is.character(x) || is.factor(x))) {
    return(FALSE)
  }
  xs <- as.character(x)
  sample_vals <- xs[!is.na(xs) & nzchar(xs)]
  if (!length(sample_vals)) {
    return(FALSE)
  }
  all(grepl("^\\d{4}-\\d{2}-\\d{2}", utils::head(sample_vals, 12)))
}

is_group_col <- function(x) {
  (is.character(x) || is.factor(x) || is.logical(x)) && !looks_like_date_vec(x)
}

name_tokens <- function(name) {
  unlist(strsplit(tolower(as.character(name)), "[^a-z0-9]+"))
}

has_token <- function(name, tokens) {
  any(name_tokens(name) %in% tokens)
}

looks_like_money <- function(name) {
  has_token(name, c(
    "revenue", "total", "amount", "sales", "spend", "profit",
    "cost", "fee", "tip", "subtotal", "price"
  ))
}

measure_fn_for <- function(name, x) {
  row <- profile_one(as.character(name)[[1]], x)
  fn <- measure_fn_for_role(row)
  if (!nzchar(fn)) "SUM" else fn
}

is_year_number <- function(x) {
  if (!is.numeric(x) || inherits(x, c("Date", "POSIXt"))) {
    return(FALSE)
  }
  xs <- x[is.finite(x)]
  if (!length(xs)) {
    return(FALSE)
  }
  all(xs == floor(xs)) && min(xs) >= 1900 && max(xs) <= 2100
}

auto_view <- function(df = NULL, table = active_table()) {
  fallback <- list(
    sql = paste("SELECT * FROM", table, "LIMIT 50"),
    spec = default_plot_spec()
  )
  if (is.null(df) || !ncol(df) || !nrow(df)) {
    return(fallback)
  }
  df <- coerce_result_df(df)
  prof <- profile_table(df)
  clock_nm <- pick_clock(prof)
  measure <- pick_measure(prof)
  measures <- profile_cols(prof, "measure")
  cat_nm <- pick_category(prof)

  if (!is.null(clock_nm) && !is.null(measure)) {
    color_nm <- pick_category(prof, exclude = clock_nm, for_color = TRUE)
    color_sql <- if (!is.null(color_nm)) paste0(", ", sql_ident(color_nm)) else ""
    clock_row <- prof[prof$name == clock_nm, ]
    already_month <- identical(clock_row$granularity[[1]], "month")
    x_expr <- if (already_month) {
      sql_ident(clock_nm)
    } else {
      sprintf("date_trunc('month', TRY_CAST(%s AS DATE))", sql_ident(clock_nm))
    }
    x_alias <- if (already_month) clock_nm else "month"
    fn <- prof$measure_fn[prof$name == measure][[1]]
    if (!nzchar(fn)) {
      fn <- "SUM"
    }
    sql <- sprintf(
      "SELECT %s AS %s%s, %s(%s) AS %s FROM %s GROUP BY 1%s ORDER BY 1%s",
      x_expr,
      sql_ident(x_alias),
      color_sql,
      fn,
      sql_ident(measure),
      sql_ident(measure),
      table,
      if (!is.null(color_nm)) ", 2" else "",
      if (!is.null(color_nm)) ", 2" else ""
    )
    # Keep the well-known monthly-by-channel wording the tests pin.
    if (already_month && !is.null(color_nm)) {
      sql <- sprintf(
        "SELECT %s, %s, %s(%s) AS %s FROM %s GROUP BY %s, %s ORDER BY %s, %s",
        sql_ident(clock_nm),
        sql_ident(color_nm),
        fn,
        sql_ident(measure),
        sql_ident(measure),
        table,
        clock_nm,
        color_nm,
        clock_nm,
        color_nm
      )
    }
    title <- if (!is.null(color_nm)) {
      paste("Monthly", measure, "by", color_nm)
    } else {
      paste("Monthly", measure)
    }
    clock_vals <- tryCatch(as.Date(df[[clock_nm]]), error = function(...) NULL)
    n_months <- if (!is.null(clock_vals)) {
      length(unique(format(clock_vals[!is.na(clock_vals)], "%Y-%m")))
    } else {
      clock_row$n_distinct[[1]]
    }
    # Enough months to overlap at 0deg with a 1-month grid -- space the
    # breaks out and angle the labels so they stay legible.
    breaks <- if (n_months <= 8) "1 month" else if (n_months <= 16) "2 months" else if (n_months <= 30) "3 months" else "6 months"
    angle <- if (n_months <= 8) 0 else 45
    return(list(
      sql = sql,
      spec = modifyList(default_plot_spec(), list(
        from = "aggregate",
        geom = "line",
        x = x_alias,
        y = measure,
        color = if (!is.null(color_nm)) color_nm else "",
        title = title,
        subtitle = "Ask a question to change this preview",
        label_format = if (grepl("revenue|total|price|amount|sales", measure, ignore.case = TRUE)) "dollar" else "auto",
        date_breaks = breaks,
        x_angle = angle
      ))
    ))
  }

  if (length(measures) >= 2 && !is.null(cat_nm)) {
    pair <- pick_scatter_pair(prof, measures)
    x <- pair[[1]]
    y <- pair[[2]]
    n_lev <- prof$n_distinct[prof$name == cat_nm][[1]]
    use_facet <- n_lev <= 4
    sql <- sprintf(
      "SELECT %s, %s, %s FROM %s WHERE %s IS NOT NULL AND %s IS NOT NULL",
      sql_ident(x),
      sql_ident(y),
      sql_ident(cat_nm),
      table,
      sql_ident(x),
      sql_ident(y)
    )
    return(list(
      sql = sql,
      spec = modifyList(default_plot_spec(), list(
        from = "aggregate",
        geom = "point",
        x = x,
        y = y,
        color = if (use_facet) "" else cat_nm,
        facet = if (use_facet) cat_nm else "",
        title = paste(y, "vs", x, "by", cat_nm),
        subtitle = "Ask a question to change this preview",
        label_format = "auto",
        date_breaks = "",
        x_angle = 0
      ))
    ))
  }

  if (!is.null(cat_nm) && !is.null(measure)) {
    fn <- prof$measure_fn[prof$name == measure][[1]]
    if (!nzchar(fn)) {
      fn <- "AVG"
    }
    sql <- sprintf(
      "SELECT %s, %s(%s) AS %s FROM %s GROUP BY 1 ORDER BY 2 DESC",
      sql_ident(cat_nm),
      fn,
      sql_ident(measure),
      sql_ident(measure),
      table
    )
    n_groups <- prof$n_distinct[prof$name == cat_nm][[1]]
    return(list(
      sql = sql,
      spec = modifyList(default_plot_spec(), list(
        from = "aggregate",
        geom = "col",
        x = cat_nm,
        y = measure,
        color = "",
        title = paste(measure, "by", cat_nm),
        subtitle = "Ask a question to change this preview",
        label_format = if (grepl("revenue|total|price|amount|sales", measure, ignore.case = TRUE)) "dollar" else "auto",
        date_breaks = "",
        x_angle = if (n_groups > 6) 45 else 0
      ))
    ))
  }

  if (!is.null(measure)) {
    return(list(
      sql = sprintf(
        "SELECT %s FROM %s WHERE %s IS NOT NULL",
        sql_ident(measure),
        table,
        sql_ident(measure)
      ),
      spec = modifyList(default_plot_spec(), list(
        from = "detail",
        geom = "histogram",
        x = measure,
        y = "",
        color = "",
        title = paste("Distribution of", measure),
        subtitle = "Ask a question to change this preview",
        label_format = "auto",
        date_breaks = ""
      ))
    ))
  }

  fallback
}

pick_scatter_pair <- function(prof, measures) {
  tokens_of <- function(nm) unlist(strsplit(tolower(nm), "[^a-z0-9]+"))
  x_score <- function(nm) {
    tok <- tokens_of(nm)
    s <- 0
    if (any(tok %in% c("x", "displ", "disp", "length", "width", "size", "carat", "engine"))) s <- s + 3
    s
  }
  y_score <- function(nm) {
    tok <- tokens_of(nm)
    s <- 0
    if (any(tok %in% c("y", "hwy", "mpg", "price", "rating", "revenue"))) s <- s + 3
    if (any(tok %in% c("depth"))) s <- s + 1
    s
  }
  xs <- vapply(measures, x_score, numeric(1))
  ys <- vapply(measures, y_score, numeric(1))
  x <- measures[[which.max(xs)]]
  rest <- setdiff(measures, x)
  y <- if (length(rest)) rest[[which.max(ys[rest])]] else measures[[min(2, length(measures))]]
  c(x, y)
}

default_view <- function(df = NULL, table = active_table()) {
  auto_view(df, table)
}

default_aggregate_sql <- function(df = NULL, table = active_table()) {
  default_view(df, table)$sql
}

default_title <- function(label = "Current table") {
  label
}

default_plot_spec_for <- function(df = NULL) {
  default_view(df)$spec
}

first_numeric <- function(df, prefer = c("revenue", "total", "quantity", "unit_price", "price", "value")) {
  nums <- names(df)[vapply(df, is.numeric, logical(1))]
  hit <- intersect(prefer, nums)
  if (length(hit)) hit[[1]] else if (length(nums)) nums[[1]] else NULL
}

first_date <- function(df, prefer = c("month", "order_date", "year")) {
  dates <- names(df)[vapply(df, function(x) inherits(x, c("Date", "POSIXt")), logical(1))]
  if (length(intersect(prefer, dates))) {
    return(intersect(prefer, dates)[[1]])
  }
  if (length(dates)) {
    return(dates[[1]])
  }
  if ("year" %in% names(df)) {
    return("year")
  }
  NULL
}

first_cat <- function(df, prefer = c("region", "channel", "category", "product", "city", "customer_type", "promo")) {
  skip <- grepl("id$", names(df), ignore.case = TRUE)
  candidates <- names(df)[!skip]
  cats <- candidates[vapply(df[candidates], function(x) {
    !is.numeric(x) && !inherits(x, c("Date", "POSIXt"))
  }, logical(1))]
  cats <- cats[vapply(cats, function(nm) {
    n <- n_distinct_safe(df[[nm]])
    n >= 1 && n <= 20
  }, logical(1))]
  hit <- intersect(prefer, cats)
  if (length(hit)) hit[[1]] else if (length(cats)) cats[[1]] else NULL
}

pretty_sql <- function(sql) {
  sql <- empty_to_null(sql)
  if (is.null(sql)) {
    return("(none for this question)")
  }
  format_sql(sql)
}

# --- Explicit ggplot spec (model proposes, R builds) ---

allowed_geoms <- c(
  "col", "line", "point", "histogram", "boxplot", "density", "area",
  "violin", "jitter", "tile", "hex", "bin2d", "ecdf", "qq",
  "errorbar", "pointrange", "step", "ribbon"
)

geom_aliases <- c(
  bar = "col",
  bars = "col",
  column = "col",
  columns = "col",
  stackedbar = "col",
  scatter = "point",
  scatterplot = "point",
  hist = "histogram",
  histo = "histogram",
  box = "boxplot",
  boxplots = "boxplot",
  kde = "density",
  heatmap = "tile",
  heat = "tile",
  hexbin = "hex",
  jittered = "jitter",
  ecdfplot = "ecdf",
  qqplot = "qq",
  qqline = "qq",
  errbar = "errorbar",
  steps = "step"
)

default_plot_spec <- function() {
  list(
    from = "aggregate",
    geom = "col",
    x = "region",
    y = "revenue",
    color = "",
    facet = "",
    facet_col = "",
    facet_scales = "fixed",
    sort = "auto",
    position = "stack",
    title = "Revenue by region",
    subtitle = "",
    caption = "",
    xlab = "",
    ylab = "",
    labels = FALSE,
    label_size = 3,
    annotate = "",
    annotate_x = "",
    annotate_y = "",
    annotate_color = "",
    annotate_size = 3.6,
    legend = "",
    label_format = "auto",
    x_angle = 0,
    y_scale = "linear",
    x_scale = "linear",
    date_breaks = "",
    smooth = "none",
    hline = "",
    vline = "",
    orientation = "auto",
    ymin = "",
    ymax = ""
  )
}

resolve_label_format <- function(x) {
  key <- normalize_key(x)
  if (is.null(key) || key %in% c("auto", "default", "none", "plain")) {
    return("auto")
  }
  if (key %in% c("dollar", "usd", "currency", "money")) {
    return("dollar")
  }
  if (key %in% c("comma", "number", "num", "count")) {
    return("comma")
  }
  if (key %in% c("percent", "pct", "percentage")) {
    return("percent")
  }
  "auto"
}

label_fun <- function(fmt) {
  switch(
    fmt,
    dollar = scales::label_dollar(accuracy = 1),
    comma = scales::label_comma(accuracy = 1),
    percent = scales::label_percent(accuracy = 1),
    NULL
  )
}

resolve_y_scale <- function(x) {
  key <- normalize_key(x)
  if (is.null(key) || key %in% c("linear", "identity", "auto")) {
    return("linear")
  }
  if (key %in% c("log", "log10", "logy")) {
    return("log")
  }
  if (key %in% c("sqrt", "root")) {
    return("sqrt")
  }
  "linear"
}

resolve_x_scale <- function(x) {
  resolve_y_scale(x)
}

resolve_smooth <- function(x) {
  key <- normalize_key(x)
  if (is.null(key) || key %in% c("none", "off", "no", "false")) {
    return("none")
  }
  if (key %in% c("lm", "linear", "ols", "line")) {
    return("lm")
  }
  if (key %in% c("loess", "lowess", "smooth", "yes", "true")) {
    return("loess")
  }
  "none"
}

resolve_orientation <- function(x) {
  key <- normalize_key(x)
  if (is.null(key) || key %in% c("auto", "default")) {
    return("auto")
  }
  if (key %in% c("horizontal", "flip", "h", "coordflip")) {
    return("horizontal")
  }
  if (key %in% c("vertical", "v", "upright")) {
    return("vertical")
  }
  "auto"
}

resolve_x_angle <- function(x) {
  if (is.null(empty_to_null(x)) && !is.numeric(x)) {
    return(0)
  }
  if (is.numeric(x) && length(x) == 1 && !is.na(x)) {
    n <- x
  } else {
    n <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(x))))
  }
  if (is.na(n)) {
    key <- normalize_key(x)
    if (is.null(key)) {
      return(0)
    }
    if (key %in% c("diag", "diagonal", "slant")) {
      return(45)
    }
    if (key %in% c("vertical", "vert")) {
      return(90)
    }
    return(0)
  }
  if (n >= 70) 90 else if (n >= 20) 45 else 0
}

resolve_date_breaks <- function(x) {
  raw <- empty_to_null(x)
  if (is.null(raw) || identical(tolower(raw), "auto")) {
    return(NULL)
  }
  key <- normalize_key(raw)
  map <- c(
    "1day" = "1 day",
    "day" = "1 day",
    "daily" = "1 day",
    "1week" = "1 week",
    "week" = "1 week",
    "weekly" = "1 week",
    "1month" = "1 month",
    "month" = "1 month",
    "months" = "1 month",
    "monthly" = "1 month",
    "2months" = "2 months",
    "3months" = "3 months",
    "quarter" = "3 months",
    "quarterly" = "3 months",
    "6months" = "6 months",
    "1year" = "1 year",
    "year" = "1 year",
    "years" = "1 year",
    "yearly" = "1 year",
    "annual" = "1 year",
    "annually" = "1 year"
  )
  if (key %in% names(map)) {
    return(unname(map[[key]]))
  }
  allowed <- c("1 day", "1 week", "1 month", "2 months", "3 months", "6 months", "1 year")
  hit <- allowed[tolower(allowed) == tolower(raw)]
  if (length(hit)) {
    return(hit[[1]])
  }
  NULL
}

resolve_facet_scales <- function(x) {
  key <- normalize_key(x)
  if (is.null(key) || key %in% c("fixed", "default", "auto")) {
    return("fixed")
  }
  if (key %in% c("free", "freeall", "freeboth")) {
    return("free")
  }
  if (key %in% c("freex", "xfree")) {
    return("free_x")
  }
  if (key %in% c("freey", "yfree")) {
    return("free_y")
  }
  "fixed"
}

resolve_sort <- function(x) {
  key <- normalize_key(x)
  if (is.null(key) || key %in% c("auto", "none", "default")) {
    return("auto")
  }
  map <- c(
    valueasc = "value_asc",
    valuedesc = "value_desc",
    byvalueasc = "value_asc",
    byvaluedesc = "value_desc",
    ascending = "value_asc",
    descending = "value_desc",
    asc = "value_asc",
    desc = "value_desc",
    alphaasc = "alpha_asc",
    alphadesc = "alpha_desc",
    alphabetical = "alpha_asc",
    alphabetically = "alpha_asc",
    az = "alpha_asc",
    za = "alpha_desc"
  )
  if (key %in% names(map)) {
    return(unname(map[[key]]))
  }
  "auto"
}

apply_sort <- function(xvec, yvec, key) {
  xvec <- as.character(xvec)
  if (key %in% c("value_asc", "value_desc") && !is.null(yvec) && is.numeric(yvec)) {
    agg <- stats::ave(yvec, xvec, FUN = function(v) mean(v, na.rm = TRUE))
    ord <- order(agg, decreasing = identical(key, "value_desc"))
    lv <- unique(xvec[ord])
  } else {
    lv <- sort(unique(xvec))
    if (key %in% c("alpha_desc", "value_desc")) {
      lv <- rev(lv)
    }
  }
  factor(xvec, levels = lv)
}

is_yes <- function(x) {
  key <- normalize_key(x)
  !is.null(key) && key %in% c("yes", "true", "y", "1", "labels", "on", "show")
}

normalize_key <- function(x) {
  x <- empty_to_null(x)
  if (is.null(x)) {
    return(NULL)
  }
  # Lowercase BEFORE stripping: [^a-z0-9] would otherwise delete every
  # uppercase letter, so "Line" became "ine" and matched no geom at all.
  gsub("[^a-z0-9]+", "", tolower(x))
}

match_column <- function(name, df) {
  raw <- empty_to_null(name)
  if (is.null(raw) || is.null(df)) {
    return(NULL)
  }
  nms <- names(df)
  if (raw %in% nms) {
    return(raw)
  }
  hit <- nms[tolower(nms) == tolower(raw)]
  if (length(hit) == 1) {
    return(hit)
  }
  key <- normalize_key(raw)
  nms_key <- gsub("[^a-z0-9]+", "", tolower(nms))
  hit <- nms[nms_key == key]
  if (length(hit) == 1) {
    return(hit)
  }
  NULL
}

resolve_geom <- function(geom) {
  g <- empty_to_null(geom)
  if (is.null(g) || identical(tolower(g), "auto")) {
    return(NULL)
  }
  key <- normalize_key(g)
  if (is.null(key)) {
    return(NULL)
  }
  if (key %in% allowed_geoms) {
    return(key)
  }
  # An unmatched name lookup yields NA, not a zero-length vector. Returning it
  # would defeat every is.null(geom) guard downstream and reach switch(), which
  # errors. Unknown geoms must fall back to auto.
  alias <- unname(geom_aliases[key])
  if (length(alias) == 1 && !is.na(alias)) {
    return(alias)
  }
  NULL
}

choose_plot_frame <- function(from, detail_df, agg_df, needed) {
  needed <- needed[!is.null(needed)]
  has_all <- function(df) {
    !is.null(df) && all(needed %in% names(df))
  }
  from <- empty_to_null(from) %or% "auto"
  from <- tolower(from)

  if (from %in% c("detail", "aggregate")) {
    df <- if (identical(from, "detail")) detail_df else agg_df
    if (has_all(df) || (is.null(needed) || !length(needed))) {
      return(list(df = df, from = from))
    }
  }

  if (has_all(agg_df)) {
    return(list(df = agg_df, from = "aggregate"))
  }
  if (has_all(detail_df)) {
    return(list(df = detail_df, from = "detail"))
  }
  if (!is.null(agg_df)) {
    return(list(df = agg_df, from = "aggregate"))
  }
  list(df = detail_df, from = "detail")
}

prepare_plot_df <- function(df, spec) {
  df <- coerce_result_df(df)
  geom <- spec$geom
  x <- empty_to_null(spec$x)
  y <- empty_to_null(spec$y)
  color <- empty_to_null(spec$color)

  needs_collapse <- geom %in% c("col", "area", "line") &&
    !is.null(x) && !is.null(y) &&
    y %in% names(df) && is.numeric(df[[y]]) &&
    nrow(df) > n_distinct_safe(df[[x]]) * max(1, n_distinct_safe(if (!is.null(color) && color %in% names(df)) df[[color]] else 1))

  if (!needs_collapse) {
    return(df)
  }

  groups <- unique(c(x, color, empty_to_null(spec$facet), empty_to_null(spec$facet_col)))
  groups <- groups[groups %in% names(df)]
  df |>
    dplyr::summarise(
      !!y := sum(.data[[y]], na.rm = TRUE),
      .by = dplyr::all_of(groups)
    )
}

resolve_plot_spec <- function(raw, detail_df, agg_df) {
  notes <- character()
  geom <- resolve_geom(raw$geom)

  # Resolve names against both frames so we can pick the right one.
  probe <- list(
    detail = coerce_result_df(detail_df),
    aggregate = coerce_result_df(agg_df)
  )

  pick_name <- function(name) {
    for (df in probe) {
      hit <- match_column(name, df)
      if (!is.null(hit)) {
        return(hit)
      }
    }
    empty_to_null(name)
  }

  x <- pick_name(raw$x)
  y <- pick_name(raw$y)
  color <- pick_name(raw$color)
  facet <- pick_name(raw$facet)
  facet_col <- pick_name(raw$facet_col)
  ymin <- pick_name(raw$ymin)
  ymax <- pick_name(raw$ymax)

  needed <- unique(c(x, y, color, facet, facet_col, ymin, ymax))
  needed <- needed[!is.null(needed)]
  picked <- choose_plot_frame(raw$from, probe$detail, probe$aggregate, needed)

  # If a requested name still isn't on the chosen frame, drop it.
  drop_missing <- function(nm) {
    if (is.null(nm)) {
      return(NULL)
    }
    if (!is.null(picked$df) && nm %in% names(picked$df)) {
      return(nm)
    }
    notes <<- c(notes, paste0("Dropped '", nm, "' (not in ", picked$from, " result)."))
    NULL
  }
  x <- drop_missing(x)
  y <- drop_missing(y)
  color <- drop_missing(color)
  facet <- drop_missing(facet)
  facet_col <- drop_missing(facet_col)
  ymin <- drop_missing(ymin)
  ymax <- drop_missing(ymax)

  if (is.null(geom)) {
    notes <- c(notes, "No geom given; auto-choosing a chart.")
  }

  orientation <- resolve_orientation(raw$orientation)
  xlab <- empty_to_null(raw$xlab)
  ylab <- empty_to_null(raw$ylab)
  # A clock on y with a numeric x is the "swap the axes" misfire: days-since
  # 1970 get a dollar scale. Keep the date on x and flip instead.
  y_is_clock <- !is.null(y) && !is.null(picked$df) && y %in% names(picked$df) &&
    inherits(picked$df[[y]], c("Date", "POSIXt"))
  x_is_num <- !is.null(x) && !is.null(picked$df) && x %in% names(picked$df) &&
    is.numeric(picked$df[[x]]) && !inherits(picked$df[[x]], c("Date", "POSIXt"))
  if (y_is_clock && x_is_num) {
    tmp <- x
    x <- y
    y <- tmp
    tmp_lab <- xlab
    xlab <- ylab
    ylab <- tmp_lab
    if (!identical(orientation, "vertical")) {
      orientation <- "horizontal"
    }
    notes <- c(notes, "Date was on y; remapped to x and flipped so the clock stays a clock.")
  }

  spec <- list(
    from = picked$from,
    geom = geom,
    x = x %or% "",
    y = y %or% "",
    color = color %or% "",
    facet = facet %or% "",
    facet_col = facet_col %or% "",
    facet_scales = resolve_facet_scales(raw$facet_scales),
    sort = resolve_sort(raw$sort),
    position = empty_to_null(raw$position) %or% "stack",
    title = empty_to_null(raw$title) %or% "",
    subtitle = empty_to_null(raw$subtitle) %or% "",
    caption = empty_to_null(raw$caption) %or% "",
    xlab = xlab %or% "",
    ylab = ylab %or% "",
    labels = is_yes(raw$labels) || isTRUE(raw$labels),
    annotate = empty_to_null(raw$annotate) %or% "",
    annotate_x = empty_to_null(raw$annotate_x) %or% "",
    annotate_y = empty_to_null(raw$annotate_y) %or% "",
    annotate_color = empty_to_null(raw$annotate_color) %or% "",
    annotate_size = spec_num(raw, "annotate_size", 3.6),
    legend = empty_to_null(raw$legend) %or% "",
    label_format = resolve_label_format(raw$label_format),
    x_angle = resolve_x_angle(raw$x_angle),
    y_scale = resolve_y_scale(raw$y_scale),
    x_scale = resolve_x_scale(raw$x_scale),
    date_breaks = resolve_date_breaks(raw$date_breaks) %or% "",
    smooth = resolve_smooth(raw$smooth),
    hline = empty_to_null(raw$hline) %or% "",
    vline = empty_to_null(raw$vline) %or% "",
    orientation = orientation,
    ymin = ymin %or% "",
    ymax = ymax %or% ""
  )

  list(spec = spec, df = picked$df, notes = notes, auto = is.null(geom))
}

as_interactive <- function(p) {
  tryCatch(
    {
      plotly::ggplotly(p, tooltip = c("x", "y", "fill", "colour", "text")) |>
        plotly::layout(
          paper_bgcolor = paper_fill,
          plot_bgcolor = paper_fill,
          font = list(color = "#1c1917"),
          margin = list(l = 70, r = 30, t = 70, b = 70)
        )
    },
    error = function(e) {
      plotly::plot_ly() |>
        plotly::layout(title = paste("Could not make plot interactive:", conditionMessage(e)))
    }
  )
}
