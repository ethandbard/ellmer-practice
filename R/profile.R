# Column profiler. Roles come from statistics (cardinality, types, missingness);
# name tokens are only a small tiebreak. The same profile feeds the system
# prompt, auto_view(), auto_features(), and the describe_data tool.

profile_roles <- c("measure", "count", "category", "clock", "id", "text", "constant")

is_integerish_vec <- function(x) {
  x <- x[is.finite(as.numeric(x))]
  if (!length(x)) {
    return(FALSE)
  }
  max(abs(x - round(x))) < 1e-8
}

skew_of <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 3) {
    return(NA_real_)
  }
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) {
    return(0)
  }
  mean((x - mean(x))^3) / (s^3)
}

parse_clock <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (inherits(x, "Date")) {
    return(x)
  }
  if (!(is.character(x) || is.factor(x))) {
    return(NULL)
  }
  xs <- as.character(x)
  sample_vals <- xs[!is.na(xs) & nzchar(xs)]
  if (!length(sample_vals)) {
    return(NULL)
  }
  probe <- utils::head(sample_vals, 12)
  if (!all(grepl("^\\d{4}-\\d{2}-\\d{2}", probe))) {
    return(NULL)
  }
  d <- as.Date(xs)
  if (all(is.na(d) == is.na(xs) | !nzchar(xs))) {
    return(d)
  }
  NULL
}

clock_granularity <- function(dates) {
  dates <- sort(unique(dates[!is.na(dates)]))
  if (length(dates) < 2) {
    return("unknown")
  }
  med <- stats::median(as.numeric(diff(dates)))
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

sql_type_for <- function(x) {
  if (inherits(x, "POSIXt")) {
    return("TIMESTAMP")
  }
  if (inherits(x, "Date")) {
    return("DATE")
  }
  if (is.logical(x)) {
    return("BOOLEAN")
  }
  if (is.integer(x)) {
    return("INTEGER")
  }
  if (is.numeric(x)) {
    return("DOUBLE")
  }
  if (is.factor(x) || is.character(x)) {
    return("VARCHAR")
  }
  toupper(class(x)[[1]])
}

profile_one <- function(name, x) {
  n <- length(x)
  n_miss <- sum(is.na(x))
  n_hat <- n_distinct_safe(x)
  clock <- parse_clock(x)
  numeric_x <- if (is.numeric(x) && !inherits(x, c("Date", "POSIXt"))) as.numeric(x) else NULL
  integerish <- is.numeric(x) && !inherits(x, c("Date", "POSIXt")) && is_integerish_vec(x)
  all_nonneg <- is.numeric(x) && !inherits(x, c("Date", "POSIXt")) &&
    length(numeric_x[is.finite(numeric_x)]) > 0 &&
    min(numeric_x, na.rm = TRUE) >= 0

  qs <- if (!is.null(numeric_x) && any(is.finite(numeric_x))) {
    stats::quantile(numeric_x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE, names = FALSE)
  } else {
    rep(NA_real_, 5)
  }

  top <- NULL
  if (is.character(x) || is.factor(x) || is.logical(x) ||
      (integerish && n_hat <= 20)) {
    tab <- sort(table(as.character(x), useNA = "no"), decreasing = TRUE)
    if (length(tab)) {
      keep <- utils::head(tab, 6)
      top <- paste0(names(keep), " (", as.integer(keep), ")", collapse = ", ")
    }
  }

  dates <- if (!is.null(clock)) clock else if (inherits(x, c("Date", "POSIXt"))) as.Date(x) else NULL
  span <- if (!is.null(dates)) {
    d <- dates[!is.na(dates)]
    if (length(d) >= 2) as.numeric(difftime(max(d), min(d), units = "days")) else 0
  } else {
    NA_real_
  }

  row <- list(
    name = name,
    r_class = paste(class(x), collapse = "/"),
    sql_type = sql_type_for(x),
    n = n,
    n_distinct = n_hat,
    pct_missing = if (n) n_miss / n else 0,
    is_integerish = integerish,
    all_nonneg = isTRUE(all_nonneg),
    is_binary = n_hat == 2,
    min = qs[[1]],
    q25 = qs[[2]],
    median = qs[[3]],
    mean = if (!is.null(numeric_x)) mean(numeric_x, na.rm = TRUE) else NA_real_,
    q75 = qs[[4]],
    max = qs[[5]],
    sd = if (!is.null(numeric_x) && sum(is.finite(numeric_x)) > 1) {
      stats::sd(numeric_x, na.rm = TRUE)
    } else {
      NA_real_
    },
    skew = if (!is.null(numeric_x)) skew_of(numeric_x) else NA_real_,
    top_levels = top %or% "",
    date_min = if (!is.null(dates) && any(!is.na(dates))) as.character(min(dates, na.rm = TRUE)) else "",
    date_max = if (!is.null(dates) && any(!is.na(dates))) as.character(max(dates, na.rm = TRUE)) else "",
    span_days = span,
    granularity = if (!is.null(dates)) clock_granularity(dates) else "",
    parsed_clock = !is.null(clock) || inherits(x, c("Date", "POSIXt"))
  )
  row$role <- assign_role(row, x)
  row$measure_fn <- measure_fn_for_role(row)
  row
}

assign_role <- function(row, x = NULL) {
  d <- row$n_distinct
  n <- row$n
  ratio <- if (n) d / n else 0
  name <- tolower(row$name)
  tokens <- unlist(strsplit(name, "[^a-z0-9]+"))

  if (d <= 1) {
    return("constant")
  }

  if (isTRUE(row$parsed_clock)) {
    return("clock")
  }

  # Integer years (penguins$year, mpg$year) are not a calendar clock.
  yearish <- isTRUE(row$is_integerish) &&
    is.finite(row$min) && is.finite(row$max) &&
    row$min >= 1900 && row$max <= 2100 && d <= 50

  id_name <- any(tokens %in% c("id", "uuid", "guid", "pk", "rowid", "index")) ||
    grepl("(^id$|_id$)", name)
  if (id_name && ratio >= 0.8) {
    return("id")
  }
  if ((isTRUE(row$is_integerish) || row$r_class %in% c("character", "factor")) &&
      ratio >= 0.98 && d > 20) {
    return("id")
  }

  mean_len <- if (!is.null(x) && (is.character(x) || is.factor(x))) {
    mean(nchar(as.character(x)), na.rm = TRUE)
  } else {
    0
  }
  if ((is.character(x) || is.factor(x)) && (d > 20 && ratio > 0.3 || isTRUE(mean_len > 40))) {
    return("text")
  }

  if (is.logical(x)) {
    return("category")
  }
  if ((is.character(x) || is.factor(x)) && d >= 2 && d <= 20) {
    return("category")
  }

  # Count-named integers (items, quantity) must win over the small-integer
  # category rule. food_delivery$items is 1–8 and is a count, not a grouping.
  count_name <- any(tokens %in% c("count", "qty", "quantity", "items", "units", "n"))
  if (is.numeric(x) && !inherits(x, c("Date", "POSIXt")) && isTRUE(row$is_integerish) &&
      isTRUE(row$all_nonneg) && count_name) {
    if (is.finite(row$min) && is.finite(row$max) && row$min >= 1 && row$max <= 10 && d <= 15 &&
        any(tokens %in% c("rating", "score", "star", "stars"))) {
      return("measure")
    }
    return("count")
  }

  # Numeric encodings of categories: cyl, gear, carb, dataset-as-number.
  if (isTRUE(row$is_integerish) && !yearish && d >= 2 && d <= 12 && ratio < 0.15) {
    return("category")
  }
  if (yearish && d <= 20) {
    return("category")
  }

  if (is.numeric(x) && !inherits(x, c("Date", "POSIXt"))) {
    return("measure")
  }
  if (is.character(x) || is.factor(x)) {
    return(if (d > 20) "text" else "category")
  }
  "text"
}

measure_fn_for_role <- function(row) {
  if (!row$role %in% c("measure", "count")) {
    return("")
  }
  if (identical(row$role, "count")) {
    return("SUM")
  }
  tokens <- unlist(strsplit(tolower(row$name), "[^a-z0-9]+"))
  if (any(tokens %in% c(
    "rating", "score", "star", "stars", "price", "rate", "ratio", "pct",
    "percent", "mpg", "hwy", "cty", "age", "temp", "latency", "duration"
  ))) {
    return("AVG")
  }
  if (is.finite(row$min) && is.finite(row$max) && row$min >= 0 && row$max <= 10) {
    return("AVG")
  }
  # Additive totals (revenue, spend) are right-skewed and unbounded.
  if (isTRUE(row$all_nonneg) && is.finite(row$skew) && row$skew > 0.8 &&
      any(tokens %in% c("revenue", "total", "amount", "sales", "spend", "profit", "cost", "tip", "subtotal"))) {
    return("SUM")
  }
  if (isTRUE(row$all_nonneg) && any(tokens %in% c("revenue", "total", "amount", "sales", "spend"))) {
    return("SUM")
  }
  "AVG"
}

# Accept a data.frame, or a DBI connection plus table name.
profile_table <- function(x, table = "data") {
  df <- if (inherits(x, "data.frame")) {
    x
  } else if (inherits(x, "DBIConnection")) {
    DBI::dbGetQuery(x, paste("SELECT * FROM", table))
  } else {
    stop("profile_table() expects a data.frame or a DBI connection.")
  }
  if (is.null(df) || !ncol(df)) {
    return(empty_profile())
  }
  rows <- lapply(names(df), function(nm) {
    r <- profile_one(nm, df[[nm]])
    r$parsed_clock <- NULL
    r[vapply(r, is.null, logical(1))] <- NA
    r
  })
  # One atomic column at a time so roles/spans are never list-columns.
  nms <- names(rows[[1]])
  out <- vector("list", length(nms))
  names(out) <- nms
  for (nm in nms) {
    vals <- lapply(rows, `[[`, nm)
    out[[nm]] <- unlist(vals, recursive = FALSE, use.names = FALSE)
  }
  as.data.frame(out, stringsAsFactors = FALSE)
}

empty_profile <- function() {
  data.frame(
    name = character(),
    r_class = character(),
    sql_type = character(),
    n = integer(),
    n_distinct = integer(),
    pct_missing = numeric(),
    is_integerish = logical(),
    all_nonneg = logical(),
    is_binary = logical(),
    min = numeric(),
    q25 = numeric(),
    median = numeric(),
    mean = numeric(),
    q75 = numeric(),
    max = numeric(),
    sd = numeric(),
    skew = numeric(),
    top_levels = character(),
    date_min = character(),
    date_max = character(),
    span_days = numeric(),
    granularity = character(),
    role = character(),
    measure_fn = character(),
    stringsAsFactors = FALSE
  )
}

profile_cols <- function(prof, roles) {
  if (is.null(prof) || !nrow(prof)) {
    return(character())
  }
  prof$name[prof$role %in% roles]
}

pick_clock <- function(prof) {
  clocks <- prof[prof$role == "clock" & is.finite(prof$span_days) & prof$span_days >= 40, , drop = FALSE]
  if (!nrow(clocks)) {
    return(NULL)
  }
  monthly <- clocks[clocks$granularity == "month", , drop = FALSE]
  if (nrow(monthly)) {
    return(monthly$name[[1]])
  }
  clocks$name[[which.max(clocks$n_distinct)]]
}

pick_measure <- function(prof) {
  cand <- prof[prof$role %in% c("measure", "count"), , drop = FALSE]
  if (!nrow(cand)) {
    return(NULL)
  }
  tokens_of <- function(nm) unlist(strsplit(tolower(nm), "[^a-z0-9]+"))
  score <- vapply(seq_len(nrow(cand)), function(i) {
    row <- cand[i, ]
    tok <- tokens_of(row$name)
    s <- if (identical(row$role, "measure")) 10 else 8
    if (any(tok %in% c("revenue", "total", "amount", "sales"))) s <- s + 2
    if (any(tok %in% c("rating", "price", "mpg", "hwy"))) s <- s + 1
    # Bare `y` is only a hint when an `x` column is also present (Anscombe).
    if (identical(tolower(row$name), "y") && any(tolower(cand$name) == "x")) s <- s + 2
    if (any(tok %in% c("id", "year", "zip", "lat", "lon"))) s <- s - 6
    s
  }, numeric(1))
  cand$name[[which.max(score)]]
}

pick_category <- function(prof, exclude = character(), for_color = FALSE) {
  cap <- if (for_color) 8 else 20
  cand <- prof[
    prof$role == "category" &
      !prof$name %in% exclude &
      prof$n_distinct >= 2 &
      prof$n_distinct <= cap,
    ,
    drop = FALSE
  ]
  if (!nrow(cand)) {
    return(NULL)
  }
  tokens_of <- function(nm) unlist(strsplit(tolower(nm), "[^a-z0-9]+"))
  score <- vapply(seq_len(nrow(cand)), function(i) {
    row <- cand[i, ]
    tok <- tokens_of(row$name)
    s <- 8 - abs(row$n_distinct - 4)
    if (any(tok %in% c(
      "channel", "category", "species", "class", "cuisine", "cut",
      "region", "dataset", "segment"
    ))) {
      s <- s + 1
    }
    if (for_color && any(tok %in% c("channel", "membership", "segment", "species", "class"))) {
      s <- s + 2
    }
    if (for_color && row$n_distinct >= 2 && row$n_distinct <= 5) {
      s <- s + 1
    }
    s
  }, numeric(1))
  cand$name[[which.max(score)]]
}

format_profile_md <- function(prof, label = NULL, n = NULL) {
  if (is.null(prof) || !nrow(prof)) {
    return("No columns to profile.")
  }
  n <- n %or% prof$n[[1]]
  header <- paste0(
    if (!is.null(label)) paste0("Profile of **", label, "** · ") else "Profile · ",
    format(n, big.mark = ","), " rows, ", nrow(prof), " columns.\n",
    "Roles are inferred from the data (cardinality, types, missingness), not from column names.\n"
  )
  lines <- vapply(seq_len(nrow(prof)), function(i) {
    row <- prof[i, ]
    extra <- switch(
      row$role,
      measure = ,
      count = paste0(
        "min ", signif(row$min, 4),
        ", median ", signif(row$median, 4),
        ", max ", signif(row$max, 4),
        if (nzchar(row$measure_fn)) paste0(", default ", row$measure_fn) else ""
      ),
      category = row$top_levels,
      clock = paste0(
        row$date_min, " .. ", row$date_max,
        " (", round(row$span_days), " days, ", row$granularity, ")"
      ),
      id = paste0(row$n_distinct, " distinct"),
      text = paste0(row$n_distinct, " distinct"),
      ""
    )
    miss <- if (isTRUE(row$pct_missing > 0)) {
      paste0(round(100 * row$pct_missing), "% missing")
    } else {
      ""
    }
    bits <- c(extra, miss)
    bits <- bits[nzchar(bits)]
    paste0(
      "| ", row$name, " | ", row$role, " | ", row$sql_type,
      " | ", row$n_distinct, " | ",
      if (length(bits)) paste(bits, collapse = "; ") else "",
      " |"
    )
  }, character(1))
  paste(
    c(
      header,
      "| column | role | type | distinct | notes |",
      "|---|---|---|---:|---|",
      lines
    ),
    collapse = "\n"
  )
}
