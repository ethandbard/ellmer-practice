# DuckDB helpers. LLM-issued SQL is gated here: comments and string literals
# are stripped before the write-keyword check, so `WHERE note = 'create'` is
# a legal read. A second read-only connection is not used — on Windows, DuckDB
# cannot ATTACH the same file while a writer is open, and an in-process
# `read_only = TRUE` handle still accepted DROP in testing.

sql_ident <- function(x) {
  paste0('"', gsub('"', '""', as.character(x), fixed = TRUE), '"')
}

strip_sql_noise <- function(sql) {
  stripped <- gsub("--[^\\n]*", " ", sql)
  stripped <- gsub("/\\*.*?\\*/", " ", stripped)
  # Single- and double-quoted literals, including escaped quotes.
  stripped <- gsub("'([^']|'')*'", "''", stripped)
  stripped <- gsub('"([^"]|"")*"', '""', stripped)
  trimws(stripped)
}

is_read_only_sql <- function(sql) {
  sql <- empty_to_null(sql)
  if (is.null(sql)) {
    return(TRUE)
  }
  stripped <- strip_sql_noise(sql)
  if (!grepl("^(with|select)\\b", stripped, ignore.case = TRUE)) {
    return(FALSE)
  }
  # A second statement after a SELECT (e.g. `SELECT 1; DROP TABLE data`).
  rest <- sub("^(with|select)\\b", "", stripped, ignore.case = TRUE)
  if (grepl(";", rest)) {
    return(FALSE)
  }
  if (grepl(
    "\\b(insert|update|delete|drop|alter|create|attach|copy|pragma|grant|truncate)\\b",
    stripped,
    ignore.case = TRUE
  )) {
    return(FALSE)
  }
  TRUE
}

run_sql <- function(con, sql) {
  sql <- empty_to_null(sql)
  if (is.null(sql)) {
    return(list(ok = TRUE, used = FALSE, sql = "", df = NULL, error = NULL))
  }
  if (!is_read_only_sql(sql)) {
    return(list(
      ok = FALSE,
      used = TRUE,
      sql = sql,
      df = NULL,
      error = "Only a SELECT or WITH ... SELECT is allowed."
    ))
  }
  tryCatch(
    {
      df <- DBI::dbGetQuery(con, sql)
      list(ok = TRUE, used = TRUE, sql = sql, df = as.data.frame(df), error = NULL)
    },
    error = function(e) {
      list(
        ok = FALSE,
        used = TRUE,
        sql = sql,
        df = NULL,
        error = conditionMessage(e)
      )
    }
  )
}

format_sql <- function(sql) {
  out <- trimws(gsub("\\s+", " ", sql))
  clauses <- c(
    "SELECT DISTINCT", "SELECT", "FROM", "WHERE", "GROUP BY", "ORDER BY",
    "HAVING", "LIMIT", "LEFT JOIN", "RIGHT JOIN", "INNER JOIN", "FULL JOIN",
    "JOIN", "UNION ALL", "UNION", "WITH"
  )
  for (kw in clauses) {
    out <- gsub(
      paste0("\\s+", kw, "\\b"),
      paste0("\n", kw),
      out,
      ignore.case = TRUE
    )
  }
  out <- gsub("\\s+AND\\b", "\n  AND", out, ignore.case = TRUE)
  out <- gsub("\\s+OR\\b", "\n  OR", out, ignore.case = TRUE)

  parts <- strsplit(out, "\n", fixed = TRUE)[[1]]
  parts <- vapply(parts, function(line) {
    if (!grepl("^SELECT\\b", line, ignore.case = TRUE)) {
      return(line)
    }
    rest <- sub("^SELECT\\s+", "", line, ignore.case = TRUE)
    cols <- trimws(strsplit(rest, ",", fixed = TRUE)[[1]])
    if (length(cols) <= 1) {
      return(paste("SELECT", rest))
    }
    paste0("SELECT\n  ", paste(cols, collapse = ",\n  "))
  }, character(1))
  out <- paste(parts, collapse = "\n")
  if (!grepl(";\\s*$", out)) {
    out <- paste0(out, ";")
  }
  out
}

coerce_result_df <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(df)
  }
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "POSIXt")) {
      df[[nm]] <- as.Date(x)
      next
    }
    if (is.character(x) || is.factor(x)) {
      xs <- as.character(x)
      sample_vals <- xs[!is.na(xs)]
      if (!length(sample_vals)) {
        next
      }
      probe <- utils::head(sample_vals, 8)
      if (all(grepl("^\\d{4}-\\d{2}-\\d{2}", probe))) {
        parsed <- as.Date(xs)
        if (mean(!is.na(parsed) | is.na(xs)) > 0.8) {
          df[[nm]] <- parsed
        }
      }
    }
  }
  df
}

# File-backed DuckDB so ingest and query share one database. Single connection:
# see the file header for why a second read-only handle is not used here.
open_dash_db <- function() {
  path <- tempfile(pattern = "ellmer-dash-", fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  list(path = path, con = con)
}

close_dash_db <- function(db) {
  if (!is.null(db$con)) {
    try(DBI::dbDisconnect(db$con, shutdown = TRUE), silent = TRUE)
  }
  if (!is.null(db$path) && file.exists(db$path)) {
    try(unlink(db$path), silent = TRUE)
  }
  invisible(NULL)
}
