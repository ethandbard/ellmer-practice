# Shared helpers. Lives in R/ so that Shiny's automatic loading puts it in the
# same environment as the functions that use it - a definition in app.R is NOT
# visible to R/ files, because runApp() evaluates app.R in a child of the
# support environment, not the other way round.

# Coalesce absent-or-blank.
#
# Deliberately NOT named `%||%`: base R 4.4 defines that operator as null-only,
# and silently widening a base operator's meaning traps any reader who knows it.
# This one also treats "" as absent, which is what the model-facing spec fields
# need, since an omitted field arrives as an empty string rather than NULL.
#
# The length-1L guard is load-bearing: `is.character(x) && !nzchar(x)` evaluates
# `&&` against a length-n logical, which is an error in R >= 4.3.
`%or%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  if (is.character(x) && length(x) == 1L && !nzchar(x)) {
    return(y)
  }
  x
}

n_distinct_safe <- function(x) {
  length(unique(x[!is.na(x)]))
}

quote_str <- function(x) {
  paste0('"', gsub('"', '\\\\"', x), '"')
}

empty_to_null <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NULL)
  }
  x <- trimws(as.character(x)[[1]])
  if (!nzchar(x) || tolower(x) %in% c("null", "none", "na", "n/a", "undefined")) {
    return(NULL)
  }
  x
}
