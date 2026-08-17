# One recipe, two consumers: the chart pane evaluates it, the code pane
# deparses it. They cannot disagree.

new_plot_recipe <- function(data = NULL, data_name = "plot_data", layers = list(),
                            header = character(), bindings = list(),
                            collapsed = FALSE, ok = TRUE, error = NULL) {
  list(
    data = data,
    data_name = data_name,
    layers = layers,
    header = header,
    bindings = bindings,
    collapsed = collapsed,
    ok = isTRUE(ok),
    error = error
  )
}

failed_recipe <- function(error) {
  new_plot_recipe(ok = FALSE, error = error)
}

render_recipe <- function(recipe) {
  if (!isTRUE(recipe$ok) || !length(recipe$layers)) {
    return(empty_plot(recipe$error %or% "No rows to plot"))
  }
  env <- rlang::new_environment(parent = rlang::current_env())
  env[[recipe$data_name]] <- recipe$data
  for (nm in names(recipe$bindings)) {
    env[[nm]] <- recipe$bindings[[nm]]
  }
  tryCatch(
    Reduce(`+`, lapply(recipe$layers, function(cl) rlang::eval_tidy(cl, env = env))),
    error = function(e) empty_plot(paste("Could not build ggplot:", conditionMessage(e)))
  )
}

deparse_layer <- function(expr) {
  paste(deparse(expr, width.cutoff = 60L), collapse = "\n    ")
}

# Print-only cleanup. The recipe still evaluates the original expressions;
# this is what the code pane shows.
pretty_r_code <- function(txt) {
  txt <- paste(txt, collapse = "\n")
  for (pkg in c(
    "ggplot2", "scales", "stats", "recipes", "parsnip", "workflows",
    "broom", "dplyr", "rlang", "plotly", "ggrepel"
  )) {
    txt <- gsub(paste0(pkg, "::"), "", txt, fixed = TRUE)
  }
  theme_fn <- c(
    paper = "theme_minimal",
    presentation = "theme_minimal",
    minimal = "theme_minimal",
    dark = "theme_minimal",
    classic = "theme_classic",
    bw = "theme_bw",
    void = "theme_void"
  )
  for (nm in names(theme_fn)) {
    txt <- gsub(
      sprintf('theme_preview\\("%s",\\s*([0-9.]+)\\)', nm),
      sprintf("%s(base_size = \\1)", theme_fn[[nm]]),
      txt
    )
  }
  txt <- gsub('\\.data\\[\\["([^"]+)"\\]\\]', ".data$\\1", txt)
  txt <- gsub(
    "\\(\\s*label_number\\(accuracy = NULL,\\s*big\\.mark = \",\"\\)\\s*\\)",
    "label_number()",
    txt
  )
  txt <- gsub("\\(\\s*label_dollar\\(accuracy = 1\\)\\s*\\)", "label_dollar()", txt)
  txt <- gsub("\\(\\s*label_comma\\(accuracy = 1\\)\\s*\\)", "label_comma()", txt)
  txt <- gsub("\\(\\s*label_percent\\(accuracy = 0\\.1\\)\\s*\\)", "label_percent()", txt)
  txt <- gsub("\n{3,}", "\n\n", txt)
  trimws(txt)
}

deparse_recipe <- function(recipe, interactive = FALSE) {
  if (!isTRUE(recipe$ok) || !length(recipe$layers)) {
    return(paste0("# ", recipe$error %or% "Could not build ggplot."))
  }
  chain <- pretty_r_code(paste(vapply(recipe$layers, deparse_layer, ""), collapse = " +\n  "))
  header <- character()
  if (isTRUE(recipe$auto) || any(grepl("Auto-chosen", recipe$header %or% "", fixed = TRUE))) {
    header <- "# auto"
  }
  if (isTRUE(recipe$collapsed)) {
    header <- c(header, "# summarised onto x / color / facet before drawing")
  }
  body <- if (isTRUE(interactive)) paste0("p <- ", chain) else chain
  paste(
    c(
      header,
      if (length(header)) "",
      body,
      if (isTRUE(interactive)) c("", "ggplotly(p)")
    ),
    collapse = "\n"
  )
}

plot_aes_expr <- function(spec) {
  geom <- spec$geom %or% "col"
  x <- empty_to_null(spec$x)
  y <- empty_to_null(spec$y)
  color <- empty_to_null(spec$color)
  ymin <- empty_to_null(spec$ymin)
  ymax <- empty_to_null(spec$ymax)
  mapping <- list()
  if (identical(geom, "qq")) {
    if (!is.null(x)) {
      mapping$sample <- rlang::sym(x)
    }
  } else if (!is.null(x)) {
    mapping$x <- rlang::sym(x)
  }
  no_y <- c("histogram", "density", "ecdf", "qq")
  if (!is.null(y) && !geom %in% no_y) {
    mapping$y <- rlang::sym(y)
  }
  if (!is.null(ymin)) {
    mapping$ymin <- rlang::sym(ymin)
  }
  if (!is.null(ymax)) {
    mapping$ymax <- rlang::sym(ymax)
  }
  if (!is.null(color)) {
    if (geom %in% c("col", "area", "histogram", "density", "violin", "tile")) {
      mapping$fill <- rlang::sym(color)
    } else {
      mapping$colour <- rlang::sym(color)
    }
  }
  if (identical(geom, "line") && is.null(color)) {
    mapping$group <- 1
  }
  rlang::expr(ggplot2::aes(!!!mapping))
}

spec_num <- function(spec, name, default) {
  val <- spec[[name]]
  if (is.null(val) || length(val) == 0 ||
      (is.character(val) && !nzchar(trimws(val))) ||
      (is.numeric(val) && is.na(val))) {
    return(default)
  }
  as.numeric(val)[[1]]
}

resolve_ref_line <- function(value, df, col) {
  raw <- empty_to_null(value)
  if (is.null(raw)) {
    return(NULL)
  }
  key <- normalize_key(raw)
  if (!is.null(col) && col %in% names(df) && is.numeric(df[[col]])) {
    if (identical(key, "mean")) {
      return(mean(df[[col]], na.rm = TRUE))
    }
    if (identical(key, "median")) {
      return(stats::median(df[[col]], na.rm = TRUE))
    }
  }
  n <- suppressWarnings(as.numeric(raw))
  if (is.finite(n)) n else NULL
}

parse_limits <- function(x) {
  if (is.numeric(x) && length(x) == 2 && all(is.finite(x))) {
    return(as.numeric(x))
  }
  raw <- empty_to_null(x)
  if (is.null(raw)) {
    return(NULL)
  }
  parts <- suppressWarnings(as.numeric(trimws(unlist(strsplit(as.character(raw), "[,; ]+")))))
  parts <- parts[is.finite(parts)]
  if (length(parts) == 2) parts else NULL
}

apply_highlight <- function(df, spec) {
  hl <- empty_to_null(spec$highlight)
  if (is.null(hl)) {
    return(list(df = df, spec = spec))
  }
  col <- empty_to_null(spec$color) %or% empty_to_null(spec$x)
  if (is.null(col) || !col %in% names(df)) {
    return(list(df = df, spec = spec))
  }
  df$.highlight <- ifelse(as.character(df[[col]]) == hl, as.character(df[[col]]), "other")
  spec$color <- ".highlight"
  spec$.highlight_value <- hl
  list(df = df, spec = spec)
}

plot_geom_layers <- function(geom, color, position, spec = list()) {
  pos <- if (position %in% c("stack", "dodge", "fill", "identity")) position else "stack"
  fill <- empty_to_null(spec$accent) %or% accent_ink
  pt <- spec_num(spec, "point_size", 2.4)
  al <- spec_num(spec, "alpha", 0.75)
  lw <- spec_num(spec, "linewidth", 1)
  bw <- spec$bar_width
  switch(
    geom,
    col = list(
      if (is.null(color)) {
        if (is.numeric(bw) && is.finite(bw)) {
          rlang::expr(ggplot2::geom_col(position = !!pos, fill = !!fill, width = !!bw))
        } else {
          rlang::expr(ggplot2::geom_col(position = !!pos, fill = !!fill))
        }
      } else {
        rlang::expr(ggplot2::geom_col(position = !!pos))
      }
    ),
    area = list(rlang::expr(ggplot2::geom_area(position = !!pos, alpha = !!al))),
    line = list(
      rlang::expr(ggplot2::geom_line(linewidth = !!lw)),
      rlang::expr(ggplot2::geom_point(size = !!pt))
    ),
    point = list(rlang::expr(ggplot2::geom_point(alpha = !!al, size = !!pt))),
    histogram = list(
      if (is.null(color)) {
        rlang::expr(ggplot2::geom_histogram(bins = 18, position = !!pos, fill = !!fill, color = "white"))
      } else {
        rlang::expr(ggplot2::geom_histogram(bins = 18, position = !!pos, color = "white"))
      }
    ),
    density = list(rlang::expr(ggplot2::geom_density(alpha = !!al))),
    boxplot = list(rlang::expr(ggplot2::geom_boxplot())),
    violin = list(
      if (is.null(color)) {
        rlang::expr(ggplot2::geom_violin(fill = !!fill, alpha = !!al, colour = NA))
      } else {
        rlang::expr(ggplot2::geom_violin(alpha = !!al, colour = NA))
      }
    ),
    jitter = list(rlang::expr(ggplot2::geom_jitter(alpha = !!al, size = !!pt, width = 0.2, height = 0))),
    tile = list(rlang::expr(ggplot2::geom_tile())),
    hex = list(
      if (requireNamespace("hexbin", quietly = TRUE)) {
        rlang::expr(ggplot2::geom_hex())
      } else {
        rlang::expr(ggplot2::geom_bin2d())
      }
    ),
    bin2d = list(rlang::expr(ggplot2::geom_bin2d())),
    ecdf = list(rlang::expr(ggplot2::stat_ecdf(linewidth = !!lw))),
    qq = list(
      rlang::expr(ggplot2::stat_qq()),
      rlang::expr(ggplot2::stat_qq_line())
    ),
    errorbar = list(rlang::expr(ggplot2::geom_errorbar(width = 0.2))),
    pointrange = list(rlang::expr(ggplot2::geom_pointrange())),
    step = list(rlang::expr(ggplot2::geom_step(linewidth = !!lw))),
    ribbon = list(rlang::expr(ggplot2::geom_ribbon(alpha = !!al, colour = NA))),
    list(rlang::expr(ggplot2::geom_col(fill = !!fill)))
  )
}

style_extra_layers <- function(spec, geom, color, df) {
  layers <- list()
  hl <- empty_to_null(spec$.highlight_value)
  if (!is.null(hl) && identical(color, ".highlight")) {
    accent <- empty_to_null(spec$accent) %or% accent_ink
    vals <- c(stats::setNames(accent, hl), other = "grey75")
    if (geom %in% c("col", "area", "histogram", "density")) {
      layers <- c(layers, list(rlang::expr(
        ggplot2::scale_fill_manual(values = !!vals, breaks = !!hl, name = NULL)
      )))
    } else {
      layers <- c(layers, list(rlang::expr(
        ggplot2::scale_color_manual(values = !!vals, breaks = !!hl, name = NULL)
      )))
    }
  } else if (!is.null(color) && color %in% names(df)) {
    pal <- empty_to_null(spec$palette) %or% "okabe_ito"
    entry <- palette_registry[[pal]]
    discrete <- !is.numeric(df[[color]]) || n_distinct_safe(df[[color]]) <= 12
    use_fill <- geom %in% c("col", "area", "histogram", "density", "violin", "tile")
    if (!is.null(entry) && discrete) {
      cols <- discrete_colors_for(entry, n_distinct_safe(df[[color]]))
      if (!is.null(cols)) {
        if (use_fill) {
          layers <- c(layers, list(rlang::expr(ggplot2::scale_fill_manual(values = !!cols))))
        } else {
          layers <- c(layers, list(rlang::expr(ggplot2::scale_colour_manual(values = !!cols))))
        }
      }
    } else if (!is.null(entry) && identical(entry$continuous, "viridis")) {
      if (use_fill) {
        layers <- c(layers, list(rlang::expr(ggplot2::scale_fill_viridis_c())))
      } else {
        layers <- c(layers, list(rlang::expr(ggplot2::scale_colour_viridis_c())))
      }
    } else if (!is.null(entry) && is.character(entry$continuous) && length(entry$continuous) >= 2) {
      cols <- entry$continuous
      if (use_fill) {
        layers <- c(layers, list(rlang::expr(ggplot2::scale_fill_gradientn(colours = !!cols))))
      } else {
        layers <- c(layers, list(rlang::expr(ggplot2::scale_colour_gradientn(colours = !!cols))))
      }
    }
  }

  legend_pos <- empty_to_null(spec$legend_position)
  if (!is.null(legend_pos)) {
    layers <- c(layers, list(rlang::expr(ggplot2::theme(legend.position = !!legend_pos))))
  }

  grid <- empty_to_null(spec$gridlines)
  if (!is.null(grid) && !identical(grid, "both")) {
    if (identical(grid, "none")) {
      layers <- c(layers, list(rlang::expr(ggplot2::theme(panel.grid = ggplot2::element_blank()))))
    } else if (identical(grid, "x")) {
      layers <- c(layers, list(rlang::expr(ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), panel.grid.minor.y = ggplot2::element_blank()))))
    } else if (identical(grid, "y")) {
      layers <- c(layers, list(rlang::expr(ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(), panel.grid.minor.x = ggplot2::element_blank()))))
    }
  }

  layers
}

label_expr_for <- function(fmt) {
  switch(
    fmt,
    dollar = rlang::expr(scales::label_dollar(accuracy = 1)),
    comma = rlang::expr(scales::label_comma(accuracy = 1)),
    percent = rlang::expr(scales::label_percent(accuracy = 0.1)),
    NULL
  )
}

# Marks on bars/points should never print raw doubles (6906.019999999999).
value_label_expr <- function(y, fmt) {
  labeller <- label_expr_for(fmt)
  if (is.null(labeller)) {
    labeller <- rlang::expr(scales::label_number(accuracy = NULL, big.mark = ","))
  }
  rlang::expr(ggplot2::aes(label = (!!labeller)(.data[[!!y]])))
}

plot_recipe <- function(spec, df, data_name = "plot_data") {
  if (is.null(df) || !nrow(df)) {
    return(failed_recipe("No rows to plot"))
  }
  if (is.null(empty_to_null(spec$geom))) {
    return(failed_recipe("No geom to plot"))
  }

  geom <- spec$geom %or% "col"
  x <- empty_to_null(spec$x)
  y <- empty_to_null(spec$y)
  color <- empty_to_null(spec$color)
  facet <- empty_to_null(spec$facet)
  facet_col <- empty_to_null(spec$facet_col)
  facet_scales <- resolve_facet_scales(spec$facet_scales)
  sort_key <- resolve_sort(spec$sort)
  position <- empty_to_null(spec$position) %or% "stack"
  title <- empty_to_null(spec$title) %or% ""
  subtitle <- empty_to_null(spec$subtitle)
  caption <- empty_to_null(spec$caption)
  xlab <- empty_to_null(spec$xlab) %or% x
  ylab <- empty_to_null(spec$ylab) %or% if (geom %in% c("histogram", "density")) "Count" else y
  show_labels <- isTRUE(spec$labels)
  label_size <- spec_num(spec, "label_size", 3)
  annotate_txt <- empty_to_null(spec$annotate)
  legend <- empty_to_null(spec$legend)
  label_fmt <- resolve_label_format(spec$label_format)
  y_scale <- resolve_y_scale(spec$y_scale)
  x_scale <- resolve_x_scale(spec$x_scale)
  x_angle <- resolve_x_angle(spec$x_angle)
  date_breaks <- resolve_date_breaks(spec$date_breaks)
  smooth <- resolve_smooth(spec$smooth)
  orientation <- resolve_orientation(spec$orientation)

  missing <- setdiff(
    c(x, y, color, facet, facet_col, empty_to_null(spec$ymin), empty_to_null(spec$ymax)),
    names(df)
  )
  missing <- missing[!is.null(missing) & nzchar(missing)]
  if (length(missing)) {
    return(failed_recipe(paste("Plot columns not in this result:", paste(missing, collapse = ", "))))
  }

  n_before <- nrow(df)
  df <- prepare_plot_df(df, spec)
  collapsed <- nrow(df) < n_before

  if (!identical(sort_key, "auto") && !is.null(x) && x %in% names(df) &&
      !is.numeric(df[[x]]) && !inherits(df[[x]], c("Date", "POSIXt"))) {
    y_for_sort <- if (!is.null(y) && y %in% names(df) && is.numeric(df[[y]])) df[[y]] else NULL
    df[[x]] <- apply_sort(df[[x]], y_for_sort, sort_key)
  }

  hl <- apply_highlight(df, spec)
  df <- hl$df
  spec <- hl$spec
  color <- empty_to_null(spec$color)

  n_x <- if (!is.null(x) && x %in% names(df)) n_distinct_safe(df[[x]]) else 0
  flip <- identical(orientation, "horizontal") ||
    (identical(orientation, "auto") && identical(geom, "col") && n_x > 6 && is.null(facet) && is.null(facet_col))

  layers <- list(
    rlang::expr(ggplot2::ggplot(!!rlang::sym(data_name), !!plot_aes_expr(spec)))
  )
  layers <- c(layers, plot_geom_layers(geom, color, position, spec))

  has_facet_row <- !is.null(facet) && facet %in% names(df)
  has_facet_col <- !is.null(facet_col) && facet_col %in% names(df)
  if (has_facet_row && has_facet_col) {
    layers <- c(layers, list(rlang::expr(
      ggplot2::facet_grid(
        rows = ggplot2::vars(!!rlang::sym(facet)),
        cols = ggplot2::vars(!!rlang::sym(facet_col)),
        scales = !!facet_scales
      )
    )))
  } else if (has_facet_col) {
    layers <- c(layers, list(rlang::expr(
      ggplot2::facet_grid(cols = ggplot2::vars(!!rlang::sym(facet_col)), scales = !!facet_scales)
    )))
  } else if (has_facet_row) {
    form <- stats::as.formula(paste("~", facet))
    layers <- c(layers, list(rlang::expr(ggplot2::facet_wrap(!!form, scales = !!facet_scales))))
  }

  lab_args <- list(title = title, x = xlab, y = ylab)
  if (!is.null(subtitle)) {
    lab_args$subtitle <- subtitle
  }
  if (!is.null(caption)) {
    lab_args$caption <- caption
  }
  if (!is.null(legend) && !is.null(color)) {
    if (geom %in% c("col", "area", "histogram", "density")) {
      lab_args$fill <- legend
    } else {
      lab_args$colour <- legend
    }
  }
  layers <- c(layers, list(rlang::expr(ggplot2::labs(!!!lab_args))))
  theme_name <- empty_to_null(spec$theme) %or% "paper"
  base_size <- spec_num(spec, "base_size", 13)
  layers <- c(layers, list(rlang::expr(theme_preview(!!theme_name, !!base_size))))
  layers <- c(layers, style_extra_layers(spec, geom, color, df))

  y_scale_args <- list()
  if (identical(y_scale, "log")) {
    y_scale_args$transform <- "log10"
  } else if (identical(y_scale, "sqrt")) {
    y_scale_args$transform <- "sqrt"
  }
  y_is_num <- !is.null(y) && y %in% names(df) &&
    is.numeric(df[[y]]) && !inherits(df[[y]], c("Date", "POSIXt"))
  y_is_date <- !is.null(y) && y %in% names(df) && inherits(df[[y]], c("Date", "POSIXt"))
  labeller <- label_expr_for(label_fmt)
  if (!is.null(labeller) && y_is_num && !geom %in% c("histogram", "density")) {
    y_scale_args$labels <- labeller
  } else if (y_is_num && grepl("revenue|price|total|amount|sales", y, ignore.case = TRUE) &&
               !geom %in% c("histogram", "density")) {
    y_scale_args$labels <- rlang::expr(scales::label_dollar(accuracy = 1))
  }
  has_value_labels <- show_labels && y_is_num && geom %in% c("col", "line", "point")
  if (has_value_labels) {
    # Default panel expansion (~5%) leaves just enough room for the tallest
    # bar/point itself, not for a text label sitting above it -- the label
    # gets clipped at the panel edge. Reserve extra top headroom scaled to
    # the label's font size so it stays clear regardless of how big the
    # user asks for it to be.
    top_mult <- 0.06 + (label_size / 100)
    y_scale_args$expand <- rlang::expr(ggplot2::expansion(mult = c(0.05, !!top_mult)))
  }
  if (y_is_date && !geom %in% c("histogram", "density", "ecdf", "qq")) {
    layers <- c(layers, list(rlang::expr(ggplot2::scale_y_date(date_labels = "%b %Y"))))
  } else if (length(y_scale_args) && y_is_num && !geom %in% c("histogram", "density", "ecdf", "qq")) {
    layers <- c(layers, list(rlang::expr(ggplot2::scale_y_continuous(!!!y_scale_args))))
  }

  if (!identical(x_scale, "linear") && !is.null(x) && x %in% names(df) && is.numeric(df[[x]])) {
    x_args <- if (identical(x_scale, "log")) list(transform = "log10") else list(transform = "sqrt")
    layers <- c(layers, list(rlang::expr(ggplot2::scale_x_continuous(!!!x_args))))
  }

  if (!is.null(x) && x %in% names(df) && inherits(df[[x]], "Date")) {
    if (!is.null(date_breaks)) {
      date_labels <- if (identical(date_breaks, "1 year")) "%Y" else "%b %Y"
      layers <- c(layers, list(rlang::expr(
        ggplot2::scale_x_date(date_breaks = !!date_breaks, date_labels = !!date_labels)
      )))
    } else {
      layers <- c(layers, list(rlang::expr(ggplot2::scale_x_date(date_labels = "%b %Y"))))
    }
  }

  if (x_angle != 0) {
    # Leave color to the active theme. Hardcoding paper ink made rotated
    # ticks disappear on the dark theme.
    layers <- c(layers, list(rlang::expr(
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          angle = !!x_angle,
          hjust = 1,
          face = "bold",
          size = 11
        )
      )
    )))
  }

  if (has_value_labels) {
    lbl <- value_label_expr(y, label_fmt)
    # After coord_flip the measure axis is horizontal, so sit the text
    # to the right of the bar (hjust) instead of above it (vjust).
    if (flip) {
      if (identical(geom, "col") && !is.null(color)) {
        layers <- c(layers, list(rlang::expr(
          ggplot2::geom_text(
            !!lbl,
            position = ggplot2::position_dodge(width = 0.9),
            hjust = -0.15,
            vjust = 0.5,
            size = !!label_size
          )
        )))
      } else {
        layers <- c(layers, list(rlang::expr(
          ggplot2::geom_text(!!lbl, hjust = -0.15, vjust = 0.5, size = !!label_size)
        )))
      }
    } else if (identical(geom, "col") && !is.null(color)) {
      layers <- c(layers, list(rlang::expr(
        ggplot2::geom_text(
          !!lbl,
          position = ggplot2::position_dodge(width = 0.9),
          vjust = -0.35,
          size = !!label_size
        )
      )))
    } else {
      layers <- c(layers, list(rlang::expr(
        ggplot2::geom_text(!!lbl, vjust = -0.35, size = !!label_size)
      )))
    }
  }

  if (!is.null(annotate_txt)) {
    ax <- empty_to_null(spec$annotate_x)
    ay <- empty_to_null(spec$annotate_y)
    annotate_color <- empty_to_null(spec$annotate_color) %or% "#1b3a4b"
    annotate_size <- spec_num(spec, "annotate_size", 3.6)
    has_explicit_pos <- !is.null(ax) || !is.null(ay)

    if (has_explicit_pos) {
      # A position was named (fully or partially) -- anchor to real data
      # coordinates so the label sits at that point, not floating in space.
      if (is.null(ax) && !is.null(x) && x %in% names(df)) {
        ax <- df[[x]][[nrow(df)]]
      }
      if (is.null(ay) && !is.null(y) && y %in% names(df) && is.numeric(df[[y]])) {
        ay <- max(df[[y]], na.rm = TRUE)
      }
      if (!is.null(x) && x %in% names(df) && inherits(df[[x]], "Date")) {
        ax <- tryCatch(as.Date(ax), error = function(...) ax)
      }
      ay_num <- suppressWarnings(as.numeric(ay))
      if (!is.null(ax) && !is.na(ay_num)) {
        layers <- c(layers, list(rlang::expr(
          ggplot2::annotate(
            "label",
            x = !!ax,
            y = !!ay_num,
            label = !!annotate_txt,
            hjust = 1.05,
            vjust = -0.3,
            size = !!annotate_size,
            colour = !!annotate_color,
            fill = "white",
            label.size = 0.25,
            fontface = "bold"
          )
        )))
      }
    } else {
      # No position named: pin a tidy callout to the panel's top-right corner
      # with Inf coordinates so it never overlaps the data or clips at the
      # edge, regardless of the data's own range.
      layers <- c(layers, list(rlang::expr(
        ggplot2::annotate(
          "label",
          x = Inf,
          y = Inf,
          label = !!annotate_txt,
          hjust = 1.05,
          vjust = 1.4,
          size = !!annotate_size,
          colour = !!annotate_color,
          fill = "white",
          label.size = 0.25,
          fontface = "bold"
        )
      )))
    }
  }

  if (!identical(smooth, "none") && geom %in% c("point", "jitter")) {
    layers <- c(layers, list(rlang::expr(
      ggplot2::geom_smooth(method = !!smooth, se = TRUE, linewidth = 0.8, alpha = 0.2)
    )))
  }

  href <- resolve_ref_line(spec$hline, df, y)
  if (!is.null(href)) {
    layers <- c(layers, list(rlang::expr(
      ggplot2::geom_hline(yintercept = !!href, linetype = "dashed", color = "#8a5a32")
    )))
  }
  vref <- resolve_ref_line(spec$vline, df, x)
  if (!is.null(vref)) {
    layers <- c(layers, list(rlang::expr(
      ggplot2::geom_vline(xintercept = !!vref, linetype = "dashed", color = "#8a5a32")
    )))
  }

  if (isTRUE(spec$direct_labels) && !is.null(color) && geom %in% c("line", "point") && requireNamespace("ggrepel", quietly = TRUE)) {
    layers <- c(layers, list(rlang::expr(
      ggrepel::geom_text_repel(ggplot2::aes(label = .data[[!!color]]), size = 3)
    )))
  }

  xlim <- parse_limits(spec$xlim)
  ylim <- parse_limits(spec$ylim)
  # One coord only. coord_flip() after coord_cartesian() drops the zoom.
  if (flip) {
    layers <- c(layers, list(rlang::expr(ggplot2::coord_flip(xlim = !!xlim, ylim = !!ylim))))
  } else if (!is.null(xlim) || !is.null(ylim)) {
    layers <- c(layers, list(rlang::expr(ggplot2::coord_cartesian(xlim = !!xlim, ylim = !!ylim))))
  }

  new_plot_recipe(
    data = df,
    data_name = data_name,
    layers = layers,
    collapsed = collapsed,
    ok = TRUE
  )
}

auto_spec_from_df <- function(df, role = "result") {
  if (is.null(df) || !nrow(df)) {
    return(NULL)
  }
  df <- coerce_result_df(df)
  y <- first_numeric(df)
  x_date <- first_date(df)
  x_cat <- first_cat(df)
  title <- tools::toTitleCase(role)

  if (identical(role, "detail") && nrow(df) > 24 && !is.null(y)) {
    return(modifyList(default_plot_spec(), list(
      from = "detail",
      geom = "histogram",
      x = y,
      y = "",
      title = paste(title, "distribution"),
      xlab = y,
      ylab = "Count"
    )))
  }
  if (!is.null(x_date) && !is.null(y) && n_distinct_safe(df[[x_date]]) >= 2) {
    return(modifyList(default_plot_spec(), list(
      from = role,
      geom = "line",
      x = x_date,
      y = y,
      title = title,
      xlab = x_date,
      ylab = y
    )))
  }
  if (!is.null(x_cat) && !is.null(y)) {
    return(modifyList(default_plot_spec(), list(
      from = role,
      geom = "col",
      x = x_cat,
      y = y,
      title = title,
      xlab = "",
      ylab = y
    )))
  }
  if (!is.null(y) && nrow(df) >= 5) {
    return(modifyList(default_plot_spec(), list(
      from = role,
      geom = "histogram",
      x = y,
      y = "",
      title = title,
      xlab = y,
      ylab = "Count"
    )))
  }
  NULL
}

dashboard_plot_recipe <- function(spec, detail_df, agg_df,
                                  detail_sql = "", aggregate_sql = "",
                                  fallback_role = "aggregate") {
  if (isTRUE(spec$auto) || is.null(empty_to_null(spec$geom))) {
    df <- if (identical(spec$from, "detail")) detail_df else if (!is.null(agg_df)) agg_df else detail_df
    role <- spec$from %or% fallback_role
    auto_spec <- auto_spec_from_df(df, role)
    if (is.null(auto_spec) || is.null(df)) {
      return(failed_recipe(paste("No", role, "query for this question")))
    }
    rec <- plot_recipe(auto_spec, df, data_name = role)
    rec$auto <- TRUE
    rec$header <- "# auto"
    return(rec)
  }

  from <- spec$from %or% "aggregate"
  df <- if (identical(from, "detail")) detail_df else agg_df
  if (is.null(df)) {
    df <- detail_df
    from <- "detail"
  }
  data_name <- if (identical(from, "detail")) "detail" else "aggregate"
  rec <- plot_recipe(spec, df, data_name = data_name)
  rec$header <- character()
  rec
}

build_ggplot <- function(df, spec) {
  render_recipe(plot_recipe(spec, df))
}

plot_result_df <- function(df, role = "result") {
  spec <- auto_spec_from_df(df, role)
  if (is.null(spec)) {
    return(empty_plot(paste(tools::toTitleCase(role), "table updated")))
  }
  render_recipe(plot_recipe(spec, df, data_name = role))
}

render_dashboard_plot <- function(spec, detail_df, agg_df, fallback_role = "aggregate") {
  render_recipe(dashboard_plot_recipe(spec, detail_df, agg_df, fallback_role = fallback_role))
}

ggplot_code_chunk <- function(spec, detail_sql = "", aggregate_sql = "",
                              detail_df = NULL, agg_df = NULL,
                              interactive = FALSE) {
  deparse_recipe(
    dashboard_plot_recipe(
      spec, detail_df, agg_df,
      detail_sql = detail_sql,
      aggregate_sql = aggregate_sql
    ),
    interactive = interactive
  )
}
