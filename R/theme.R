# Theme and palette registries. Adding an entry here is enough for the tool
# schema to offer it — names(registry) become the enum values.

paper_fill <- "#f6f4ef"
paper_ink <- "#1c1917"
accent_ink <- "#2c3e50"

# Okabe–Ito, the usual colorblind-safe categorical default.
okabe_ito <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "#000000"
)

theme_registry <- list(
  paper = list(
    label = "Paper",
    description = "The current preview look: warm off-white, theme_minimal.",
    build = function(base_size = 13) {
      ggplot2::theme_minimal(base_size = base_size) +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = paper_fill, color = NA),
          panel.background = ggplot2::element_rect(fill = paper_fill, color = NA),
          legend.background = ggplot2::element_rect(fill = paper_fill, color = NA),
          legend.key = ggplot2::element_rect(fill = paper_fill, color = NA),
          axis.title = ggplot2::element_text(face = "bold", color = paper_ink, size = base_size - 1),
          axis.text = ggplot2::element_text(face = "bold", color = "#2a2622", size = base_size - 2),
          plot.title = ggplot2::element_text(face = "bold", color = paper_ink)
        )
    }
  ),
  minimal = list(
    label = "Minimal",
    description = "ggplot2::theme_minimal.",
    build = function(base_size = 13) ggplot2::theme_minimal(base_size = base_size)
  ),
  classic = list(
    label = "Classic",
    description = "ggplot2::theme_classic.",
    build = function(base_size = 13) ggplot2::theme_classic(base_size = base_size)
  ),
  bw = list(
    label = "Black and white",
    description = "ggplot2::theme_bw.",
    build = function(base_size = 13) ggplot2::theme_bw(base_size = base_size)
  ),
  void = list(
    label = "Void",
    description = "No axes or grid. Useful for tiles and trees.",
    build = function(base_size = 13) ggplot2::theme_void(base_size = base_size)
  ),
  dark = list(
    label = "Dark",
    description = "Dark background for screen-share.",
    build = function(base_size = 13) {
      ggplot2::theme_minimal(base_size = base_size) +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = "#1c1917", color = NA),
          panel.background = ggplot2::element_rect(fill = "#1c1917", color = NA),
          legend.background = ggplot2::element_rect(fill = "#1c1917", color = NA),
          text = ggplot2::element_text(color = "#f6f4ef"),
          axis.text = ggplot2::element_text(color = "#e7e2d8"),
          panel.grid = ggplot2::element_line(color = "#3f3a34")
        )
    }
  ),
  presentation = list(
    label = "Presentation",
    description = "Paper theme at a larger base size for a projector.",
    build = function(base_size = 18) theme_registry$paper$build(base_size)
  )
)

palette_registry <- list(
  okabe_ito = list(
    label = "Okabe–Ito",
    description = "Colorblind-safe categorical default.",
    discrete = okabe_ito,
    continuous = "viridis"
  ),
  viridis = list(
    label = "Viridis",
    description = "Perceptually uniform sequential (and discrete bins).",
    discrete = NULL,
    continuous = "viridis"
  ),
  sequential = list(
    label = "Sequential",
    description = "Light-to-dark teal ramp for magnitudes.",
    discrete = NULL,
    continuous = c("#f6f4ef", "#8fb8c4", "#1b3a4b")
  ),
  diverging = list(
    label = "Diverging",
    description = "Brown–teal through paper white. For residuals and correlations.",
    discrete = NULL,
    continuous = c("#8a5a32", "#f6f4ef", "#1b3a4b")
  ),
  mono = list(
    label = "Mono",
    description = "Single ink color. One-series charts.",
    discrete = accent_ink,
    continuous = c("#f6f4ef", accent_ink)
  ),
  warm = list(
    label = "Warm",
    description = "Oranges and browns.",
    discrete = c("#8a5a32", "#c47a3a", "#e09a4a", "#f0c989"),
    continuous = c("#f6f4ef", "#8a5a32")
  ),
  cool = list(
    label = "Cool",
    description = "Teals and blues.",
    discrete = c("#1b3a4b", "#2c6e8a", "#56B4E9", "#8fb8c4"),
    continuous = c("#f6f4ef", "#1b3a4b")
  )
)

# Stretch a registry palette to n discrete levels. Recycle-by-interpolation
# so a 9th category does not error on Okabe–Ito's 8 colors, and so
# sequential / diverging actually paint a bar chart.
discrete_colors_for <- function(entry, n) {
  n <- as.integer(n)
  if (is.null(entry) || !is.finite(n) || n < 1) {
    return(NULL)
  }
  ramp <- function(cols) {
    cols <- unname(cols)
    cols <- cols[!is.na(cols) & nzchar(cols)]
    if (!length(cols)) {
      return(NULL)
    }
    if (length(cols) == 1) {
      cols <- c("#f6f4ef", cols)
    }
    grDevices::colorRampPalette(cols)(n)
  }
  disc <- entry$discrete
  if (!is.null(disc) && length(disc) >= n) {
    return(unname(disc)[seq_len(n)])
  }
  if (!is.null(disc) && length(disc) >= 1) {
    return(ramp(disc))
  }
  cont <- entry$continuous
  if (is.character(cont) && length(cont) == 1 && identical(cont, "viridis")) {
    return(grDevices::hcl.colors(n, palette = "viridis"))
  }
  if (is.character(cont) && length(cont) >= 1) {
    return(ramp(cont))
  }
  NULL
}

theme_preview <- function(name = "paper", base_size = 13) {
  entry <- theme_registry[[name]]
  if (is.null(entry)) {
    entry <- theme_registry$paper
  }
  entry$build(base_size)
}

empty_plot <- function(msg) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = msg, size = 4.5) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = paper_fill, color = NA),
      panel.background = ggplot2::element_rect(fill = paper_fill, color = NA)
    )
}
