# Chat-driven dashboard: the model proposes up to two SQL queries
# (detail + aggregate). R runs them and updates SQL, cards, tables, charts.

library(shiny)
library(bslib)
library(querychat)
library(ellmer)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(rlang)
library(DBI)
library(duckdb)

options(shiny.maxRequestSize = 50 * 1024^2)

# Shiny auto-sources R/, but app.R also calls those helpers at load time
# (provider setup, default SQL, the opening model fit). Explicit source()
# keeps `source("app.R")` and the tests working. utils.R first so `%or%`
# exists before the view files are evaluated.
source("R/utils.R", local = TRUE)
source("R/profile.R", local = TRUE)
source("R/sql.R", local = TRUE)
source("R/theme.R", local = TRUE)
source("R/dashboard-view.R", local = TRUE)
source("R/plot-recipe.R", local = TRUE)
source("R/model-tidymodels.R", local = TRUE)
source("R/model-view.R", local = TRUE)
source("R/model-recipe.R", local = TRUE)
source("R/stats-test.R", local = TRUE)
source("R/unsupervised.R", local = TRUE)
source("R/providers.R", local = TRUE)
source("R/tools.R", local = TRUE)

sample_path <- file.path("data", "orders.csv")
if (!file.exists(sample_path)) {
  stop("Could not find data/orders.csv. Open the ellmer-practice folder as the working directory.")
}

load_sample_orders <- function() {
  read_csv(sample_path, show_col_types = FALSE) |>
    mutate(
      order_date = as.Date(order_date),
      year = as.integer(format(order_date, "%Y")),
      month = as.Date(format(order_date, "%Y-%m-01")),
      promo = factor(promo, levels = c("Yes", "No"))
    )
}

starter <- load_sample_orders()

source_catalog <- list(
  orders = list(
    label = "Sample: coffee orders",
    load = load_sample_orders
  ),
  penguins = list(
    label = "penguins (datasets)",
    load = function() {
      df <- tryCatch(
        as.data.frame(datasets::penguins),
        error = function(...) {
          if (requireNamespace("palmerpenguins", quietly = TRUE)) {
            as.data.frame(palmerpenguins::penguins)
          } else {
            stop("penguins is not available in this R version.")
          }
        }
      )
      df[, colSums(!is.na(df)) > 0, drop = FALSE]
    }
  ),
  mpg = list(
    label = "mpg (ggplot2)",
    load = function() as.data.frame(ggplot2::mpg)
  ),
  diamonds = list(
    label = "diamonds (ggplot2)",
    load = function() as.data.frame(ggplot2::diamonds)
  ),
  mtcars = list(
    label = "mtcars (datasets)",
    load = function() {
      df <- datasets::mtcars
      df$model <- rownames(df)
      rownames(df) <- NULL
      df
    }
  ),
  iris = list(
    label = "iris (datasets)",
    load = function() as.data.frame(datasets::iris)
  ),
  economics = list(
    label = "economics (ggplot2)",
    load = function() as.data.frame(ggplot2::economics)
  ),
  txhousing = list(
    label = "txhousing (ggplot2)",
    load = function() {
      df <- as.data.frame(ggplot2::txhousing)
      df$date <- as.Date(paste(df$year, df$month, "01", sep = "-"))
      df
    }
  ),
  storms = list(
    label = "storms (dplyr)",
    load = function() {
      df <- as.data.frame(dplyr::storms)
      df$date <- as.Date(paste(df$year, df$month, df$day, sep = "-"))
      df
    }
  ),
  midwest = list(
    label = "midwest (ggplot2)",
    load = function() as.data.frame(ggplot2::midwest)
  )
)

source_choices <- stats::setNames(names(source_catalog), vapply(source_catalog, `[[`, "", "label"))

data_dir <- function() {
  normalizePath("data", winslash = "/", mustWork = TRUE)
}

list_data_files <- function() {
  sort(list.files(
    data_dir(),
    pattern = "(?i)\\.(csv|tsv|txt|parquet|pq)$",
    full.names = FALSE
  ))
}

require_any_provider_key()
startup_provider <- startup_provider_id()
startup_model <- provider_catalog[[startup_provider]]$default

# One DuckDB table for the whole process. A second browser tab overwrites
# the first tab's data — documented in the README, not a multi-user app.
dash_db <- open_dash_db()
dash_con <- dash_db$con
dbWriteTable(dash_con, "data", starter, overwrite = TRUE)
onStop(function() {
  close_dash_db(dash_db)
})

# Mirrors whatever is currently in table `data`. A session's own
# reactiveValues used to be seeded with the literal `starter`/"orders.csv"
# regardless of what an earlier session had since loaded into the
# process-wide table above -- a new tab, or a reload of the same tab, would
# then claim "orders.csv" in its UI while the real table held something
# else. current_source is the single source of truth: apply_loaded_table()
# updates it on every load, and every session reads from it at start.
current_source <- local({
  env <- new.env(parent = emptyenv())
  env$label <- "orders.csv"
  env$columns <- names(starter)
  env$n <- nrow(starter)
  env$preview <- starter
  env
})

ingest_path <- function(path, label) {
  ext <- tolower(tools::file_ext(label))
  qpath <- dbQuoteString(dash_con, normalizePath(path, winslash = "/", mustWork = TRUE))
  reader <- switch(
    ext,
    csv = ,
    txt = sprintf("read_csv_auto(%s, header = true)", qpath),
    tsv = sprintf("read_csv_auto(%s, header = true, sep = '\\t')", qpath),
    parquet = ,
    pq = sprintf("read_parquet(%s)", qpath),
    stop("Please upload a .csv, .tsv, .txt, or .parquet file.")
  )
  dbExecute(dash_con, paste("CREATE OR REPLACE TABLE data AS SELECT * FROM", reader))
  n <- dbGetQuery(dash_con, "SELECT COUNT(*) AS n FROM data")$n
  cols <- dbListFields(dash_con, "data")
  preview <- dbGetQuery(dash_con, "SELECT * FROM data LIMIT 200")
  list(n = n, columns = cols, preview = preview, label = label)
}

client <- make_chat_client(startup_provider, startup_model)
client_ml <- make_chat_client(startup_provider, startup_model)

# querychat hosts the chat UI. Dashboard SQL runs against DuckDB table `data`.
#
# data_source is the live DuckDB connection, not a data.frame snapshot. A
# data.frame source gets wrapped in a DataFrameSource that freezes the rows
# at construction time; passing dash_con builds a DBISource whose query tool
# re-reads table `data` from the connection on every call. That matters
# because a session's chat client is cloned once at session start (see
# QueryChat$server()) and keeps whatever DataSource object it captured then
# -- with a DBISource that's fine, since it's a thin proxy to the live table
# rather than a frozen copy, so every session (and every upload mid-session)
# sees current rows without needing to rebuild the client.
qc <- QueryChat$new(
  dash_con,
  table_name = "data",
  id = "explore",
  client = client,
  tools = "query",
  greeting = "greeting.md",
  data_description = "The active DuckDB table is named data. A statistical profile of the loaded columns is in the system prompt.",
  extra_instructions = build_analyst_prompt(names(starter), nrow(starter), "orders.csv", df = starter)
)

qc_ml <- QueryChat$new(
  dash_con,
  table_name = "data",
  id = "model",
  client = client_ml,
  tools = "query",
  greeting = "ml-greeting.md",
  data_description = "Same table `data`. This page fits a preview model, a statistical test, or an unsupervised view.",
  extra_instructions = build_modeler_prompt(names(starter), nrow(starter), "orders.csv", df = starter)
)

# No module-level "initial_*" query results here: those used to be computed
# once at boot from `starter` and handed to every session's reactiveValues,
# which is exactly the staleness bug this file now avoids (see
# current_source above). Each session computes its own fresh view by
# calling apply_loaded_table() against current_source at server() start.

app_theme <- bs_theme(
  version = 5,
  bg = "#f6f4ef",
  fg = "#1c1917",
  primary = "#1b3a4b",
  secondary = "#8a5a32",
  base_font = font_google("Source Sans 3"),
  heading_font = font_google("Source Serif 4"),
  code_font = font_google("IBM Plex Mono")
) |>
  bs_add_rules("
    .navbar { background: #1b3a4b !important; }
    .navbar-brand, .navbar { color: #f6f4ef !important; letter-spacing: .02em; }
    .bslib-sidebar-layout > .sidebar { background: #efeae1; border-right: 1px solid #ddd4c6; }
    .card { border: 1px solid #e2d9cc; box-shadow: none; border-radius: 10px; }
    body, p, .form-control, .selectize-input { font-weight: 500; }
    .sidebar p, .sidebar li { font-weight: 500; }
    .navbar { color: #f6f4ef !important; }
    .card-header { background: transparent; font-size: .78rem; font-weight: 700;
                   letter-spacing: .08em; text-transform: uppercase; color: #3f3a34; }
    .code-card { flex: 1 1 auto; }
    .code-card pre { font-size: .78rem; line-height: 1.5; min-height: 8rem;
                     max-height: 32vh; overflow: auto; background: #1b3a4b; color: #f4efe6;
                     border-radius: 8px; padding: .8rem 1rem; white-space: pre-wrap;
                     word-break: break-word; font-weight: 400; tab-size: 2; }
    .hero-card .card-header { font-size: .95rem; letter-spacing: .04em; color: #1b3a4b;
                              font-weight: 700; text-transform: none; }
    .output-meta { font-size: .8rem; color: #3f3a34; margin-bottom: .2rem; font-weight: 600; }
    .btn-outline-secondary { border-color: #c4b8a8; font-weight: 600; }
    .sidebar .form-group, .settings-popover-body .form-group { margin-bottom: .55rem; }
    .navbar .nav-link { color: #d9d0c4 !important; font-weight: 600; }
    .navbar .nav-link.active { color: #f6f4ef !important; }
    .hero-card .shiny-plot-output img { width: 100%; height: auto; }
    /* bslib reserves ~48px above sidebar content for the collapse toggle;
       that read as a dead gap above our chat header. The toggle itself is
       absolutely positioned (top: 8px), so it doesn't need that much
       clearance -- tighten it up. */
    .bslib-sidebar-layout .sidebar-content { padding-top: .65rem; }
    .chat-panel-header { display: flex; align-items: center; gap: .6rem;
                          padding: .6rem .8rem; margin: 0 0 .7rem; border-radius: 10px;
                          background: linear-gradient(135deg, #1b3a4b, #234a60);
                          box-shadow: 0 2px 6px rgba(27, 58, 75, .22);
                          position: sticky; top: 0; z-index: 5; }
    .chat-panel-icon { display: inline-flex; align-items: center; justify-content: center;
                        width: 1.9rem; height: 1.9rem; border-radius: 50%;
                        background: rgba(246, 244, 239, .16);
                        border: 1px solid rgba(246, 244, 239, .3);
                        font-size: 1.05rem; flex: 0 0 auto; line-height: 1; }
    .chat-panel-label { font-weight: 700; font-size: .82rem; letter-spacing: .04em;
                         color: #f6f4ef; text-transform: uppercase; }
    .chat-panel-sub { display: block; font-size: .68rem; font-weight: 500;
                       letter-spacing: .02em; color: #c7d4da; margin-top: .05rem;
                       text-transform: none; }
    .chat-panel-titles { flex: 1 1 auto; min-width: 0; }
    .chat-settings-trigger { flex: 0 0 auto; display: inline-flex; align-items: center;
                              justify-content: center; width: 1.9rem; height: 1.9rem;
                              border-radius: 50%; border: 1px solid rgba(246, 244, 239, .3);
                              background: rgba(246, 244, 239, .1); color: #f6f4ef;
                              font-size: 1rem; line-height: 1; cursor: pointer;
                              transition: background .15s ease; }
    .chat-settings-trigger:hover, .chat-settings-trigger:focus { background: rgba(246, 244, 239, .28);
                                                                   outline: none; }
    .settings-popover-body hr { margin: .6rem 0; }
    /* Bootstrap popovers default to max-width: 276px, which clips our
       300px settings content (the selects/radio group stick out past the
       right edge). This is the only popover in the app, so widen it
       directly rather than relying on :has() support. */
    .popover { max-width: 336px; }
    /* Drag-to-resize plot/code split. .resizable-panes stays a row even
       though bindFillRole()'s default fill-container direction is column;
       pane-right gets a fixed, JS-adjustable flex-basis instead of the
       flex-item default of growing to fill. */
    .resizable-panes { display: flex !important; flex-direction: row !important;
                        align-items: stretch; }
    .pane-left { flex: 1 1 0; min-width: 320px; }
    /* flex-grow/shrink pinned to 0 so bslib's fill-item default (flex: 1 1
       auto) can't make this pane stretch; flex-basis is deliberately left
       alone so the inline style (initial width, then JS drag updates) wins. */
    .pane-right { flex-grow: 0 !important; flex-shrink: 0 !important;
                   min-width: 280px; max-width: 62%; }
    .pane-resize-handle { flex: 0 0 14px; cursor: col-resize; position: relative; }
    .pane-resize-handle::after { content: \"\"; position: absolute; top: 0; bottom: 0;
                                  left: 50%; width: 3px; border-radius: 2px;
                                  background: #ddd4c6; transform: translateX(-50%);
                                  transition: background .15s ease; }
    .pane-resize-handle:hover::after, .pane-resize-handle.dragging::after { background: #1b3a4b; }
  ")

# left/right go inside a flex row with a draggable handle between them;
# bindFillRole() keeps them full-height fill participants like bslib's own
# layout_columns() would, since we're bypassing that helper to get a
# fixed + resizable right pane instead of a fixed grid fraction.
resizable_split <- function(left, right, right_width = 380) {
  htmltools::bindFillRole(
    tags$div(
      class = "resizable-panes",
      htmltools::bindFillRole(tags$div(class = "pane-left", left), item = TRUE, container = TRUE),
      tags$div(
        class = "pane-resize-handle",
        tabindex = "0",
        role = "separator",
        `aria-orientation` = "vertical",
        `aria-label` = "Resize panels"
      ),
      htmltools::bindFillRole(
        tags$div(class = "pane-right", style = paste0("flex-basis: ", right_width, "px;"), right),
        item = TRUE,
        container = TRUE
      )
    ),
    item = TRUE,
    container = TRUE
  )
}

preview_canvas <- function(title_id, plot_id, left_header, left_id, right_header, right_id) {
  left <- card(
    class = "hero-card",
    full_screen = TRUE,
    min_height = 420,
    card_header(textOutput(title_id)),
    card_body(
      fill = TRUE,
      conditionalPanel(
        "input.use_plotly",
        plotlyOutput(paste0(plot_id, "_plotly"), height = "520px")
      ),
      conditionalPanel(
        "!input.use_plotly",
        plotOutput(plot_id, height = "520px")
      )
    )
  )
  right <- layout_column_wrap(
    width = 1,
    fill = TRUE,
    card(
      class = "code-card",
      fill = TRUE,
      card_header(left_header),
      card_body(verbatimTextOutput(left_id))
    ),
    card(
      class = "code-card",
      fill = TRUE,
      card_header(right_header),
      card_body(verbatimTextOutput(right_id))
    )
  )
  resizable_split(left, right)
}

# Model page: stacked main/diagnostic (top) / estimates (bottom) plot cards
# on the left, spec + code cards on the right -- two views of the same fit
# instead of one plot the user has to keep re-asking to switch.
model_canvas <- function() {
  left <- layout_column_wrap(
    width = 1,
    fill = TRUE,
    card(
      class = "hero-card model-hero",
      full_screen = TRUE,
      min_height = 280,
      card_header(textOutput("model_title")),
      card_body(
        fill = TRUE,
        conditionalPanel(
          "input.use_plotly",
          style = "flex: 1 1 auto; min-height: 0;",
          plotlyOutput("model_plot_plotly", height = "100%")
        ),
        conditionalPanel(
          "!input.use_plotly",
          style = "flex: 1 1 auto; min-height: 0;",
          plotOutput("model_plot", height = "100%")
        )
      )
    ),
    card(
      class = "hero-card model-hero",
      full_screen = TRUE,
      min_height = 280,
      card_header("Estimates"),
      card_body(
        fill = TRUE,
        conditionalPanel(
          "input.use_plotly",
          style = "flex: 1 1 auto; min-height: 0;",
          plotlyOutput("estimates_plot_plotly", height = "100%")
        ),
        conditionalPanel(
          "!input.use_plotly",
          style = "flex: 1 1 auto; min-height: 0;",
          plotOutput("estimates_plot", height = "100%")
        )
      )
    )
  )
  right <- layout_column_wrap(
    width = 1,
    fill = TRUE,
    card(
      class = "code-card",
      fill = TRUE,
      card_header("1 · Model"),
      card_body(verbatimTextOutput("model_spec"))
    ),
    card(
      class = "code-card",
      fill = TRUE,
      card_header("2 · R"),
      card_body(verbatimTextOutput("model_code"))
    )
  )
  resizable_split(left, right)
}

# Data source / chart / model controls, shown in a popover off the chat
# header's gear button instead of eating sidebar space -- the sidebar is
# the chat's, full height.
settings_panel <- function() {
  tags$div(
    class = "settings-popover-body",
    style = "width: 300px;",
    tags$p(class = "output-meta", "Data source"),
    selectInput("dataset", NULL, choices = source_choices, selected = "orders", width = "100%"),
    selectInput(
      "local_file",
      NULL,
      choices = list_data_files(),
      selected = if ("orders.csv" %in% list_data_files()) "orders.csv" else NULL,
      width = "100%"
    ),
    fileInput(
      "upload",
      NULL,
      accept = c(".csv", ".tsv", ".txt", ".parquet", ".pq"),
      buttonLabel = "Upload",
      placeholder = "or drop into data/"
    ),
    tags$p(class = "output-meta", textOutput("data_status")),
    actionButton("reset", "Reset preview", class = "btn-outline-secondary w-100 btn-sm"),
    hr(),
    tags$p(class = "output-meta", "Chart"),
    input_switch("use_plotly", "Interactive (Plotly)", value = FALSE),
    hr(),
    tags$p(class = "output-meta", "Model"),
    radioButtons(
      "provider",
      NULL,
      choices = provider_choices(),
      selected = startup_provider,
      inline = TRUE
    ),
    selectInput(
      "model",
      NULL,
      choices = provider_catalog[[startup_provider]]$models,
      selected = startup_model,
      width = "100%"
    ),
    tags$p(class = "output-meta", textOutput("model_status"))
  )
}

ui <- page_navbar(
  id = "main_nav",
  title = "Query & model preview",
  fillable = TRUE,
  selected = "explore",
  theme = app_theme,
  underline = TRUE,
  sidebar = sidebar(
    tags$div(
      style = "flex: 1 1 auto; min-height: 180px; display: flex; flex-direction: column;",
      tags$div(
        class = "chat-panel-header",
        tags$span(class = "chat-panel-icon", "\U0001F916"),
        tags$div(
          class = "chat-panel-titles",
          conditionalPanel(
            "input.main_nav == 'explore' || input.main_nav == null",
            tags$span(
              tags$span(class = "chat-panel-label", "Explore assistant"),
              tags$span(class = "chat-panel-sub", "Ask about the loaded table")
            )
          ),
          conditionalPanel(
            "input.main_nav == 'model'",
            tags$span(
              tags$span(class = "chat-panel-label", "Model assistant"),
              tags$span(class = "chat-panel-sub", "Fit and explain models")
            )
          )
        ),
        bslib::popover(
          tags$button(
            class = "chat-settings-trigger",
            type = "button",
            title = "Data, chart, and model settings",
            "⚙"
          ),
          settings_panel(),
          title = "Settings",
          placement = "bottom"
        )
      ),
      conditionalPanel("input.main_nav == 'explore' || input.main_nav == null", qc$ui()),
      conditionalPanel("input.main_nav == 'model'", qc_ml$ui())
    ),
    width = 380,
    fillable = TRUE
  ),
  nav_panel(
    title = "Explore",
    value = "explore",
    preview_canvas("plot_title", "featured_plot", "1 · SQL", "preview_sql", "2 · ggplot2", "ggplot_code")
  ),
  nav_panel(
    title = "Model",
    value = "model",
    model_canvas()
  )
)

# Drag-to-resize the plot/code split. Delegated + idempotent (data-bound
# guard) since nav_panel content for both pages is always in the DOM, and a
# naive re-scan on every Shiny render could otherwise double-bind handles.
ui <- tagList(
  ui,
  tags$script(HTML("
    (function() {
      function bindResizer(handle) {
        if (handle.dataset.bound) return;
        handle.dataset.bound = '1';
        var pane = handle.nextElementSibling;
        var dragging = false, startX = 0, startWidth = 0;
        function onMove(e) {
          if (!dragging) return;
          var container = handle.parentElement;
          var dx = startX - e.clientX;
          var maxWidth = container.getBoundingClientRect().width * 0.62;
          var newWidth = Math.max(280, Math.min(maxWidth, startWidth + dx));
          pane.style.flexBasis = newWidth + 'px';
        }
        function onUp() {
          if (!dragging) return;
          dragging = false;
          handle.classList.remove('dragging');
          document.body.style.cursor = '';
          document.body.style.userSelect = '';
          window.dispatchEvent(new Event('resize'));
        }
        handle.addEventListener('mousedown', function(e) {
          dragging = true;
          startX = e.clientX;
          startWidth = pane.getBoundingClientRect().width;
          handle.classList.add('dragging');
          document.body.style.cursor = 'col-resize';
          document.body.style.userSelect = 'none';
          e.preventDefault();
        });
        document.addEventListener('mousemove', onMove);
        document.addEventListener('mouseup', onUp);
      }
      function scan() {
        document.querySelectorAll('.pane-resize-handle').forEach(bindResizer);
      }
      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', scan);
      } else {
        scan();
      }
    })();
  "))
)

server <- function(input, output, session) {
  qc_vals <- qc$server()
  qc_ml_vals <- qc_ml$server()

  active_llm <- reactiveValues(
    provider = startup_provider,
    model = startup_model
  )

  mod <- reactiveValues(
    result = NULL
  )

  # Placeholder shape only -- apply_loaded_table() below overwrites every
  # field immediately from current_source, which holds whatever is actually
  # in table `data` right now (not necessarily "orders.csv": an earlier
  # session may have loaded something else into this process-wide table).
  dash <- reactiveValues(
    title = default_title(current_source$label),
    detail_sql = NULL,
    aggregate_sql = NULL,
    detail_df = NULL,
    aggregate_df = NULL,
    plot = NULL,
    columns = current_source$columns,
    n = current_source$n,
    label = current_source$label,
    preview = current_source$preview,
    style = default_style()
  )

  apply_loaded_table <- function(info, reset_chat = FALSE) {
    # current_source is process-wide (matches dash_con), so every session
    # that starts after this call -- including a reload of this same tab --
    # picks up this table instead of the literal starter/"orders.csv".
    current_source$label <- info$label
    current_source$columns <- info$columns
    current_source$n <- info$n
    current_source$preview <- info$preview

    full <- NULL
    if (isTRUE(info$n <= 80000)) {
      full <- tryCatch(dbGetQuery(dash_con, "SELECT * FROM data"), error = function(...) NULL)
    }
    profile <- if (!is.null(full)) full else info$preview
    view <- default_view(profile)

    dash$columns <- info$columns
    dash$n <- info$n
    dash$label <- info$label
    dash$preview <- info$preview
    dash$title <- info$label
    dash$plot <- apply_style_to_spec(view$spec, default_style())
    dash$style <- default_style()
    dres <- run_sql(dash_con, default_detail_sql())
    ares <- run_sql(dash_con, view$sql)
    dash$detail_sql <- dres$sql
    dash$aggregate_sql <- ares$sql
    dash$detail_df <- dres$df
    dash$aggregate_df <- ares$df

    prompt <- build_analyst_prompt(info$columns, info$n, info$label, df = profile)
    ml_prompt <- build_modeler_prompt(info$columns, info$n, info$label, df = profile)
    chat <- qc_vals$client
    chat_ml <- qc_ml_vals$client
    if (reset_chat) {
      notify_try(chat$set_turns(list()), "Reset Explore chat")
      notify_try(chat_ml$set_turns(list()), "Reset Model chat")
    }
    notify_try(chat$set_system_prompt(prompt), "Update Explore prompt")
    notify_try(chat_ml$set_system_prompt(ml_prompt), "Update Model prompt")
    # Rebuild the DataSource so cached column names track the new schema.
    # Row reads are always live (dash_con), so this is cheap regardless of
    # table size -- unlike the old data.frame path, it doesn't require
    # `full` to have been fetched.
    set_querychat_data(qc, dash_con)
    set_querychat_data(qc_ml, dash_con)
    mod$result <- default_model_result(profile, dash_con, info$label)
    if (reset_chat) {
      chat_id <- paste0(qc$id, "-chat")
      ml_chat_id <- paste0(qc_ml$id, "-chat")
      notify_try(shinychat::chat_clear(chat_id, greeting = FALSE), "Clear Explore chat")
      notify_try(shinychat::chat_clear(ml_chat_id, greeting = FALSE), "Clear Model chat")
      notify_try(
        shinychat::chat_append(
          chat_id,
          dataset_greeting(info$label, info$columns, info$n)
        ),
        "Append Explore greeting"
      )
      notify_try(
        shinychat::chat_append(
          ml_chat_id,
          dataset_model_greeting(info$label, info$columns, info$n)
        ),
        "Append Model greeting"
      )
    }
  }

  # Sync this session to whatever is actually in table `data` right now.
  # current_source may not be the boot-time starter/"orders.csv" -- an
  # earlier session (or an earlier reload of this same tab) may have loaded
  # something else into the shared, process-wide table. reset_chat = FALSE:
  # this session's chat client was just cloned fresh by qc$server(), so
  # there's no stale history to clear, and the greeting shown is
  # querychat's own (built from the same current data already).
  apply_loaded_table(list(
    columns = current_source$columns,
    n = current_source$n,
    label = current_source$label,
    preview = current_source$preview
  ))

  qc_vals$client$register_tool(tool(
    fun = function(title, detail_sql = NULL, aggregate_sql = NULL, plot = NULL, style = NULL) {
      detail <- run_sql(dash_con, detail_sql)
      agg <- run_sql(dash_con, aggregate_sql)
      reused <- reuse_last_queries(detail, agg, list(
        detail_df = isolate(dash$detail_df),
        aggregate_df = isolate(dash$aggregate_df),
        detail_sql = isolate(dash$detail_sql),
        aggregate_sql = isolate(dash$aggregate_sql)
      ))
      detail <- reused$detail
      agg <- reused$agg

      if (!detail$used && !agg$used) {
        return("No chart to restyle yet. Ask a data question first, or provide SQL.")
      }
      if (!detail$ok) {
        return(paste0(
          "detail_sql failed: ", detail$error,
          " Allowed columns: ", paste(isolate(dash$columns), collapse = ", "),
          ". The table is named data. Fix the SQL and call set_dashboard again."
        ))
      }
      if (!agg$ok) {
        return(paste0(
          "aggregate_sql failed: ", agg$error,
          " Allowed columns: ", paste(isolate(dash$columns), collapse = ", "),
          ". The table is named data. Alias aggregates (e.g. SUM(col) AS col) and retry."
        ))
      }

      raw <- merge_plot_spec(isolate(dash$plot), flatten_plot_args(plot))
      resolved <- resolve_plot_spec(raw, detail$df, agg$df)
      dash$style <- merge_style(isolate(dash$style), style)
      dash_title <- empty_to_null(title) %or% isolate(dash$title) %or% default_title()
      plot_spec <- apply_style_to_spec(resolved$spec, isolate(dash$style))
      plot_spec$auto <- resolved$auto

      # Assign whole objects only. Nested dash$plot$auto <- ... reads
      # reactiveValues outside a reactive consumer and errors.
      dash$title <- dash_title
      dash$detail_sql <- detail$sql
      dash$aggregate_sql <- agg$sql
      dash$detail_df <- detail$df
      dash$aggregate_df <- agg$df
      dash$plot <- plot_spec

      format_dashboard_return(dash_title, plot_spec, detail, agg, resolved$notes)
    },
    name = "set_dashboard",
    description = dashboard_tool_description,
    arguments = dashboard_tool_arguments
  ))

  qc_vals$client$register_tool(tool(
    fun = function() {
      describe_dashboard_view(
        isolate(dash$plot),
        isolate(dash$detail_sql),
        isolate(dash$aggregate_sql),
        isolate(dash$title)
      )
    },
    name = "describe_current_chart",
    description = paste(
      "Describe the chart and SQL already shown on the Explore page, without changing anything.",
      "Call this if the user asks about the chart/table that is already on screen and you have not",
      "called set_dashboard yet this conversation (e.g. right after switching datasets, or",
      "'what is this?', 'tell me about this preview'). Do not assume nothing has been shown yet — check first.",
      "Takes no arguments."
    ),
    arguments = list()
  ))

  describe_loaded_data <- function() {
    df <- isolate(dash$preview)
    n <- isolate(dash$n)
    label <- isolate(dash$label)
    if (is.null(df) || !ncol(df)) {
      return("No table is loaded.")
    }
    format_profile_md(profile_table(df), label, n)
  }

  qc_vals$client$register_tool(tool(
    fun = describe_loaded_data,
    name = "describe_data",
    description = paste(
      "Return a compact statistical profile of the currently loaded table:",
      "each column's role (measure, count, category, clock, id, text, constant),",
      "distinct counts, missingness, and a short numeric or categorical summary.",
      "Call this when the user asks what is in the data, which columns to use,",
      "or what a column means. Takes no arguments."
    ),
    arguments = list()
  ))

  qc_ml_vals$client$register_tool(tool(
    fun = describe_loaded_data,
    name = "describe_data",
    description = paste(
      "Return a compact statistical profile of the currently loaded table:",
      "each column's role, distinct counts, missingness, and a short summary.",
      "Call this when choosing a target, features, or a clock. Takes no arguments."
    ),
    arguments = list()
  ))

  qc_ml_vals$client$register_tool(tool(
    fun = function(
      title = NULL,
      method = NULL,
      prepare_sql = NULL,
      target = NULL,
      features = NULL,
      time_col = NULL,
      horizon = NULL,
      period = NULL,
      plot = NULL,
      cv_folds = NULL,
      subgroup = NULL,
      interactions = NULL,
      auto_select = NULL,
      k = NULL,
      threshold = NULL
    ) {
      spec <- merge_model_spec(
        current_model_spec(isolate(mod$result)),
        list(
          title = title,
          method = method,
          prepare_sql = prepare_sql,
          target = target,
          features = features,
          time_col = time_col,
          horizon = horizon,
          period = period,
          plot = plot,
          cv_folds = cv_folds,
          subgroup = subgroup,
          interactions = interactions,
          auto_select = auto_select,
          k = k,
          threshold = threshold
        )
      )
      if (!style_value_present(spec$method)) {
        return("No model to update yet. Send method (and usually a target) on the first call.")
      }
      res <- run_model_request(
        dash_con,
        spec,
        isolate(dash$columns)
      )
      if (!isTRUE(res$ok)) {
        return(res$error %or% "Could not fit that model. Adjust the spec and call set_model again.")
      }
      mod$result <- res
      format_model_spec(res)
    },
    name = "set_model",
    description = model_tool_description,
    arguments = model_tool_arguments
  ))

  qc_ml_vals$client$register_tool(tool(
    fun = function() {
      format_model_spec(isolate(mod$result))
    },
    name = "describe_current_model",
    description = paste(
      "Describe the model already shown on the Model page, without fitting anything new.",
      "Call this if the user asks about the model that is already on screen and you have not",
      "called set_model yet this conversation (e.g. right after switching datasets, or",
      "'what is this?', 'tell me about this preview model'). Do not assume nothing has been fit yet — check first.",
      "Takes no arguments."
    ),
    arguments = list()
  ))

  plotted_frame <- reactive({
    from <- dash$plot$from %or% "aggregate"
    df <- if (identical(from, "detail")) dash$detail_df else dash$aggregate_df
    if (is.null(df)) {
      df <- dash$detail_df
    }
    if (is.null(df)) {
      df <- dash$aggregate_df
    }
    list(df = df, from = if (identical(from, "detail")) "detail" else "aggregate")
  })

  output$preview_sql <- renderText({
    src <- plotted_frame()
    sql <- if (identical(src$from, "detail")) dash$detail_sql else dash$aggregate_sql
    if (!nzchar(empty_to_null(sql) %or% "")) {
      sql <- dash$detail_sql %or% dash$aggregate_sql
    }
    pretty_sql(sql)
  })

  output$ggplot_code <- renderText({
    ggplot_code_chunk(
      dash$plot,
      detail_sql = dash$detail_sql,
      aggregate_sql = dash$aggregate_sql,
      detail_df = dash$detail_df,
      agg_df = dash$aggregate_df,
      interactive = isTRUE(input$use_plotly)
    )
  })

  output$plot_title <- renderText({
    dash$plot$title %or% dash$title %or% "Preview"
  })

  output$model_status <- renderText({
    llm_status_label(active_llm$provider, active_llm$model)
  })

  output$data_status <- renderText({
    paste0(dash$label, " · ", format(dash$n, big.mark = ","), " rows · table `data`")
  })

  output$model_title <- renderText({
    res <- mod$result
    res$title %or% res$fit_label %or% "Model"
  })

  output$model_spec <- renderText({
    res <- mod$result
    res$summary_text %or% "# Ask a modeling question"
  })

  output$model_code <- renderText({
    res <- mod$result
    if (is.null(res)) {
      return("# R for the fitted model will land here.")
    }
    if (isTRUE(res$ok)) {
      return(model_code_chunk(res, interactive = isTRUE(input$use_plotly)))
    }
    res$code_text %or% "# R for the fitted model will land here."
  })

  featured_gg <- function() {
    render_dashboard_plot(dash$plot, dash$detail_df, dash$aggregate_df)
  }
  estimates_gg <- function() {
    render_recipe(estimates_plot_recipe(mod$result, style = dash$style))
  }
  model_gg <- function() {
    render_recipe(diagnostic_plot_recipe(mod$result, style = dash$style))
  }

  output$featured_plot <- renderPlot({
    req(!isTRUE(input$use_plotly))
    featured_gg()
  }, res = 120, bg = "transparent")

  output$featured_plot_plotly <- renderPlotly({
    req(isTRUE(input$use_plotly))
    as_interactive(featured_gg())
  })

  output$estimates_plot <- renderPlot({
    req(!isTRUE(input$use_plotly))
    estimates_gg()
  }, res = 120, bg = "transparent")

  output$estimates_plot_plotly <- renderPlotly({
    req(isTRUE(input$use_plotly))
    as_interactive(estimates_gg())
  })

  output$model_plot <- renderPlot({
    req(!isTRUE(input$use_plotly))
    model_gg()
  }, res = 120, bg = "transparent")

  output$model_plot_plotly <- renderPlotly({
    req(isTRUE(input$use_plotly))
    as_interactive(model_gg())
  })

  apply_llm <- function(provider, model) {
    if (identical(provider, isolate(active_llm$provider)) &&
        identical(model, isolate(active_llm$model))) {
      return(invisible(FALSE))
    }
    if (!has_provider_key(provider)) {
      showNotification(key_setup_note(provider), type = "error", duration = 8)
      updateRadioButtons(session, "provider", selected = isolate(active_llm$provider))
      updateSelectInput(
        session,
        "model",
        choices = provider_catalog[[isolate(active_llm$provider)]]$models,
        selected = isolate(active_llm$model)
      )
      return(invisible(FALSE))
    }
    new_chat <- tryCatch(
      make_chat_client(provider, model),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 8)
        NULL
      }
    )
    if (is.null(new_chat)) {
      return(invisible(FALSE))
    }
    swapped <- isTRUE(attach_chat_provider(qc_vals$client, new_chat)) &&
      isTRUE(attach_chat_provider(qc_ml_vals$client, new_chat))
    if (!swapped) {
      showNotification(
        "Could not swap the chat provider in place. Restart the app if the next reply still uses the old model.",
        type = "warning",
        duration = 8
      )
    }
    prompt <- build_analyst_prompt(
      isolate(dash$columns), isolate(dash$n), isolate(dash$label),
      df = isolate(dash$preview)
    )
    ml_prompt <- build_modeler_prompt(
      isolate(dash$columns), isolate(dash$n), isolate(dash$label),
      df = isolate(dash$preview)
    )
    notify_try(qc_vals$client$set_system_prompt(prompt), "Update Explore prompt")
    notify_try(qc_ml_vals$client$set_system_prompt(ml_prompt), "Update Model prompt")
    active_llm$provider <- provider
    active_llm$model <- model
    chat_id <- paste0(qc$id, "-chat")
    ml_chat_id <- paste0(qc_ml$id, "-chat")
    note <- paste0("Now using **", llm_status_label(provider, model), "**. Ask about the current table.")
    notify_try(shinychat::chat_clear(chat_id, greeting = FALSE), "Clear Explore chat")
    notify_try(shinychat::chat_clear(ml_chat_id, greeting = FALSE), "Clear Model chat")
    notify_try(shinychat::chat_append(chat_id, note), "Append Explore note")
    notify_try(shinychat::chat_append(ml_chat_id, note), "Append Model note")
    showNotification(paste("Using", llm_status_label(provider, model)), type = "message")
    invisible(TRUE)
  }

  observeEvent(input$provider, {
    spec <- provider_catalog[[input$provider]]
    req(!is.null(spec))
    selected <- if (input$provider == isolate(active_llm$provider)) {
      isolate(active_llm$model)
    } else {
      spec$default
    }
    if (!selected %in% spec$models) {
      selected <- spec$default
    }
    updateSelectInput(session, "model", choices = spec$models, selected = selected)
  }, ignoreInit = TRUE)

  observeEvent(input$model, {
    req(input$provider, input$model)
    apply_llm(input$provider, input$model)
  }, ignoreInit = TRUE)

  observeEvent(input$reset, {
    info <- list(
      columns = isolate(dash$columns),
      n = isolate(dash$n),
      label = isolate(dash$label),
      preview = isolate(dash$preview)
    )
    if (identical(input$main_nav, "model")) {
      profile <- isolate(dash$preview)
      if (isTRUE(isolate(dash$n) <= 80000)) {
        full <- tryCatch(dbGetQuery(dash_con, "SELECT * FROM data"), error = function(...) NULL)
        if (!is.null(full)) {
          profile <- full
        }
      }
      mod$result <- default_model_result(profile, dash_con, isolate(dash$label))
    } else {
      apply_loaded_table(info)
    }
  })

  load_named_source <- function(id) {
    src <- source_catalog[[id]]
    req(!is.null(src))
    df <- src$load()
    dbWriteTable(dash_con, "data", df, overwrite = TRUE)
    apply_loaded_table(
      list(
        columns = names(df),
        n = nrow(df),
        label = src$label,
        preview = utils::head(df, 200)
      ),
      reset_chat = TRUE
    )
  }

  observeEvent(input$dataset, {
    load_named_source(input$dataset)
  }, ignoreInit = TRUE)

  load_file_path <- function(path, label) {
    info <- tryCatch(
      ingest_path(path, label),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        NULL
      }
    )
    req(info)
    apply_loaded_table(info, reset_chat = TRUE)
    showNotification(paste("Loaded", info$label), type = "message")
    invisible(info)
  }

  select_local_file <- function(dest_name, path) {
    current <- isolate(input$local_file)
    updateSelectInput(session, "local_file", choices = list_data_files(), selected = dest_name)
    # updateSelectInput only reloads when the selection changes
    if (identical(current, dest_name)) {
      load_file_path(path, dest_name)
    }
  }

  import_into_data <- function(path, name) {
    dest_name <- basename(name)
    dest <- file.path(data_dir(), dest_name)
    if (!file.copy(path, dest, overwrite = TRUE)) {
      showNotification(paste("Could not copy into data/:", dest_name), type = "error")
      return(invisible(NULL))
    }
    select_local_file(dest_name, dest)
  }

  local_files <- reactivePoll(
    2000,
    session,
    checkFunc = function() {
      files <- list_data_files()
      paste(files, file.mtime(file.path(data_dir(), files)), collapse = "|")
    },
    valueFunc = list_data_files
  )

  observe({
    files <- local_files()
    selected <- isolate(input$local_file)
    if (!is.null(selected) && selected %in% files) {
      updateSelectInput(session, "local_file", choices = files, selected = selected)
    } else {
      updateSelectInput(session, "local_file", choices = files)
    }
  })

  observeEvent(input$local_file, {
    req(input$local_file)
    path <- file.path(data_dir(), input$local_file)
    req(file.exists(path))
    load_file_path(path, input$local_file)
  }, ignoreInit = TRUE)

  observeEvent(input$upload, {
    req(input$upload)
    import_into_data(input$upload$datapath, input$upload$name)
  })
}

shinyApp(ui, server)
